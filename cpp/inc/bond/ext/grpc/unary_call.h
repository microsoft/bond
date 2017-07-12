// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpc++/grpc++.h>
#include <grpc++/impl/codegen/async_unary_call.h>
#include <grpc++/impl/codegen/status.h>
#include <grpc++/impl/codegen/server_context.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <bond/ext/grpc/detail/io_manager_tag.h>

#include <boost/assert.hpp>
#include <boost/optional.hpp>
#include <atomic>
#include <memory>

namespace bond { namespace ext { namespace gRPC {

namespace detail {

    template <typename TRequest, typename TResponse, typename TThreadPool>
    struct service_unary_call_data;

    /// @brief Implementation class that holds the state associated with a
    /// single async, unary call.
    ///
    /// Two different object participate in shared ownership of instances of
    /// this class: the user-facing unary_call object and the
    /// response_sent_tag helper struct.
    ///
    /// To ensure that it stays alive while sending a response,
    /// unary_call_impl creates a response_sent_tag with a shared_ptr to
    /// itself that it enques in the completion queue. When that sending of
    /// the response is done and it is dequeued from the completion queue,
    /// response_sent_tag clears its shared_ptr to the unary_call_impl.
    ///
    /// The user-facing unary_call object also has a shared_ptr to an
    /// instance of unary_call_impl to keep the state alive while the call
    /// object is itself alive.
    template <typename TRequest, typename TResponse>
    struct unary_call_impl final
        : std::enable_shared_from_this<unary_call_impl<TRequest, TResponse>>,
          io_manager_tag
    {
        grpc::ServerContext _context;
        TRequest _request;
        grpc::ServerAsyncResponseWriter<bond::bonded<TResponse>> _responder;
        /// Tracks whether any response has been sent yet.
        std::atomic_flag _responseSentFlag;
        /// A pointer to ourselves used to keep us alive while the response is
        /// being sent.
        std::shared_ptr<unary_call_impl> _self;

        unary_call_impl()
            : _context(),
            _request(),
            _responder(&_context),
            _responseSentFlag(),
            _self()
        { }

        unary_call_impl(const unary_call_impl&) = delete;
        unary_call_impl& operator=(const unary_call_impl&) = delete;

        void Finish(const bond::bonded<TResponse>& msg, const grpc::Status& status)
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _self = this->shared_from_this();
                _responder.Finish(msg, status, static_cast<void*>(this));
            }
        }

        void FinishWithError(const grpc::Status& status)
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _self = this->shared_from_this();
                _responder.FinishWithError(status, static_cast<void*>(this));
            }
        }

        void invoke(bool /* ok */) override
        {
            // response has been sent, so we no longer need to keep
            // ourselves alive
            _self.reset();
        }
    };

}

/// @brief The details of a single async, unary call.
///
/// Call \p Finish or \p FinishWithError to send a response back to the
/// client.
///
/// If no explicit call to Finish/FinishWithError has been made before this
/// unary_call instance is destroyed, a generic internal server error is
/// sent.
///
/// @note This class can only be moved.
template <typename TRequest, typename TResponse>
class unary_call final
{
public:
    unary_call(unary_call&& other) noexcept
        : _impl(std::move(other._impl))
    {
        other._impl.reset();
    }

    unary_call& operator=(unary_call&& rhs)
    {
        if (this != &rhs)
        {
            reset(std::move(rhs.impl));
            rhs._impl.reset();
        }

        return *this;
    }

    // unary_call is move-only
    unary_call(const unary_call&) = delete;
    unary_call& operator=(const unary_call&) = delete;

    ~unary_call()
    {
        reset(nullptr);
    }

    void swap(unary_call& rhs) noexcept
    {
        using std::swap;
        swap(_impl, rhs._impl);
    }

    /// @brief Get the server context for this call.
    const grpc::ServerContext& context() const
    {
        BOOST_ASSERT(_impl);
        return _impl->_context;
    }

    /// @brief Get the server context for this call.
    grpc::ServerContext& context()
    {
        BOOST_ASSERT(_impl);
        return _impl->_context;
    }

    /// @brief Get the request message for this call.
    const TRequest& request() const
    {
        BOOST_ASSERT(_impl);
        return _impl->_request;
    }

    /// @brief Get the request message for this call.
    TRequest& request()
    {
        BOOST_ASSERT(_impl);
        return _impl->_request;
    }

    /// @brief Responds to the client with the given message and status.
    ///
    /// Only the first call to \p Finish or \p FinishWithError will be
    /// honored.
    void Finish(const TResponse& msg, const grpc::Status& status = grpc::Status::OK)
    {
        Finish(bond::bonded<TResponse>{msg}, status);
    }

    /// @brief Responds to the client with the given message and status.
    ///
    /// Only the first call to \p Finish or \p FinishWithError will be
    /// honored.
    void Finish(const bond::bonded<TResponse>& msg, const grpc::Status& status = grpc::Status::OK)
    {
        BOOST_ASSERT(_impl);
        _impl->Finish(msg, status);
    }

    /// @brief Responds to the client with the given status and no message.
    ///
    /// Only the first call to \p Finish or \p FinishWithError will be
    /// honored.
    void FinishWithError(const grpc::Status& status)
    {
        BOOST_ASSERT(_impl);
        _impl->FinishWithError(status);
    }

    template <typename SUCDRequest, typename SUCDResponse, typename SUCDThreadPool>
    friend struct detail::service_unary_call_data;

private:
    std::shared_ptr<detail::unary_call_impl<TRequest, TResponse>> _impl;

    explicit unary_call(std::shared_ptr<detail::unary_call_impl<TRequest, TResponse>>&& impl) noexcept
        : _impl(std::move(impl))
    {
        BOOST_ASSERT(_impl);
    }

    /// Resets the underlying detail::unary_call_impl that this is wrapping.
    /// If the previous \p _impl was non-null, attempts to send an internal
    /// server error.
    void reset(std::shared_ptr<detail::unary_call_impl<TRequest, TResponse>>&& newValue)
    {
        auto oldValue = _impl;
        _impl = newValue;

        if (oldValue != _impl)
        {
            if (oldValue)
            {
                // the current impl may not have had Finish called on it by
                // the user code, so attempt to send an error response.
                // Relies on unary_call_impl to only send one actual
                // response.
                oldValue->FinishWithError(
                    grpc::Status{
                        grpc::StatusCode::INTERNAL,
                        "An internal server error has occurred." });
            }
        }
    }
};

template <typename TRequest, typename TResponse>
void swap(unary_call<TRequest, TResponse>& lhs, unary_call<TRequest, TResponse>& rhs) noexcept
{
    lhs.swap(rhs);
}

} } } //namespace bond::ext::gRPC
