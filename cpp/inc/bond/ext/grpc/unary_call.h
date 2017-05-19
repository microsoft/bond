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
#include <atomic>

namespace bond { namespace ext { namespace gRPC {

namespace detail {

    /// @brief Implementation class that holds the state associated with a
    /// single async, unary call.
    ///
    /// It manages its own lifetime: to send a response, it enques itself in
    /// the completion queue. When that sending of the response is done and
    /// it is dequeued from the completion queue, it deletes itself.
    template <typename TRequest, typename TResponse>
    struct unary_call_impl final : detail::io_manager_tag
    {
        grpc::ServerContext _context;
        TRequest _request;
        grpc::ServerAsyncResponseWriter<TResponse> _responder;
        std::atomic_flag _responseSentFlag;

        unary_call_impl()
            : _context(),
            _request(),
            _responder(&_context),
            _responseSentFlag()
        { }

        unary_call_impl(const unary_call_impl&) = delete;
        unary_call_impl& operator=(const unary_call_impl&) = delete;

        void Finish(const TResponse& msg, const grpc::Status& status)
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _responder.Finish(msg, status, this);
            }
        }

        void FinishWithError(const grpc::Status& status)
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _responder.FinishWithError(status, this);
            }
        }

        void invoke(bool /* ok */) override
        {
            delete this;
        }
    };

}

/// @brief The details of a single async, unary call.
///
/// Call \p Finish or \p FinishWithError to send a response back to the
/// client.
///
/// If no explicit call to Finish/FinishWithError has been made before this
/// call is destroyed, a generic internal server error is sent.
///
/// \note This class can only be moved.
template <typename TRequest, typename TResponse>
class unary_call final
{
public:
    unary_call() noexcept : _impl(nullptr) {}

    explicit unary_call(detail::unary_call_impl<TRequest, TResponse>* impl) noexcept
        : _impl(impl)
    {
        BOOST_ASSERT(impl);
    }

    unary_call(unary_call&& other) noexcept
        : _impl(other._impl)
    {
        other._impl = nullptr;
    }

    unary_call& operator=(unary_call&& rhs)
    {
        if (this != &rhs)
        {
            reset(rhs.impl);
            rhs._impl = nullptr;
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
        return _impl->_context;
    }

    /// @brief Get the server context for this call.
    grpc::ServerContext& context()
    {
        return _impl->_context;
    }

    /// @brief Get the request message for this call.
    const TRequest& request() const
    {
        return _impl->_request;
    }

    /// @brief Get the request message for this call.
    TRequest& request()
    {
        return _impl->_request;
    }

    /// @brief Responds to the client with the given message and status.
    ///
    /// Only the first call to \p Finish or \p FinishWithError will be
    /// honored.
    void Finish(const TResponse& msg, const grpc::Status& status)
    {
        _impl->Finish(msg, status);
    }

    /// @brief Responds to the client with the given status and no message.
    ///
    /// Only the first call to \p Finish or \p FinishWithError will be
    /// honored.
    void FinishWithError(const grpc::Status& status)
    {
        _impl->FinishWithError(status);
    }

private:
    detail::unary_call_impl<TRequest, TResponse>* _impl;

    /// Resets the underlying detail::unary_call_impl that this is wrapping.
    /// If the previous \p _impl was non-null, attempts to send an internal
    /// server error.
    ///
    /// By always sending something back we get two benefits:
    ///    - the client will get some sort of response instead of having to
    ///      wait for a timeout
    ///    - resources will be cleaned up after the response has been sent
    void reset(detail::unary_call_impl<TRequest, TResponse>* newValue)
    {
        auto oldValue = _impl;
        _impl = newValue;

        if (oldValue != newValue)
        {
            if (oldValue)
            {
                // the current impl is being destroyed, so send an error
                // response. Relies on unary_call_impl to only send on
                // actual response.
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
