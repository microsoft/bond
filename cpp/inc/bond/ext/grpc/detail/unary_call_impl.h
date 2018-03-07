// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bonded.h>
#include <bond/ext/grpc/detail/io_manager_tag.h>

#ifdef _MSC_VER
    #pragma warning (push)
    // warning C4100: unreferenced formal parameter
    //
    // warning C4291: no matching operator delete found; memory will not be
    // freed if initialization throws an exception
    //
    // warning C4702: unreachable code
    #pragma warning (disable: 4100 4291 4702)
#endif

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/async_unary_call.h>
#include <grpcpp/impl/codegen/status.h>
#include <grpcpp/impl/codegen/server_context.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>
#include <boost/smart_ptr/intrusive_ptr.hpp>

#include <atomic>
#include <utility>

namespace bond { namespace ext { namespace gRPC { namespace detail {

    /// @brief Implementation class that holds the state associated with a
    /// single async, unary call.
    ///
    /// Potentually three different objects participate in shared ownership
    /// of instances of this class: the user-facing \ref unary_call and \ref
    /// shared_unary_call objects, as well as this object itself.
    ///
    /// To ensure that this instance stays alive while sending a response,
    /// unary_call_impl creates itself with a ref count of 1, opposed to the
    /// usual 0 that many boost::intrusive_ptr implementations use. When the
    /// notification that the response has been sent is dequeued from the
    /// completion queue, %invoke() calls %Release() on itself, decrementing
    /// the ref count, and allowing the remaining unary_call and
    /// shared_unary_call objects, if any, to control lifetime.
    template <typename TRequest, typename TResponse>
    class unary_call_impl final : io_manager_tag
    {
    public:
        unary_call_impl() = default;

        unary_call_impl(const unary_call_impl&) = delete;
        unary_call_impl& operator=(const unary_call_impl&) = delete;

        const grpc::ServerContext& context() const noexcept
        {
            return _context;
        }

        grpc::ServerContext& context() noexcept
        {
            return _context;
        }

        const TRequest& request() const noexcept
        {
            return _request;
        }

        TRequest& request() noexcept
        {
            return _request;
        }

        const grpc::ServerAsyncResponseWriter<bond::bonded<TResponse>>& responder() const noexcept
        {
            return _responder;
        }

        grpc::ServerAsyncResponseWriter<bond::bonded<TResponse>>& responder() noexcept
        {
            return _responder;
        }

        void Finish(const bond::bonded<TResponse>& msg, const grpc::Status& status)
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _responder.Finish(msg, status, static_cast<void*>(static_cast<io_manager_tag*>(this)));
            }
        }

        void FinishWithError(const grpc::Status& status)
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _responder.FinishWithError(status, static_cast<void*>(static_cast<io_manager_tag*>(this)));
            }
        }

    private:
        void invoke(bool /* ok */) override
        {
            // The response has been sent, so we no longer need to keep
            // ourselves alive: release the implicit initial refcount that
            // this instance was constructed with.
            Release();
        }

        void AddRef() noexcept
        {
            _refCount.fetch_add(1, std::memory_order_relaxed);
        }

        void Release()
        {
            if (_refCount.fetch_sub(1, std::memory_order_acq_rel) == 1)
            {
                delete this;
            }
        }

        void TryFinishWithError()
        {
            if (_refCount.load(std::memory_order::memory_order_acquire) == 2)
            {
                // The last user reference has just gone away, but Finish was
                // not called. In this case, we are responsible for sending
                // an error response and decrementing the final ref
                // count. FinishWithError will schedule the send of the
                // error response, and notification of completion of the
                // send via invoke() will decrement the final ref count.
                // Since we hold the ref count ourselves, we will not
                // get deleted until we're done sending.
                //
                // Even when multiple threads enter this case, at most one
                // will succeed in sending error, thanks to _responseSentFlag.

                FinishWithError({ grpc::StatusCode::INTERNAL, "An internal server error has occurred." });
            }
        }

        friend void intrusive_ptr_add_ref(unary_call_impl* call) noexcept
        {
            call->AddRef();
        }

        friend void intrusive_ptr_release(unary_call_impl* call)
        {
            call->TryFinishWithError();
            call->Release();
        }

        // A pointer to the context is passed to _responder when
        // constructing it, so this needs to be declared before _responder.
        grpc::ServerContext _context{};
        TRequest _request{};
        grpc::ServerAsyncResponseWriter<bond::bonded<TResponse>> _responder{ &_context };
        std::atomic_flag _responseSentFlag = ATOMIC_FLAG_INIT; // Tracks whether any response has been sent yet.
        // The ref count intentionally starts at 1, because this instance
        // needs to keep itself alive until the response has finished being
        // sent, regardless of whether there are any outstanding user
        // references still alive.
        std::atomic<size_t> _refCount{ 1 };

    };

    /// @brief Detail class that helps implement \ref unary_call and \ref
    /// shared_unary_call.
    template <typename TRequest, typename TResponse>
    class unary_call_base
    {
        using impl_type = unary_call_impl<TRequest, TResponse>;

    public:
        void swap(unary_call_base& rhs) noexcept
        {
            using std::swap;
            swap(_impl, rhs._impl);
        }

        /// @brief Get the server context for this call.
        const grpc::ServerContext& context() const noexcept
        {
            return impl().context();
        }

        /// @brief Get the server context for this call.
        grpc::ServerContext& context() noexcept
        {
            return impl().context();
        }

        /// @brief Get the request message for this call.
        const TRequest& request() const noexcept
        {
            return impl().request();
        }

        /// @brief Get the request message for this call.
        TRequest& request() noexcept
        {
            return impl().request();
        }

        /// @brief Responds to the client with the given message and status.
        ///
        /// Only the first call to \p Finish or \p FinishWithError will be
        /// honored.
        void Finish(const TResponse& msg, const grpc::Status& status = grpc::Status::OK)
        {
            Finish(bond::bonded<TResponse>{ msg }, status);
        }

        /// @brief Responds to the client with the given message and status.
        ///
        /// Only the first call to \p Finish or \p FinishWithError will be
        /// honored.
        void Finish(const bond::bonded<TResponse>& msg, const grpc::Status& status = grpc::Status::OK)
        {
            impl().Finish(msg, status);
        }

        /// @brief Responds to the client with the given status and no message.
        ///
        /// Only the first call to \p Finish or \p FinishWithError will be
        /// honored.
        void FinishWithError(const grpc::Status& status)
        {
            impl().FinishWithError(status);
        }

    protected:
        unary_call_base() = default;

        explicit unary_call_base(boost::intrusive_ptr<impl_type> impl) noexcept
            : _impl(std::move(impl))
        {
            BOOST_ASSERT(_impl);
        }

        explicit operator bool() const noexcept
        {
            return static_cast<bool>(_impl);
        }

    private:
        impl_type& impl() noexcept
        {
            BOOST_ASSERT(_impl);
            return *_impl;
        }

        const impl_type& impl() const noexcept
        {
            BOOST_ASSERT(_impl);
            return *_impl;
        }

        boost::intrusive_ptr<impl_type> _impl;
    };

} } } } //namespace bond::ext::gRPC::detail
