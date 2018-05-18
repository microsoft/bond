// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "io_manager_tag.h"

#include <bond/core/bonded.h>

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

namespace bond { namespace ext { namespace gRPC { namespace detail
{
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
    template <typename Request, typename Response>
    class unary_call_impl : io_manager_tag
    {
    public:
        unary_call_impl() = default;

        const grpc::ServerContext& context() const noexcept
        {
            return _context;
        }

        grpc::ServerContext& context() noexcept
        {
            return _context;
        }

        const bonded<Request>& request() const noexcept
        {
            return _request;
        }

        bonded<Request>& request() noexcept
        {
            return _request;
        }

        const grpc::ServerAsyncResponseWriter<bonded<Response>>& responder() const noexcept
        {
            return _responder;
        }

        grpc::ServerAsyncResponseWriter<bonded<Response>>& responder() noexcept
        {
            return _responder;
        }

        void Finish(const bonded<Response>& msg = bonded<Response>{ Response{} })
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _responder.Finish(msg, grpc::Status::OK, tag());
            }
        }

        void Finish(const grpc::Status& status)
        {
            bool wasResponseSent = _responseSentFlag.test_and_set();
            if (!wasResponseSent)
            {
                _responder.FinishWithError(status, tag());
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
                // count. Finish with grpc::Status will schedule the send of the
                // error response, and notification of completion of the
                // send via invoke() will decrement the final ref count.
                // Since we hold the ref count ourselves, we will not
                // get deleted until we're done sending.
                //
                // Even when multiple threads enter this case, at most one
                // will succeed in sending error, thanks to _responseSentFlag.

                Finish({ grpc::StatusCode::INTERNAL, "An internal server error has occurred." });
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
        bonded<Request> _request{};
        grpc::ServerAsyncResponseWriter<bonded<Response>> _responder{ &_context };
        std::atomic_flag _responseSentFlag = ATOMIC_FLAG_INIT; // Tracks whether any response has been sent yet.
        // The ref count intentionally starts at 1, because this instance
        // needs to keep itself alive until the response has finished being
        // sent, regardless of whether there are any outstanding user
        // references still alive.
        std::atomic<size_t> _refCount{ 1 };
    };

    template <typename T>
    struct payload
    {
        using type = T;
    };

    template <>
    struct payload<void>
    {
        using type = Void;
    };

    template <>
    struct payload<bond::reflection::nothing>
        : payload<void>
    {};


    template <typename Request, typename Response>
    class unary_call_base;

    template <typename Base>
    class unary_call_impl_base;

    template <typename Base>
    class unary_call_input_base;

    template <typename Base>
    class unary_call_result_base;


    template <template <typename> class Base, typename Request, typename Response>
    class unary_call_impl_base<Base<unary_call_base<Request, Response>>>
    {
    protected:
        unary_call_base<Request, Response>& impl() noexcept
        {
            return static_cast<unary_call_base<Request, Response>&>(
                static_cast<Base<unary_call_base<Request, Response>>&>(*this));
        }

        const unary_call_base<Request, Response>& impl() const noexcept
        {
            return static_cast<unary_call_base<Request, Response>&>(
                static_cast<Base<unary_call_base<Request, Response>>&>(*this));
        }
    };


    template <typename Request, typename Response>
    class unary_call_input_base<unary_call_base<Request, Response>>
        : public unary_call_impl_base<unary_call_input_base<unary_call_base<Request, Response>>>
    {
    public:
        /// @brief Get the request message for this call.
        const bonded<Request>& request() const noexcept
        {
            return this->impl().impl().request();
        }

        /// @brief Get the request message for this call.
        bonded<Request>& request() noexcept
        {
            return this->impl().impl().request();
        }
    };

    template <typename Response>
    class unary_call_input_base<unary_call_base<void, Response>>
    {};


    template <typename Request, typename Response>
    class unary_call_result_base<unary_call_base<Request, Response>>
        : public unary_call_impl_base<unary_call_result_base<unary_call_base<Request, Response>>>
    {
    public:
        /// @brief Responds to the client with the given message.
        ///
        /// Only the first call to \p Finish will be honored.
        void Finish(const Response& msg = {})
        {
            Finish(bonded<Response>{ msg });
        }

        /// @brief Responds to the client with the given message.
        ///
        /// Only the first call to \p Finish will be honored.
        void Finish(const bonded<Response>& msg)
        {
            this->impl().impl().Finish(msg);
        }

        /// @brief Responds to the client with the given status and no message.
        ///
        /// Only the first call to \p Finish will be honored.
        void Finish(const grpc::Status& status)
        {
            this->impl().impl().Finish(status);
        }

    protected:
        void FinishEvent()
        {}
    };

    template <typename Request>
    class unary_call_result_base<unary_call_base<Request, void>>
        : public unary_call_impl_base<unary_call_result_base<unary_call_base<Request, void>>>
    {
    public:
        /// @brief Responds to the client with empty message.
        ///
        /// Only the first call to \p Finish will be honored.
        void Finish()
        {
            this->impl().impl().Finish();
        }

        /// @brief Responds to the client with the given status and no message.
        ///
        /// Only the first call to \p Finish will be honored.
        void Finish(const grpc::Status& status)
        {
            this->impl().impl().Finish(status);
        }

    protected:
        void FinishEvent()
        {}
    };

    template <typename Request>
    class unary_call_result_base<unary_call_base<Request, bond::reflection::nothing>>
        : public unary_call_impl_base<unary_call_result_base<unary_call_base<Request, bond::reflection::nothing>>>
    {
    protected:
        void FinishEvent()
        {
            this->impl().impl().Finish();
        }
    };


    /// @brief Detail class that helps implement \ref unary_call and \ref
    /// shared_unary_call.
    template <typename Request, typename Response>
    class unary_call_base
        : public unary_call_input_base<unary_call_base<Request, Response>>,
          public unary_call_result_base<unary_call_base<Request, Response>>
    {
        using impl_type = unary_call_impl<
            typename payload<Request>::type,
            typename payload<Response>::type>;

    public:
        unary_call_base() = default;

        explicit unary_call_base(boost::intrusive_ptr<impl_type> impl) noexcept
            : _impl(std::move(impl))
        {
            BOOST_ASSERT(_impl);
            this->FinishEvent();
        }

        explicit operator bool() const noexcept
        {
            return static_cast<bool>(_impl);
        }

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

    private:
        friend class unary_call_input_base<unary_call_base>;
        friend class unary_call_result_base<unary_call_base>;

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
