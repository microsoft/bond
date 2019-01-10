// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "io_manager_tag.h"

#include <bond/ext/grpc/abstract_service.h>
#include <bond/ext/grpc/scheduler.h>
#include <bond/ext/grpc/unary_call.h>

#ifdef _MSC_VER
    #pragma warning (push)
    // warning C4100: unreferenced formal parameter
    //
    // warning C4127: conditional expression is constant
    //
    // warning C4702: unreachable code
    //
    // warning C4800: 'int': forcing value to bool 'true' or 'false' (performance warning)
    #pragma warning (disable: 4100 4127 4702 4800)
#endif

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/completion_queue.h>
#include <grpcpp/impl/codegen/server_context.h>
#include <grpcpp/impl/codegen/service_type.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>
#include <boost/smart_ptr/intrusive_ptr.hpp>

#include <initializer_list>
#include <functional>
#include <memory>

namespace bond { namespace ext { namespace grpc
{
    class server;

namespace detail
{
    /// @brief Base class that all Bond grpc++ services implement.
    ///
    /// @note This class is for use by generated and helper code only.
    ///
    /// Helper class that codegen uses to generate abstract service classes,
    /// which a bond::ext::grpc::server then hosts multiple services.
    class service : public abstract_service, private ::grpc::Service
    {
        class unary_call_data;

    public:
        /// @brief Provides access to the raw ::grpc::Service type.
        ///
        /// @note This method is for use by generated and helper code only.
        ::grpc::Service* grpc_service()
        {
            return this;
        }

        Scheduler& scheduler()
        {
            return _scheduler;
        }

        template <typename ServiceT, typename Request, typename Response>
        std::function<void(unary_call<Request, Response>)>
        static make_callback(void (ServiceT::*callback)(unary_call<Request, Response>), ServiceT& svc)
        {
            BOOST_STATIC_ASSERT(std::is_base_of<service, ServiceT>::value);
            return std::bind(callback, &svc, std::placeholders::_1);
        }

    protected:
        using Method = unary_call_data;

        service(const Scheduler& scheduler, std::initializer_list<const char*> methodNames)
            : _scheduler{ scheduler },
              _cq{ nullptr }
        {
            BOOST_ASSERT(_scheduler);
            AddMethods(methodNames);
        }

    private:
        friend class grpc::server;

        /// @brief Starts the service.
        ///
        /// @note This method is for use by generated and helper code only.
        ///
        /// Typical implementations call queue_receive on all the methods in the
        /// service to kick of the process of receiving messages.
        virtual void start() = 0;

        /// @brief Starts the receive process for a method.
        ///
        /// @note This method is for use by generated and helper code only.
        ///
        /// When a request for the method has been received, \p tag will be
        /// added to \p cq.
        ///
        /// @param methodIndex the index of the method (indices are assigned by
        /// the order in which the methods are registered via calls to
        /// AddMethod)
        ///
        /// @param context a fresh ::grpc::ServerContext for the call to populate
        ///
        /// @param request pointer to a request object to populate
        ///
        /// @param responseStream pointer to a response stream to populate
        ///
        /// @param tag the io_manager_tag to include with the completion queue
        /// notification
        template <typename Request>
        void queue_receive(
            int methodIndex,
            ::grpc::ServerContext* context,
            Request* request,
            ::grpc::internal::ServerAsyncStreamingInterface* responseStream,
            io_manager_tag* tag)
        {
            BOOST_ASSERT(_cq);
            RequestAsyncUnary(methodIndex, context, request, responseStream, _cq, _cq, tag);
        }

        void AddMethods(std::initializer_list<const char*> names)
        {
            for (const char* name : names)
            {
                BOOST_ASSERT(name);

                // ownership of the service method is transfered to ::grpc::Service
                ::grpc::Service::AddMethod(
                    new ::grpc::internal::RpcServiceMethod(
                        name,
                        ::grpc::internal::RpcMethod::NORMAL_RPC,
                        nullptr)); // nullptr indicates async handler
            }
        }

        void SetCompletionQueue(::grpc::ServerCompletionQueue* cq)
        {
            BOOST_ASSERT(!_cq);
            _cq = cq;
        }

        Scheduler _scheduler;
        ::grpc::ServerCompletionQueue* _cq;
    };

    /// @brief Implementation class that hold the state associated with
    /// receiving incoming calls for one method.
    ///
    /// There only needs to be one of these per method in a service, and it can
    /// be re-used for receiving subsequent calls. A new detail::unary_call_impl
    /// is created for each individual call to hold the call-specific data. Once
    /// the invocation of the user callback along with the call-specific data
    /// has been scheduled, unary_call_data re-enqueues itself to get the next call.
    class service::unary_call_data : io_manager_tag
    {
    public:
        template <typename Request, typename Response>
        unary_call_data(
            service& service,
            int methodIndex,
            const std::function<void(unary_call<Request, Response>)>& cb)
            : _service{ service },
              _methodIndex{ methodIndex },
              _invoke{ std::bind(&unary_call_data::invoke<Request, Response>, this, cb) },
              _receivedCall{}
        {
            BOOST_ASSERT(cb);
            queue_receive();
        }

    private:
        template <typename Request, typename Response>
        void invoke(const std::function<void(unary_call<Request, Response>)>& callback)
        {
            // TODO: Use lambda with move-capture when allowed to use C++14.
            _service.scheduler()(std::bind(
                [](const decltype(callback)& cb, boost::intrusive_ptr<unary_call_impl>& receivedCall)
                {
                    cb(unary_call<Request, Response>{ std::move(receivedCall) });
                },
                callback,
                queue_receive()));
        }

        void invoke(bool ok) override
        {
            if (ok)
            {
                BOOST_ASSERT(_invoke);
                _invoke();
            }
        }

        boost::intrusive_ptr<unary_call_impl> queue_receive()
        {
            boost::intrusive_ptr<unary_call_impl> receivedCall{ _receivedCall.release() };

            // create new state for the next request that will be received
            _receivedCall.reset(new unary_call_impl{});

            _service.queue_receive(
                _methodIndex,
                &_receivedCall->context(),
                &_receivedCall->request_buffer(),
                &_receivedCall->responder(),
                tag());

            return receivedCall;
        }

        /// The service implementing the method.
        service& _service;
        /// The index of the method. Method indices correspond to the order in
        /// which they were registered with detail::service::AddMethod
        const int _methodIndex;
        /// @brief Type-erased function to invoke user-callback for a response.
        std::function<void()> _invoke;
        /// Individual state for one specific call to this method.
        std::unique_ptr<unary_call_impl> _receivedCall;
    };

} } } } // namespace bond::ext::grpc::detail
