// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bond_utils.h"
#include "io_manager_tag.h"

#include <bond/ext/grpc/scheduler.h>
#include <bond/ext/grpc/unary_call.h>

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
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

namespace bond { namespace ext { namespace gRPC
{

class server_builder;

namespace detail
{
    /// @brief Base class that all Bond gRPC++ services implement.
    ///
    /// @note This class is for use by generated and helper code only.
    ///
    /// Helper class that codegen uses to generate abstract service classes,
    /// which a bond::ext::gRPC::server then hosts multiple services.
    class service : private grpc::Service
    {
    public:
        service(const service& other) = delete;
        service& operator=(const service& other) = delete;

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
        /// @param context a fresh grpc::ServerContext for the call to populate
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
            grpc::ServerContext* context,
            Request* request,
            grpc::internal::ServerAsyncStreamingInterface* responseStream,
            io_manager_tag* tag)
        {
            BOOST_ASSERT(_cq);

            RequestAsyncUnary(
                methodIndex,
                context,
                request,
                responseStream,
                _cq,
                _cq,
                tag);
        }

        /// @brief Provides access to the raw grpc::Service type.
        ///
        /// @note This method is for use by generated and helper code only.
        grpc::Service* grpc_service()
        {
            return this;
        }

        Scheduler& scheduler()
        {
            return _scheduler;
        }

    private:
        template <typename Request, typename Response>
        class unary_call_data;

    protected:
        template <typename MethodT>
        using Method = unary_call_data<
            typename MethodT::input_type,
            typename remove_bonded<
                typename std::conditional<
                    std::is_void<typename MethodT::result_type>::value,
                    Void,
                    typename MethodT::result_type>::type>::type>;

        service(const Scheduler& scheduler, std::initializer_list<const char*> methodNames)
            : _scheduler{ scheduler },
              _cq{ nullptr }
        {
            BOOST_ASSERT(_scheduler);

            AddMethods(methodNames);
        }

    private:
        friend class gRPC::server_builder;

        void AddMethods(std::initializer_list<const char*> names)
        {
            for (const char* name : names)
            {
                BOOST_ASSERT(name);

                // ownership of the service method is transfered to grpc::Service
                grpc::Service::AddMethod(
                    new grpc::internal::RpcServiceMethod(
                        name,
                        grpc::internal::RpcMethod::NORMAL_RPC,
                        nullptr)); // nullptr indicates async handler
            }
        }

        void SetCompletionQueue(grpc::ServerCompletionQueue* cq)
        {
            BOOST_ASSERT(!_cq);
            _cq = cq;
        }

        Scheduler _scheduler;
        grpc::ServerCompletionQueue* _cq;
    };

    /// @brief Implementation class that hold the state associated with
    /// receiving incoming calls for one method.
    ///
    /// There only needs to be one of these per method in a service, and it can
    /// be re-used for receiving subsequent calls. A new detail::unary_call_impl
    /// is created for each individual call to hold the call-specific data. Once
    /// the invocation of the user callback along with the call-specific data
    /// has been scheduled, detail::service_unary_call_data re-enqueues itself
    /// to get the next call.
    template <typename Request, typename Response>
    class service::unary_call_data : io_manager_tag
    {
    public:
        template <typename Callback>
        unary_call_data(service& service, int methodIndex, Callback&& cb)
            : _service(service),
              _methodIndex(methodIndex),
              _cb(std::forward<Callback>(cb)),
              _receivedCall()
        {
            BOOST_ASSERT(_cb);
            queue_receive();
        }

        void invoke(bool ok) override
        {
            if (ok)
            {
                // TODO: Use lambda with move-capture when allowed to use C++14.
                _service.scheduler()(std::bind(
                    [](const decltype(_cb)& cb, boost::intrusive_ptr<uc_impl>& receivedCall)
                    {
                        cb(unary_call<Request, Response>{ std::move(receivedCall) });
                    },
                    _cb,
                    queue_receive()));
            }
        }

    private:
        using uc_impl = unary_call_impl<Request, Response>;

        boost::intrusive_ptr<uc_impl> queue_receive()
        {
            boost::intrusive_ptr<uc_impl> receivedCall{ _receivedCall.release() };

            // create new state for the next request that will be received
            _receivedCall.reset(new uc_impl);

            _service.queue_receive(
                _methodIndex,
                &_receivedCall->context(),
                &_receivedCall->request(),
                &_receivedCall->responder(),
                tag());

            return receivedCall;
        }

        /// The service implementing the method.
        service& _service;
        /// The index of the method. Method indices correspond to the order in
        /// which they were registered with detail::service::AddMethod
        const int _methodIndex;
        /// The user code to invoke when a call to this method is received.
        std::function<void(unary_call<Request, Response>)> _cb;
        /// Individual state for one specific call to this method.
        std::unique_ptr<uc_impl> _receivedCall;
    };

} } } } // namespace bond::ext::gRPC::detail
