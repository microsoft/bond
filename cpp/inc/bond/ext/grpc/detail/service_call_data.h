// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/ext/grpc/detail/io_manager_tag.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/unary_call.h>

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpcpp/grpcpp.h>
#include <grpcpp/impl/codegen/rpc_method.h>
#include <grpcpp/impl/codegen/service_type.h>
#include <grpcpp/impl/codegen/status.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>
#include <boost/smart_ptr/intrusive_ptr.hpp>

#include <functional>
#include <memory>
#include <thread>

namespace bond { namespace ext { namespace gRPC { namespace detail {

/// @brief Implementation class that hold the state associated with
/// receiving incomming calls for one method.
///
/// There only needs to be one of these per method in a service, and it can
/// be re-used for receiving subsequent calls. A new detail::unary_call_impl
/// is created for each individual call to hold the call-specific data. Once
/// the invocation of the user callback along with the call-specific data
/// has been enqueued in the thread pool, detail::service_unary_call_data
/// re-enqueues itself to get the next call.
template <typename TRequest, typename TResponse, typename TThreadPool>
struct service_unary_call_data : io_manager_tag
{
    /// The type of the user-defined callback that will be invoked upon receipt
    /// of this call.
    typedef std::function<void(unary_call<TRequest, TResponse> call)> CallbackType;

    using uc_impl = unary_call_impl<TRequest, TResponse>;

    /// The service implementing the method.
    service<TThreadPool>* _service;
    /// The index of the method. Method indices correspond to the order in
    /// which they were registered with detail::service::AddMethod
    int _methodIndex;
    /// The completion port to post IO operations to.
    grpc::ServerCompletionQueue* _cq;
    /// The thread pool implementation to use to invoke the user callback.
    std::shared_ptr<TThreadPool> _threadPool;
    /// The user code to invoke when a call to this method is received.
    CallbackType _cb;
    /// Individual state for one specific call to this method.
    std::unique_ptr<uc_impl> _receivedCall;

    service_unary_call_data(
        service<TThreadPool>* service,
        int methodIndex,
        grpc::ServerCompletionQueue* cq,
        std::shared_ptr<TThreadPool> threadPool,
        CallbackType cb)
        : _service(service),
          _methodIndex(methodIndex),
          _cq(cq),
          _threadPool(threadPool),
          _cb(cb),
          _receivedCall(new uc_impl)
    {
        BOOST_ASSERT(service);
        BOOST_ASSERT(cq);
        BOOST_ASSERT(threadPool);
        BOOST_ASSERT(cb);
    }

    void invoke(bool ok) override
    {
        if (ok)
        {
            // Capture the data associated with this one incomming request
            // so that we can pass it to the user callback. When we create
            // the unary_call to pass to the user callback, the unary_call
            // and unary_call_impl start collaborating to manage the
            // lifetime of the unary_call_impl.
            {
                // We have to manually release the pointer from
                // _receivedCall to capture it in the lambda. When we can
                // use C++14, we can simplify this by using lambda's
                // capture-by-move.
                auto receivedCall = boost::intrusive_ptr<uc_impl>{ _receivedCall.release() };

                _threadPool->schedule([this, receivedCall]() mutable
                {
                    _cb(unary_call<TRequest, TResponse> { std::move(receivedCall) });
                });
            }

            // create new state for the next request that will be received
            _receivedCall.reset(new uc_impl);
            _service->queue_receive(
                _methodIndex,
                &_receivedCall->context(),
                &_receivedCall->request(),
                &_receivedCall->responder(),
                _cq,
                this);
        }
        else
        {
            // we're shutting down, so don't requeue
        }
    }
};

} } } } //namespace bond::ext::gRPC::detail
