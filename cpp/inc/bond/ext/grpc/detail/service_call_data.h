// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpc++/grpc++.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/service_type.h>
#include <grpc++/impl/codegen/status.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <bond/ext/grpc/detail/io_manager_tag.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/unary_call.h>

#include <boost/assert.hpp>
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
/// the detail::unary_call_impl has been dispatched to the user callback,
/// detail::unary_call_impl is in charge of its own lifetime, and
/// detail::service_unary_call_data re-enqueues itself to get the next call.
template <typename TRequest, typename TResponse>
struct service_unary_call_data : io_manager_tag
{
    typedef std::function<void(unary_call<TRequest, TResponse> call)> CallbackType;

    /// The service implementing the method.
    service* _service;
    /// The index of the method. Method indices correspond to the order in
    /// which they were registered with detail::service::AddMethod
    int _methodIndex;
    /// The completion port to post IO operations to.
    grpc::ServerCompletionQueue* _cq;
    /// The user code to invoke when a call to this method is received.
    CallbackType _cb;
    /// Individual state for one specific call to this method.
    std::unique_ptr<unary_call_impl<TRequest, TResponse>> _receivedCall;

    service_unary_call_data(
        service* service,
        int methodIndex,
        grpc::ServerCompletionQueue* cq,
        CallbackType cb)
        : _service(service),
          _methodIndex(methodIndex),
          _cq(cq),
          _cb(cb),
          _receivedCall(new unary_call_impl<TRequest, TResponse>)
    {
        BOOST_ASSERT(service);
        BOOST_ASSERT(cq);
        BOOST_ASSERT(cb);
    }

    void invoke(bool ok) override
    {
        if (ok)
        {
            // unary_call_impl::invoke will delete itself after it's posted
            // back to the completion queue as the result of sending a
            // response. The UnaryCall wrapper that we create inside the
            // thread guarantees that some response will always be sent.
            unary_call_impl<TRequest, TResponse>* receivedCall = _receivedCall.release();

            // TODO: switch to thread pool
            // TODO: use queuing policy here after switching to thread pool
            std::thread([this, receivedCall]()
            {
                _cb(unary_call<TRequest, TResponse> { receivedCall });
            }).detach();

            _receivedCall.reset(new unary_call_impl<TRequest, TResponse>);
            _service->queue_receive(
                _methodIndex,
                &_receivedCall->_context,
                &_receivedCall->_request,
                &_receivedCall->_responder,
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
