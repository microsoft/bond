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
/// receiving incoming calls for one method.
///
/// There only needs to be one of these per method in a service, and it can
/// be re-used for receiving subsequent calls. A new detail::unary_call_impl
/// is created for each individual call to hold the call-specific data. Once
/// the invocation of the user callback along with the call-specific data
/// has been scheduled, detail::service_unary_call_data re-enqueues itself
/// to get the next call.
template <typename Request, typename Response>
class service_unary_call_data : io_manager_tag
{
public:
    template <typename Callback>
    service_unary_call_data(service& service, int methodIndex, Callback&& cb)
        : _service(service),
          _methodIndex(methodIndex),
          _cb(std::forward<Callback>(cb)),
          _receivedCall()
    {
        BOOST_ASSERT(_cb);
        queue_receive();
    }

    service_unary_call_data(const service_unary_call_data& other) = delete;
    service_unary_call_data& operator=(const service_unary_call_data& other) = delete;

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

} } } } //namespace bond::ext::gRPC::detail
