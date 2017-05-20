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

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/unary_call.h>

#include <boost/assert.hpp>
#include <functional>
#include <memory>
#include <thread>

namespace bond { namespace ext { namespace gRPC { namespace detail {

/// @brief Implementation class that hold the state associated with
/// outgoing incomming calls.
template <typename TRequest, typename TResponse>
struct client_unary_call_data : io_manager_tag
{
    typedef std::function<void(const TResponse&, const grpc::Status&)> CallbackType;

    /// The user code to invoke when a response is received.
    CallbackType _cb;
    TResponse _response;
    grpc::Status _status;
    std::unique_ptr<grpc::ClientAsyncResponseReader<TResponse>> _responseReader;

    client_unary_call_data(
        CallbackType cb)
        : _cb(cb),
        _response(),
        _status()
    {
        BOOST_ASSERT(cb);
    }

    void dispatch(grpc::ChannelInterface* channel,
           io_manager* ioManager,
           grpc::RpcMethod method,
           grpc::ClientContext* context,
           const TRequest& request)
    {
        _responseReader = std::unique_ptr<grpc::ClientAsyncResponseReader<TResponse>>(
            new ::grpc::ClientAsyncResponseReader<TResponse>(
                channel,
                ioManager->cq(),
                method,
                context,
                request));
        _responseReader->Finish(&_response, &_status, static_cast<void*>(this));
    }

    void invoke(bool ok) override
    {
        if (ok)
        {
            // TODO: switch to thread pool
            // TODO: use queuing policy here after switching to thread pool
            std::thread([this]()
            {
                _cb(_response, _status);

                delete this;
            }).detach();
        }
        else
        {
            delete this;
        }
    }
};

} } } } //namespace bond::ext::gRPC::detail
