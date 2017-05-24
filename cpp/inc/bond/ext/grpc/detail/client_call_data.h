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
/// outgoing unary calls.
template <typename TRequest, typename TResponse, typename TThreadPool>
struct client_unary_call_data : io_manager_tag
{
    /// The type of the user-defined callback that will be invoked for the
    /// response.
    typedef std::function<void(const TResponse&, const grpc::Status&)> CallbackType;

    /// The channel to send the request on.
    std::shared_ptr<grpc::ChannelInterface> _channel;
    /// The io_manager to use for both sending and receiving.
    std::shared_ptr<io_manager> _ioManager;
    /// The thread pool in which to invoke the callback.
    std::shared_ptr<TThreadPool> _threadPool;
    /// A response reader.
    std::unique_ptr<grpc::ClientAsyncResponseReader<TResponse>> _responseReader;

    /// The user code to invoke when a response is received.
    CallbackType _cb;
    /// The response received.
    TResponse _response;
    /// The status received.
    grpc::Status _status;

    client_unary_call_data(
        std::shared_ptr<grpc::ChannelInterface> channel,
        std::shared_ptr<io_manager> ioManager,
        std::shared_ptr<TThreadPool> threadPool,
        CallbackType cb)
        : _channel(channel),
          _ioManager(ioManager),
          _threadPool(threadPool),
          _responseReader(),
          _cb(cb),
          _response(),
          _status()
    {
        BOOST_ASSERT(channel);
        BOOST_ASSERT(ioManager);
        BOOST_ASSERT(threadPool);
        BOOST_ASSERT(cb);
    }

    void dispatch(
        grpc::RpcMethod method,
        grpc::ClientContext* context,
        const TRequest& request)
    {
        _responseReader = std::unique_ptr<grpc::ClientAsyncResponseReader<TResponse>>(
            new ::grpc::ClientAsyncResponseReader<TResponse>(
                _channel.get(),
                _ioManager->cq(),
                method,
                context,
                request));
        _responseReader->Finish(&_response, &_status, static_cast<void*>(this));
    }

    void invoke(bool ok) override
    {
        if (ok)
        {
            _threadPool->schedule([this]()
            {
                _cb(_response, _status);

                delete this;
            });
        }
        else
        {
            delete this;
        }
    }
};

} } } } //namespace bond::ext::gRPC::detail
