// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bonded.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/client_callback.h>
#include <bond/ext/grpc/io_manager.h>
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

#include <functional>
#include <memory>


namespace bond { namespace ext { namespace gRPC { namespace detail {

/// @brief Implementation class that hold the state associated with
/// outgoing unary calls.
template <typename Request, typename Response>
class client_unary_call_data
    : public std::enable_shared_from_this<client_unary_call_data<Request, Response>>,
      io_manager_tag
{
    /// The type of the user-defined callback that will be invoked for the
    /// response.
    using CallbackType = std::function<void(unary_call_result<Response>)>;

public:
    client_unary_call_data(
        std::shared_ptr<grpc::ChannelInterface> channel,
        std::shared_ptr<io_manager> ioManager,
        const Scheduler& scheduler,
        std::shared_ptr<grpc::ClientContext> context,
        CallbackType cb = {})
        : _channel(std::move(channel)),
          _cq(ioManager->cq()),
          _scheduler(scheduler),
          _responseReader(),
          _context(std::move(context)),
          _cb(std::move(cb)),
          _self()
    {
        BOOST_ASSERT(_channel);
        BOOST_ASSERT(_cq);
        BOOST_ASSERT(_scheduler);
        BOOST_ASSERT(_context);
    }

    client_unary_call_data(const client_unary_call_data& other) = delete;
    client_unary_call_data& operator=(const client_unary_call_data& other) = delete;

    /// @brief Initiates the client request and wires up completion
    /// notification.
    void dispatch(const grpc::internal::RpcMethod& method, const bonded<Request>& request)
    {
        _responseReader.reset(
            ::grpc::internal::ClientAsyncResponseReaderFactory<bonded<Response>>::Create(
                _channel.get(),
                _cq,
                method,
                _context.get(),
                request,
                /* start */ true));

        _self = this->shared_from_this();

        auto self = _self; // Make sure `this` will outlive the below call.
        _responseReader->Finish(&_response, &_status, tag());
    }

private:
    /// @brief Invoked after the response has been received.
    void invoke(bool ok) override
    {
        if (ok && _cb)
        {
            // TODO: Use lambda with move-capture when allowed to use C++14.
            _scheduler(std::bind(
                [](CallbackType& cb, unary_call_result<Response>& result) { cb(std::move(result)); },
                std::move(_cb),
                unary_call_result<Response>{ std::move(_response), _status, std::move(_context) }));
        }

        _self.reset();
    }


    /// The channel to send the request on.
    std::shared_ptr<grpc::ChannelInterface> _channel;
    /// The completion port to post IO operations to.
    grpc::CompletionQueue* _cq;
    /// The scheduler in which to invoke the callback.
    Scheduler _scheduler;
    /// A response reader.
    std::unique_ptr<grpc::ClientAsyncResponseReader<bonded<Response>>> _responseReader;
    /// @brief The client context under which the request was executed.
    std::shared_ptr<grpc::ClientContext> _context;
    /// @brief The response received from the service.
    bonded<Response> _response;
    /// @brief The status of the request.
    grpc::Status _status;
    /// The user code to invoke when a response is received.
    CallbackType _cb;
    /// A pointer to ourselves used to keep us alive while waiting to
    /// receive the response.
    std::shared_ptr<client_unary_call_data> _self;
};

} } } } //namespace bond::ext::gRPC::detail
