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
#include <boost/intrusive_ptr.hpp>
#include <boost/smart_ptr/intrusive_ref_counter.hpp>

#include <functional>
#include <memory>


namespace bond { namespace ext { namespace gRPC { namespace detail {

/// @brief Implementation class that hold the state associated with
/// outgoing unary calls.
template <typename Request, typename Response>
class client_unary_call_data
    : public boost::intrusive_ref_counter<client_unary_call_data<Request, Response>>,
      io_manager_tag
{
    /// The type of the user-defined callback that will be invoked for the
    /// response.
    using CallbackType = std::function<void(unary_call_result<Response>)>;

public:
    client_unary_call_data(
        const grpc::internal::RpcMethod& method,
        const bonded<Request>& request,
        std::shared_ptr<grpc::CompletionQueue> cq,
        std::shared_ptr<grpc::ChannelInterface> channel,
        std::shared_ptr<grpc::ClientContext> context,
        const Scheduler& scheduler,
        CallbackType cb = {})
        : _cq(std::move(cq)),
          _channel(std::move(channel)),
          _context(std::move(context)),
          _responseReader(
              ::grpc::internal::ClientAsyncResponseReaderFactory<bonded<Response>>::Create(
                  _channel.get(),
                  _cq.get(),
                  method,
                  _context.get(),
                  request,
                  /* start */ true)),
          _scheduler(scheduler),
          _response(),
          _status(),
          _cb(std::move(cb)),
          _self(this)
    {
        BOOST_ASSERT(_scheduler);

        auto alive = _self;

        _responseReader->Finish(&_response, &_status, tag());
    }

    client_unary_call_data(const client_unary_call_data& other) = delete;
    client_unary_call_data& operator=(const client_unary_call_data& other) = delete;

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

    /// The completion port to post IO operations to.
    std::shared_ptr<grpc::CompletionQueue> _cq;
    /// The channel to send the request on.
    std::shared_ptr<grpc::ChannelInterface> _channel;
    /// @brief The client context under which the request was executed.
    std::shared_ptr<grpc::ClientContext> _context;
    /// A response reader.
    std::unique_ptr<grpc::ClientAsyncResponseReader<bonded<Response>>> _responseReader;
    /// The scheduler in which to invoke the callback.
    Scheduler _scheduler;
    /// @brief The response received from the service.
    bonded<Response> _response;
    /// @brief The status of the request.
    grpc::Status _status;
    /// The user code to invoke when a response is received.
    CallbackType _cb;
    /// A pointer to ourselves used to keep us alive while waiting to
    /// receive the response.
    boost::intrusive_ptr<client_unary_call_data> _self;
};

} } } } //namespace bond::ext::gRPC::detail
