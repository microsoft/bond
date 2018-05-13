// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "client_call_data.h"

#include <bond/ext/grpc/thread_pool.h>

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100 4702)
#endif

#include <grpcpp/impl/codegen/channel_interface.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <boost/assert.hpp>

#include <functional>

namespace bond { namespace ext { namespace gRPC {

using Scheduler = std::function<void(const std::function<void()>& func)>;
    
namespace detail {

/// @brief Helper base class Bond gRPC++ clients.
///
/// @note This class is for use by generated and helper code only.
class client
{
public:
    client(
        std::shared_ptr<grpc::ChannelInterface> channel,
        std::shared_ptr<io_manager> ioManager,
        const Scheduler& scheduler = {})
        : _channel{ std::move(channel) },
          _ioManager{ std::move(ioManager) },
          _scheduler{ scheduler ? scheduler : thread_pool{} }
    {}

    client(const client& other) = delete;
    client& operator=(const client& other) = delete;

    client(client&& other) = default;
    client& operator=(client&& other) = default;

protected:
#if !defined(__GNUC__) || (__GNUC__ > 7) || (__GNUC__ == 7 && __GNUC_MINOR__ >= 2)
    using RpcMethod = grpc::internal::RpcMethod;
#else
    // Workaround for a bug in GCC < 7.2: https://gcc.gnu.org/bugzilla/show_bug.cgi?id=67054.
    struct RpcMethod : grpc::internal::RpcMethod
    {
        using grpc::internal::RpcMethod::RpcMethod;
        RpcMethod();
    };
#endif

    RpcMethod make_method(const char* name) const
    {
        return RpcMethod{ name, grpc::internal::RpcMethod::NORMAL_RPC, _channel };
    }

    template <typename Response, typename Request>
    void dispatch(
        const grpc::internal::RpcMethod& method,
        const bonded<Request>& request,
        std::shared_ptr<grpc::ClientContext> context = {},
        const std::function<void(unary_call_result<Response>)>& cb = {})
    {
        new client_unary_call_data<Request, Response>{
            method,
            request,
            _ioManager->cq(),
            _channel,
            context ? std::move(context) : std::make_shared<grpc::ClientContext>(),
            _scheduler,
            cb };
    }

    std::shared_ptr<grpc::ChannelInterface> _channel;
    std::shared_ptr<io_manager> _ioManager;
    Scheduler _scheduler;
};

} } } } // namespace bond::ext::gRPC::detail
