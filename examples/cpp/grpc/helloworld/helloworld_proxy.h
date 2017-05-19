//------------------------------------------------------------------------------

#pragma once

#include "helloworld_reflection.h"
#include "helloworld_types.h"
#include <bond/comm/message.h>

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100)
#endif

#include <bond/ext/grpc/bond_utils.h>

//?#include <grpc++/impl/codegen/async_stream.h>
#include <grpc++/impl/codegen/async_unary_call.h>
#include <grpc++/impl/codegen/method_handler_impl.h>
//#include <grpc++/impl/codegen/bond_utils.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/service_type.h>
#include <grpc++/impl/codegen/status.h>
#include <grpc++/impl/codegen/stub_options.h>
//??#include <grpc++/impl/codegen/sync_stream.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <bond/ext/grpc/detail/client_call_data.h>
#include <bond/ext/grpc/io_manager.h>

namespace helloworld
{

class Greeter2 final {
 public:

  class GreeterClient {
   public:
    GreeterClient(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager);
    void AsyncSayHello(::grpc::ClientContext* context, const ::bond::comm::message< ::helloworld::HelloRequest>& request, std::function<void(const ::bond::comm::message< ::helloworld::HelloReply>&, const ::grpc::Status&)> cb);

    GreeterClient(const GreeterClient&) = delete;
    GreeterClient& operator=(const GreeterClient&) = delete;

    GreeterClient(GreeterClient&&) = default;
    GreeterClient& operator=(GreeterClient&&) = default;

   private:
    std::shared_ptr< ::grpc::ChannelInterface> channel_;
    std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager_;
    const ::grpc::RpcMethod rpcmethod_SayHello_;
  };
};

static const char* Greeter_method_names[] = {
    "/helloworld.Greeter/SayHello",
};

Greeter2::GreeterClient::GreeterClient(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager)
    : channel_(channel),
    ioManager_(ioManager),
    rpcmethod_SayHello_(Greeter_method_names[0], ::grpc::RpcMethod::NORMAL_RPC, channel)
  {
  }


void Greeter2::GreeterClient::AsyncSayHello(::grpc::ClientContext* context, const ::bond::comm::message< ::helloworld::HelloRequest>& request, std::function<void(const ::bond::comm::message< ::helloworld::HelloReply>&, const ::grpc::Status&)> cb) {
  ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::helloworld::HelloRequest> , ::bond::comm::message< ::helloworld::HelloReply> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::helloworld::HelloRequest> , ::bond::comm::message< ::helloworld::HelloReply> >(cb);
  calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_SayHello_, context, request);
}
}
