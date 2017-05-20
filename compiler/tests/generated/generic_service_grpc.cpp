
#include "generic_service_grpc.h"

namespace tests
{

static const char* Foo_method_names[] =
{
    "/tests.Foo/foo31",
    "/tests.Foo/foo32",
    "/tests.Foo/foo33",
};

Foo::FooClient::FooClient(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager)
    : channel_(channel)
    , ioManager_(ioManager)
    , rpcmethod_foo31_(Foo_method_names[0], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_(Foo_method_names[1], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_(Foo_method_names[2], ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

void Foo::FooClient::Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<void> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<void> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo31_, context, request);
}

void Foo::FooClient::Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<Payload>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<Payload> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<Payload> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo32_, context, request);
}

void Foo::FooClient::Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, std::function<void(const ::bond::comm::message<Payload>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<Payload> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<Payload>, ::bond::comm::message<Payload> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo33_, context, request);
}

} // namespace tests

