
#include "service_attributes_grpc.h"

namespace tests
{

static const char* Foo_method_names[] =
{
    "/tests.Foo/foo",
};

Foo::FooClient::FooClient(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager)
    : channel_(channel)
    , ioManager_(ioManager)
    , rpcmethod_foo_(Foo_method_names[0], ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

void Foo::FooClient::Asyncfoo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, std::function<void(const ::bond::comm::message< ::tests::Result>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::Param>, ::bond::comm::message< ::tests::Result> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::Param>, ::bond::comm::message< ::tests::Result> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo_, context, request);
}

} // namespace tests

