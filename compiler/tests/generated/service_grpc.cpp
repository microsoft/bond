
#include "service_grpc.h"

namespace tests
{

static const char* Foo_method_names[] =
{
    "/tests.Foo/foo11",
    "/tests.Foo/foo12",
    "/tests.Foo/foo12_impl",
    "/tests.Foo/foo13",
    "/tests.Foo/foo14",
    "/tests.Foo/foo15",
    "/tests.Foo/foo21",
    "/tests.Foo/foo22",
    "/tests.Foo/foo23",
    "/tests.Foo/foo24",
    "/tests.Foo/foo31",
    "/tests.Foo/foo32",
    "/tests.Foo/foo33",
    "/tests.Foo/_rd_foo33",
    "/tests.Foo/foo34",
    "/tests.Foo/foo41",
    "/tests.Foo/foo42",
    "/tests.Foo/foo43",
    "/tests.Foo/foo44",
    "/tests.Foo/cq",
};

Foo::FooClient::FooClient(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager)
    : channel_(channel)
    , ioManager_(ioManager)
    /* TODO stub ctor initialization for event foo11 */
    /* TODO stub ctor initialization for event foo12 */
    /* TODO stub ctor initialization for event foo12_impl */
    /* TODO stub ctor initialization for event foo13 */
    /* TODO stub ctor initialization for event foo14 */
    /* TODO stub ctor initialization for event foo15 */
    , rpcmethod_foo21_(Foo_method_names[6], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo22_(Foo_method_names[7], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo23_(Foo_method_names[8], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo24_(Foo_method_names[9], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo31_(Foo_method_names[10], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_(Foo_method_names[11], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_(Foo_method_names[12], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod__rd_foo33_(Foo_method_names[13], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo34_(Foo_method_names[14], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo41_(Foo_method_names[15], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo42_(Foo_method_names[16], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo43_(Foo_method_names[17], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo44_(Foo_method_names[18], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_cq_(Foo_method_names[19], ::grpc::RpcMethod::NORMAL_RPC, channel)
    { }

/* TODO: stub implementation for event foo11 */

/* TODO: stub implementation for event foo12 */

/* TODO: stub implementation for event foo12_impl */

/* TODO: stub implementation for event foo13 */

/* TODO: stub implementation for event foo14 */

/* TODO: stub implementation for event foo15 */

void Foo::FooClient::Asyncfoo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo21_, context, request);
}

void Foo::FooClient::Asyncfoo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message<void> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo22_, context, request);
}

void Foo::FooClient::Asyncfoo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo23_, context, request);
}

void Foo::FooClient::Asyncfoo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message<void>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo24_, context, request);
}

void Foo::FooClient::Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo31_, context, request);
}

void Foo::FooClient::Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo32_, context, request);
}

void Foo::FooClient::Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo33_, context, request);
}

void Foo::FooClient::Async_rd_foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod__rd_foo33_, context, request);
}

void Foo::FooClient::Asyncfoo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo34_, context, request);
}

void Foo::FooClient::Asyncfoo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo41_, context, request);
}

void Foo::FooClient::Asyncfoo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo42_, context, request);
}

void Foo::FooClient::Asyncfoo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo43_, context, request);
}

void Foo::FooClient::Asyncfoo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, std::function<void(const ::bond::comm::message< ::tests::dummy>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_foo44_, context, request);
}

void Foo::FooClient::Asynccq(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, std::function<void(const ::bond::comm::message< ::tests::BasicTypes>&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes> >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes> >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_cq_, context, request);
}

} // namespace tests

