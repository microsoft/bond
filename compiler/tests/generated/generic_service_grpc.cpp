
#include "generic_service_reflection.h"
#include "generic_service_grpc.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100)
#endif

//#include <grpc++/impl/codegen/async_stream.h>
#include <grpc++/impl/codegen/async_unary_call.h>
#include <grpc++/impl/codegen/channel_interface.h>
#include <grpc++/impl/codegen/client_unary_call.h>
#include <grpc++/impl/codegen/method_handler_impl.h>
#include <grpc++/impl/codegen/rpc_service_method.h>
#include <grpc++/impl/codegen/service_type.h>
//#include <grpc++/impl/codegen/sync_stream.h>


namespace tests
{

static const char* Foo_method_names[] =
{
    "/tests.Foo/foo31",
    "/tests.Foo/foo32",
    "/tests.Foo/foo33",
};

std::unique_ptr< Foo::Stub> Foo::NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options)
{
    std::unique_ptr< Foo::Stub> stub(new Foo::Stub(channel));
    return stub;
}

Foo::Stub::Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel)
    : channel_(channel)
    , rpcmethod_foo31_(Foo_method_names[0], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo32_(Foo_method_names[1], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo33_(Foo_method_names[2], ::grpc::RpcMethod::NORMAL_RPC, channel)
  { }

::grpc::Status Foo::Stub::foo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<void>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo31_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Foo::Stub::Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>(channel_.get(), cq, rpcmethod_foo31_, context, request);
}

::grpc::Status Foo::Stub::foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<Payload>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo32_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>* Foo::Stub::Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>(channel_.get(), cq, rpcmethod_foo32_, context, request);
}

::grpc::Status Foo::Stub::foo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<Payload>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo33_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>* Foo::Stub::Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>(channel_.get(), cq, rpcmethod_foo33_, context, request);
}

} // namespace tests

#ifdef _MSC_VER
#pragma warning (pop)
#endif
