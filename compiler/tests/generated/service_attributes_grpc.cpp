
#include "service_attributes_reflection.h"
#include "service_attributes_grpc.h"

#pragma warning (push)
#pragma warning (disable: 4100)

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
    "/tests.Foo/foo",
};

std::unique_ptr< Foo::Stub> Foo::NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options)
{
    std::unique_ptr< Foo::Stub> stub(new Foo::Stub(channel));
    return stub;
}

Foo::Stub::Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel)
    : channel_(channel)
    , rpcmethod_foo_(Foo_method_names[0], ::grpc::RpcMethod::NORMAL_RPC, channel)
  { }

::grpc::Status Foo::Stub::foo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::bond::comm::message< ::tests::Result>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>* Foo::Stub::AsyncfooRaw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>(channel_.get(), cq, rpcmethod_foo_, context, request);
}

} // namespace tests

#pragma warning (pop)
