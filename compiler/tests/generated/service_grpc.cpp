
#include "service_reflection.h"
#include "service_grpc.h"

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
    "/tests.Foo/foo34",
    "/tests.Foo/foo41",
    "/tests.Foo/foo42",
    "/tests.Foo/foo43",
    "/tests.Foo/foo44",
};

std::unique_ptr< Foo::Stub> Foo::NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options)
{
    std::unique_ptr< Foo::Stub> stub(new Foo::Stub(channel));
    return stub;
}

Foo::Stub::Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel)
    : channel_(channel)
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
    , rpcmethod_foo34_(Foo_method_names[13], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo41_(Foo_method_names[14], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo42_(Foo_method_names[15], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo43_(Foo_method_names[16], ::grpc::RpcMethod::NORMAL_RPC, channel)
    , rpcmethod_foo44_(Foo_method_names[17], ::grpc::RpcMethod::NORMAL_RPC, channel)
  { }

/* TODO: stub implementation for event foo11 */

/* TODO: stub implementation for event foo12 */

/* TODO: stub implementation for event foo12_impl */

/* TODO: stub implementation for event foo13 */

/* TODO: stub implementation for event foo14 */

/* TODO: stub implementation for event foo15 */

::grpc::Status Foo::Stub::foo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo21_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Foo::Stub::Asyncfoo21Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>(channel_.get(), cq, rpcmethod_foo21_, context, request);
}

::grpc::Status Foo::Stub::foo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo22_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Foo::Stub::Asyncfoo22Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>(channel_.get(), cq, rpcmethod_foo22_, context, request);
}

::grpc::Status Foo::Stub::foo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message<void>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo23_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Foo::Stub::Asyncfoo23Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>(channel_.get(), cq, rpcmethod_foo23_, context, request);
}

::grpc::Status Foo::Stub::foo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message<void>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo24_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Foo::Stub::Asyncfoo24Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>(channel_.get(), cq, rpcmethod_foo24_, context, request);
}

::grpc::Status Foo::Stub::foo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo31_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Foo::Stub::Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>(channel_.get(), cq, rpcmethod_foo31_, context, request);
}

::grpc::Status Foo::Stub::foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo32_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Foo::Stub::Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>(channel_.get(), cq, rpcmethod_foo32_, context, request);
}

::grpc::Status Foo::Stub::foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo33_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Foo::Stub::Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>(channel_.get(), cq, rpcmethod_foo33_, context, request);
}

::grpc::Status Foo::Stub::foo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo34_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Foo::Stub::Asyncfoo34Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>(channel_.get(), cq, rpcmethod_foo34_, context, request);
}

::grpc::Status Foo::Stub::foo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo41_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Foo::Stub::Asyncfoo41Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>(channel_.get(), cq, rpcmethod_foo41_, context, request);
}

::grpc::Status Foo::Stub::foo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo42_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Foo::Stub::Asyncfoo42Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>(channel_.get(), cq, rpcmethod_foo42_, context, request);
}

::grpc::Status Foo::Stub::foo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::dummy>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo43_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Foo::Stub::Asyncfoo43Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>(channel_.get(), cq, rpcmethod_foo43_, context, request);
}

::grpc::Status Foo::Stub::foo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::dummy>* response)
{
    return ::grpc::BlockingUnaryCall(channel_.get(), rpcmethod_foo44_, context, request, response);
}
::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Foo::Stub::Asyncfoo44Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq)
{
    return new ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>(channel_.get(), cq, rpcmethod_foo44_, context, request);
}

Foo::Service::Service()
{
    /* TODO: register event foo11 */
    /* TODO: register event foo12 */
    /* TODO: register event foo12_impl */
    /* TODO: register event foo13 */
    /* TODO: register event foo14 */
    /* TODO: register event foo15 */
    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[6],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message<void>, ::bond::comm::message<void>>(
          std::mem_fn(&Foo::Service::foo21), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[7],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message<void>, ::bond::comm::message<void>>(
          std::mem_fn(&Foo::Service::foo22), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[8],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message<void>>(
          std::mem_fn(&Foo::Service::foo23), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[9],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message< ::tests::dummy>, ::bond::comm::message<void>>(
          std::mem_fn(&Foo::Service::foo24), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[10],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>(
          std::mem_fn(&Foo::Service::foo31), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[11],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message<void>, ::bond::comm::message< ::tests::BasicTypes>>(
          std::mem_fn(&Foo::Service::foo32), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[12],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::BasicTypes>>(
          std::mem_fn(&Foo::Service::foo33), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[13],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::BasicTypes>>(
          std::mem_fn(&Foo::Service::foo34), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[14],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>>(
          std::mem_fn(&Foo::Service::foo41), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[15],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message<void>, ::bond::comm::message< ::tests::dummy>>(
          std::mem_fn(&Foo::Service::foo42), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[16],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message< ::tests::BasicTypes>, ::bond::comm::message< ::tests::dummy>>(
          std::mem_fn(&Foo::Service::foo43), this)));

    AddMethod(new ::grpc::RpcServiceMethod(
      Foo_method_names[17],
      ::grpc::RpcMethod::NORMAL_RPC,
      new ::grpc::RpcMethodHandler< Foo::Service, ::bond::comm::message< ::tests::dummy>, ::bond::comm::message< ::tests::dummy>>(
          std::mem_fn(&Foo::Service::foo44), this)));

}

Foo::Service::~Service() { }

/*TODO: service implementation of event foo11 */

/*TODO: service implementation of event foo12 */

/*TODO: service implementation of event foo12_impl */

/*TODO: service implementation of event foo13 */

/*TODO: service implementation of event foo14 */

/*TODO: service implementation of event foo15 */

::grpc::Status Foo::Service::foo21(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<void>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo22(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<void>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo23(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message<void>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo24(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message<void>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo31(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo32(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo33(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo34(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message< ::tests::BasicTypes>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo41(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::dummy>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo42(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::dummy>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo43(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message< ::tests::dummy>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}

::grpc::Status Foo::Service::foo44(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message< ::tests::dummy>* response)
{
    (void) context;
    (void) request;
    (void) response;
    return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
}


} // namespace tests

#pragma warning (pop)
