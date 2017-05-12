
#pragma once

#include "service_attributes_reflection.h"
#include "service_attributes_types.h"
#include <bond/comm/message.h>


#pragma warning (push)
#pragma warning (disable: 4100 4267)
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

namespace tests
{

class Foo final {
 public:
  class StubInterface {
   public:
    virtual ~StubInterface() {}
    virtual ::grpc::Status foo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::bond::comm::message< ::tests::Result>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::Result>>> Asyncfoo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::Result>>>(AsyncfooRaw(context, request, cq));
    }

   private:
    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::Result>>* AsyncfooRaw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq) = 0;

  };

  class Stub final : public StubInterface {
   public:
    Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);
    ::grpc::Status foo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::bond::comm::message< ::tests::Result>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>> Asyncfoo(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>>(AsyncfooRaw(context, request, cq));
    }

   private:
    std::shared_ptr< ::grpc::ChannelInterface> channel_;
    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::Result>>* AsyncfooRaw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::Param>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo_;

  };

  static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

  class Service : public ::grpc::Service {
   public:
    Service();
    virtual ~Service();
    virtual ::grpc::Status foo(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::Param>* request, ::bond::comm::message< ::tests::Result>* response);
  };


  template <class BaseClass>
  class WithAsyncMethod_foo : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo() {
      ::grpc::Service::MarkMethodAsync(0);
    }
    ~WithAsyncMethod_foo() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::Param>* request, ::bond::comm::message< ::tests::Result>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo(::grpc::ServerContext* context, ::bond::comm::message< ::tests::Param>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::Result>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(0, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  typedef WithAsyncMethod_foo<Service > AsyncService;

};


} // namespace tests

#pragma warning (pop)
