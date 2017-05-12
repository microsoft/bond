
#pragma once

#include "generic_service_reflection.h"
#include "generic_service_types.h"
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
    virtual ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<void>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo31Raw(context, request, cq));
    }

    virtual ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<Payload>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>>(Asyncfoo32Raw(context, request, cq));
    }

    virtual ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<Payload>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>>(Asyncfoo33Raw(context, request, cq));
    }

   private:
    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<Payload>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) = 0;

  };

  class Stub final : public StubInterface {
   public:
    Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);
    ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<void>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo31Raw(context, request, cq));
    }

    ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<Payload>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>>(Asyncfoo32Raw(context, request, cq));
    }

    ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::bond::comm::message<Payload>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>>(Asyncfoo33Raw(context, request, cq));
    }

   private:
    std::shared_ptr< ::grpc::ChannelInterface> channel_;
    ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo31_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo32_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message<Payload>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message<Payload>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo33_;

  };

  static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

  class Service : public ::grpc::Service {
   public:
    Service();
    virtual ~Service();
    virtual ::grpc::Status foo31(::grpc::ServerContext* context, const ::bond::comm::message<Payload>* request, ::bond::comm::message<void>* response);

    virtual ::grpc::Status foo32(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<Payload>* response);

    virtual ::grpc::Status foo33(::grpc::ServerContext* context, const ::bond::comm::message<Payload>* request, ::bond::comm::message<Payload>* response);
  };


  template <class BaseClass>
  class WithAsyncMethod_foo31 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo31() {
      ::grpc::Service::MarkMethodAsync(0);
    }
    ~WithAsyncMethod_foo31() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo31(::grpc::ServerContext* context, const ::bond::comm::message<Payload>* request, ::bond::comm::message<void>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo31(::grpc::ServerContext* context, ::bond::comm::message<Payload>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message<void>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(0, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo32 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo32() {
      ::grpc::Service::MarkMethodAsync(1);
    }
    ~WithAsyncMethod_foo32() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo32(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<Payload>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo32(::grpc::ServerContext* context, ::bond::comm::message<void>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message<Payload>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(1, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo33 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo33() {
      ::grpc::Service::MarkMethodAsync(2);
    }
    ~WithAsyncMethod_foo33() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo33(::grpc::ServerContext* context, const ::bond::comm::message<Payload>* request, ::bond::comm::message<Payload>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo33(::grpc::ServerContext* context, ::bond::comm::message<Payload>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message<Payload>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(2, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  typedef WithAsyncMethod_foo31<WithAsyncMethod_foo32<WithAsyncMethod_foo33<Service > > > AsyncService;

};


} // namespace tests

#pragma warning (pop)
