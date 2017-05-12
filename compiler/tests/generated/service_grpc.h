
#pragma once

#include "service_reflection.h"
#include "service_types.h"
#include <bond/comm/message.h>
#include "basic_types_grpc.h"
#include "namespace_basic_types_grpc.h"

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
    virtual ::grpc::Status foo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo21Raw(context, request, cq));
    }

    virtual ::grpc::Status foo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo22Raw(context, request, cq));
    }

    virtual ::grpc::Status foo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message<void>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo23Raw(context, request, cq));
    }

    virtual ::grpc::Status foo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message<void>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>> Asyncfoo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>>(Asyncfoo24Raw(context, request, cq));
    }

    virtual ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo31Raw(context, request, cq));
    }

    virtual ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo32Raw(context, request, cq));
    }

    virtual ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo33Raw(context, request, cq));
    }

    virtual ::grpc::Status foo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::BasicTypes>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo34Raw(context, request, cq));
    }

    virtual ::grpc::Status foo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo41Raw(context, request, cq));
    }

    virtual ::grpc::Status foo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo42Raw(context, request, cq));
    }

    virtual ::grpc::Status foo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo43Raw(context, request, cq));
    }

    virtual ::grpc::Status foo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::dummy>* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>> Asyncfoo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo44Raw(context, request, cq));
    }

   private:
    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo21Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo22Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo23Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message<void>>* Asyncfoo24Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo34Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo41Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo42Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo43Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) = 0;

    virtual ::grpc::ClientAsyncResponseReaderInterface< ::bond::comm::message< ::tests::dummy>>* Asyncfoo44Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) = 0;

  };

  class Stub final : public StubInterface {
   public:
    Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);
    ::grpc::Status foo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo21(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo21Raw(context, request, cq));
    }

    ::grpc::Status foo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message<void>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo22(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo22Raw(context, request, cq));
    }

    ::grpc::Status foo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message<void>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo23(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo23Raw(context, request, cq));
    }

    ::grpc::Status foo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message<void>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>> Asyncfoo24(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>>(Asyncfoo24Raw(context, request, cq));
    }

    ::grpc::Status foo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo31(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo31Raw(context, request, cq));
    }

    ::grpc::Status foo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo32(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo32Raw(context, request, cq));
    }

    ::grpc::Status foo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo33(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo33Raw(context, request, cq));
    }

    ::grpc::Status foo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::BasicTypes>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>> Asyncfoo34(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>>(Asyncfoo34Raw(context, request, cq));
    }

    ::grpc::Status foo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo41(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo41Raw(context, request, cq));
    }

    ::grpc::Status foo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::bond::comm::message< ::tests::dummy>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo42(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo42Raw(context, request, cq));
    }

    ::grpc::Status foo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::bond::comm::message< ::tests::dummy>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo43(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo43Raw(context, request, cq));
    }

    ::grpc::Status foo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::bond::comm::message< ::tests::dummy>* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>> Asyncfoo44(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>>(Asyncfoo44Raw(context, request, cq));
    }

   private:
    std::shared_ptr< ::grpc::ChannelInterface> channel_;
    ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo21Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo21_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo22Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo22_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo23Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo23_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message<void>>* Asyncfoo24Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo24_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo31Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo31_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo32Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo32_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo33Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo33_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::BasicTypes>>* Asyncfoo34Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo34_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo41Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo41_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo42Raw(::grpc::ClientContext* context, const ::bond::comm::message<void>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo42_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo43Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::BasicTypes>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo43_;

    ::grpc::ClientAsyncResponseReader< ::bond::comm::message< ::tests::dummy>>* Asyncfoo44Raw(::grpc::ClientContext* context, const ::bond::comm::message< ::tests::dummy>& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_foo44_;

  };

  static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

  class Service : public ::grpc::Service {
   public:
    Service();
    virtual ~Service();
    virtual ::grpc::Status foo21(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<void>* response);

    virtual ::grpc::Status foo22(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<void>* response);

    virtual ::grpc::Status foo23(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message<void>* response);

    virtual ::grpc::Status foo24(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message<void>* response);

    virtual ::grpc::Status foo31(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::BasicTypes>* response);

    virtual ::grpc::Status foo32(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::BasicTypes>* response);

    virtual ::grpc::Status foo33(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message< ::tests::BasicTypes>* response);

    virtual ::grpc::Status foo34(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message< ::tests::BasicTypes>* response);

    virtual ::grpc::Status foo41(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::dummy>* response);

    virtual ::grpc::Status foo42(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::dummy>* response);

    virtual ::grpc::Status foo43(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message< ::tests::dummy>* response);

    virtual ::grpc::Status foo44(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message< ::tests::dummy>* response);
  };


  template <class BaseClass>
  class WithAsyncMethod_foo21 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo21() {
      ::grpc::Service::MarkMethodAsync(6);
    }
    ~WithAsyncMethod_foo21() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo21(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<void>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo21(::grpc::ServerContext* context, ::bond::comm::message<void>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message<void>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(6, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo22 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo22() {
      ::grpc::Service::MarkMethodAsync(7);
    }
    ~WithAsyncMethod_foo22() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo22(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message<void>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo22(::grpc::ServerContext* context, ::bond::comm::message<void>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message<void>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(7, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo23 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo23() {
      ::grpc::Service::MarkMethodAsync(8);
    }
    ~WithAsyncMethod_foo23() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo23(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message<void>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo23(::grpc::ServerContext* context, ::bond::comm::message< ::tests::BasicTypes>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message<void>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(8, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo24 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo24() {
      ::grpc::Service::MarkMethodAsync(9);
    }
    ~WithAsyncMethod_foo24() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo24(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message<void>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo24(::grpc::ServerContext* context, ::bond::comm::message< ::tests::dummy>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message<void>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(9, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo31 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo31() {
      ::grpc::Service::MarkMethodAsync(10);
    }
    ~WithAsyncMethod_foo31() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo31(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::BasicTypes>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo31(::grpc::ServerContext* context, ::bond::comm::message<void>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::BasicTypes>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(10, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo32 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo32() {
      ::grpc::Service::MarkMethodAsync(11);
    }
    ~WithAsyncMethod_foo32() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo32(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::BasicTypes>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo32(::grpc::ServerContext* context, ::bond::comm::message<void>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::BasicTypes>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(11, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo33 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo33() {
      ::grpc::Service::MarkMethodAsync(12);
    }
    ~WithAsyncMethod_foo33() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo33(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message< ::tests::BasicTypes>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo33(::grpc::ServerContext* context, ::bond::comm::message< ::tests::BasicTypes>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::BasicTypes>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(12, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo34 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo34() {
      ::grpc::Service::MarkMethodAsync(13);
    }
    ~WithAsyncMethod_foo34() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo34(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message< ::tests::BasicTypes>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo34(::grpc::ServerContext* context, ::bond::comm::message< ::tests::dummy>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::BasicTypes>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(13, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo41 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo41() {
      ::grpc::Service::MarkMethodAsync(14);
    }
    ~WithAsyncMethod_foo41() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo41(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::dummy>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo41(::grpc::ServerContext* context, ::bond::comm::message<void>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::dummy>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(14, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo42 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo42() {
      ::grpc::Service::MarkMethodAsync(15);
    }
    ~WithAsyncMethod_foo42() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo42(::grpc::ServerContext* context, const ::bond::comm::message<void>* request, ::bond::comm::message< ::tests::dummy>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo42(::grpc::ServerContext* context, ::bond::comm::message<void>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::dummy>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(15, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo43 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo43() {
      ::grpc::Service::MarkMethodAsync(16);
    }
    ~WithAsyncMethod_foo43() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo43(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::BasicTypes>* request, ::bond::comm::message< ::tests::dummy>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo43(::grpc::ServerContext* context, ::bond::comm::message< ::tests::BasicTypes>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::dummy>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(16, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  template <class BaseClass>
  class WithAsyncMethod_foo44 : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_foo44() {
      ::grpc::Service::MarkMethodAsync(17);
    }
    ~WithAsyncMethod_foo44() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status foo44(::grpc::ServerContext* context, const ::bond::comm::message< ::tests::dummy>* request, ::bond::comm::message< ::tests::dummy>* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Requestfoo44(::grpc::ServerContext* context, ::bond::comm::message< ::tests::dummy>* request, ::grpc::ServerAsyncResponseWriter< ::bond::comm::message< ::tests::dummy>>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(17, context, request, response, new_call_cq, notification_cq, tag);
    }
  };


  typedef WithAsyncMethod_foo11<WithAsyncMethod_foo12<WithAsyncMethod_foo12_impl<WithAsyncMethod_foo13<WithAsyncMethod_foo14<WithAsyncMethod_foo15<WithAsyncMethod_foo21<WithAsyncMethod_foo22<WithAsyncMethod_foo23<WithAsyncMethod_foo24<WithAsyncMethod_foo31<WithAsyncMethod_foo32<WithAsyncMethod_foo33<WithAsyncMethod_foo34<WithAsyncMethod_foo41<WithAsyncMethod_foo42<WithAsyncMethod_foo43<WithAsyncMethod_foo44<Service > > > > > > > > > > > > > > > > > > AsyncService;

};


} // namespace tests

#pragma warning (pop)
