-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_h (grpc_h) where

import System.FilePath
import Data.List (elemIndex)
import Data.Monoid
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
-- import Language.Bond.Util
import Language.Bond.Syntax.Types
-- import Language.Bond.Syntax.Util
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping
import qualified Language.Bond.Codegen.Cpp.Util as CPP


-- | Codegen template for generating /base_name/_grpc.h containing declarations of
-- of service interface and proxy.
grpc_h :: Maybe String -> MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
grpc_h _ cpp file imports declarations = ("_grpc.h", [lt|
#pragma once

#include "#{file}_reflection.h"
#include "#{file}_types.h"
#include <bond/comm/message.h>
#{newlineSep 0 includeImport imports}

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

#{CPP.openNamespace cpp}
#{doubleLineSep 1 grpc declarations}

#{CPP.closeNamespace cpp}

#pragma warning (pop)
|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension path}_grpc.h"|]

    cppType = getTypeName cpp

    request mt = request' (payload mt)
      where
        payload = maybe "void" cppType
        request' params =  [lt|::bond::comm::message<#{padLeft}#{params}>|]
          where
            paramsText = toLazyText params
            padLeft = if L.head paramsText == ':' then [lt| |] else mempty

    response mt = response' (payload mt)
      where
        payload = maybe "void" cppType
        response' params =  [lt|::bond::comm::message<#{padLeft}#{params}>|]
          where
            paramsText = toLazyText params
            padLeft = if L.head paramsText == ':' then [lt| |] else mempty

    grpc Service {..} = [lt|
class #{declName} final {
 public:
  class StubInterface {
   public:
    virtual ~StubInterface() {}
    #{newlineSep 1 publicInterfaceMethodDecl serviceMethods}
   private:
    #{newlineSep 1 privateInterfaceMethodDecl serviceMethods}
  };

  class Stub final : public StubInterface {
   public:
    Stub(const std::shared_ptr< ::grpc::ChannelInterface>& channel);
    #{newlineSep 1 publicStubMethodDecl serviceMethods}
   private:
    std::shared_ptr< ::grpc::ChannelInterface> channel_;
    #{newlineSep 1 privateStubMethodDecl serviceMethods}
  };

  static std::unique_ptr<Stub> NewStub(const std::shared_ptr< ::grpc::ChannelInterface>& channel, const ::grpc::StubOptions& options = ::grpc::StubOptions());

  class Service : public ::grpc::Service {
   public:
    Service();
    virtual ~Service();
    #{doubleLineSep 1 virtualServiceMethodDecl serviceMethods}
  };

#{newlineSep 0 baseClassMethodDecl serviceMethods}

  #{asyncServiceDef serviceMethods}

};
|]
      where
        publicInterfaceMethodDecl Function{..} = [lt|virtual ::grpc::Status #{methodName}(::grpc::ClientContext* context, const #{request methodInput}& request, #{response methodResult}* response) = 0;
    std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< #{response methodResult}>> Async#{methodName}(::grpc::ClientContext* context, const #{request methodInput}& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReaderInterface< #{response methodResult}>>(Async#{methodName}Raw(context, request, cq));
    }
|]
        publicInterfaceMethodDecl Event{..} = [lt||]

        privateInterfaceMethodDecl Function{..} = [lt|virtual ::grpc::ClientAsyncResponseReaderInterface< #{response methodResult}>* Async#{methodName}Raw(::grpc::ClientContext* context, const #{request methodInput}& request, ::grpc::CompletionQueue* cq) = 0;
|]
        privateInterfaceMethodDecl Event{..} = [lt||]

        publicStubMethodDecl Function{..} = [lt|::grpc::Status #{methodName}(::grpc::ClientContext* context, const #{request methodInput}& request, #{response methodResult}* response) override;
    std::unique_ptr< ::grpc::ClientAsyncResponseReader< #{response methodResult}>> Async#{methodName}(::grpc::ClientContext* context, const #{request methodInput}& request, ::grpc::CompletionQueue* cq) {
      return std::unique_ptr< ::grpc::ClientAsyncResponseReader< #{response methodResult}>>(Async#{methodName}Raw(context, request, cq));
    }
|]
        publicStubMethodDecl Event{..} = [lt||]

        privateStubMethodDecl Function{..} = [lt|::grpc::ClientAsyncResponseReader< #{response methodResult}>* Async#{methodName}Raw(::grpc::ClientContext* context, const #{request methodInput}& request, ::grpc::CompletionQueue* cq) override;
    const ::grpc::RpcMethod rpcmethod_#{methodName}_;
|]
        privateStubMethodDecl Event{..} = [lt||]

        virtualServiceMethodDecl Function{..} = [lt|virtual ::grpc::Status #{methodName}(::grpc::ServerContext* context, const #{request methodInput}* request, #{response methodResult}* response);|]
        virtualServiceMethodDecl Event{..} = [lt||]

        baseClassMethodDecl f@Function{..} = [lt|
  template <class BaseClass>
  class WithAsyncMethod_#{methodName} : public BaseClass {
   private:
    void BaseClassMustBeDerivedFromService(const Service *service) {}
   public:
    WithAsyncMethod_#{methodName}() {
      ::grpc::Service::MarkMethodAsync(#{index});
    }
    ~WithAsyncMethod_#{methodName}() override {
      BaseClassMustBeDerivedFromService(this);
    }
    // disable synchronous version of this method
    ::grpc::Status #{methodName}(::grpc::ServerContext* context, const #{request methodInput}* request, #{response methodResult}* response) final override {
      abort();
      return ::grpc::Status(::grpc::StatusCode::UNIMPLEMENTED, "");
    }
    void Request#{methodName}(::grpc::ServerContext* context, #{request methodInput}* request, ::grpc::ServerAsyncResponseWriter< #{response methodResult}>* response, ::grpc::CompletionQueue* new_call_cq, ::grpc::ServerCompletionQueue* notification_cq, void *tag) {
      ::grpc::Service::RequestAsyncUnary(#{index}, context, request, response, new_call_cq, notification_cq, tag);
    }
  };
|]
          where
            index = maybe (-1) id (elemIndex f serviceMethods)

        baseClassMethodDecl Event{..} = [lt||]

        asyncMethodChain [] = ""
        asyncMethodChain [Function{..}] = "WithAsyncMethod_" ++ methodName ++ "<Service >"
        asyncMethodChain [Event{..}] = "WithAsyncMethod_" ++ methodName ++ "<Service >"
        asyncMethodChain (Function{..}:xs) = "WithAsyncMethod_" ++ methodName ++ "<" ++ asyncMethodChain xs ++ " >"
        asyncMethodChain (Event{..}:xs) = "WithAsyncMethod_" ++ methodName ++ "<" ++ asyncMethodChain xs ++ " >"

        asyncServiceDef [] = mempty
        asyncServiceDef m = [lt|typedef #{asyncMethodChain m} AsyncService;|]

    grpc _ = mempty
