-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_h (grpc_h) where

import System.FilePath
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
#{newlineSep 0 includeImport imports}
// todo: remove message
#include <bond/comm/message.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/detail/client_call_data.h>
#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/detail/service_call_data.h>

#include <boost/optional/optional.hpp>
#include <functional>
#include <memory>

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100 4267)
#endif

#include <grpc++/impl/codegen/channel_interface.h>
#include <grpc++/impl/codegen/client_context.h>
#include <grpc++/impl/codegen/completion_queue.h>
#include <grpc++/impl/codegen/rpc_method.h>
#include <grpc++/impl/codegen/status.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

#{CPP.openNamespace cpp}
#{doubleLineSep 1 grpc declarations}

#{CPP.closeNamespace cpp}

|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension path}_grpc.h"|]

    idl = MappingContext idlTypeMapping [] [] []

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

    grpc s@Service{..} = [lt|
class #{declName} final
{
public:
    class #{proxyName}
    {
    public:
        #{proxyName}(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager);

        #{doubleLineSep 2 publicStubMethodDecl serviceMethods}

        #{proxyName}(const #{proxyName}&) = delete;
        #{proxyName}& operator=(const #{proxyName}&) = delete;

        #{proxyName}(#{proxyName}&&) = default;
        #{proxyName}& operator=(#{proxyName}&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> channel_;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager_;

        #{doubleLineSep 2 privateStubMethodDecl serviceMethods}
    };

    class Service : public ::bond::ext::gRPC::detail::service
    {
    public:
        Service()
        {
            #{newlineSep 3 serviceAddMethod serviceMethods}
        }

        virtual ~Service() { }
        #{serviceStartMethod}

        #{newlineSep 2 serviceVirtualMethod serviceMethods}

    private:
        #{newlineSep 2 serviceMethodReceiveData serviceMethods}
    };
};|]
      where
        methodNames :: [String]
        methodNames = map methodName serviceMethods

        proxyName = declName ++ "Client"

        serviceMethodsWithIndex :: [(Integer,Method)]
        serviceMethodsWithIndex = zip [0..] serviceMethods

        publicStubMethodDecl Function{..} = [lt|void Async#{methodName}(::grpc::ClientContext* context, const #{request methodInput}& request, std::function<void(const #{response methodResult}&, const ::grpc::Status&)> cb);|]
        publicStubMethodDecl Event{..} = [lt|/* TODO stub implementation (public) for event #{methodName} */|]

        privateStubMethodDecl Function{..} = [lt|const ::grpc::RpcMethod rpcmethod_#{methodName}_;|]
        privateStubMethodDecl Event{..} = [lt|/* TODO stub implementation (private) for event #{methodName} */|]

        serviceAddMethod Function{..} = [lt|AddMethod("/#{getDeclTypeName idl s}/#{methodName}");|]
        serviceAddMethod Event{..} = [lt|AddMethod("/#{getDeclTypeName idl s}/#{methodName}");|]

        serviceStartMethod = [lt|virtual void start(::grpc::ServerCompletionQueue* #{cqParam}) override
        {
            BOOST_ASSERT(#{cqParam});

            #{newlineSep 3 initMethodReceiveData serviceMethodsWithIndex}

            #{newlineSep 3 queueReceive serviceMethodsWithIndex}
        }|]
            where cqParam = uniqueName "cq" methodNames
                  initMethodReceiveData (index,Function{..}) = [lt|#{serviceRdMember methodName}.emplace(this, #{index}, #{cqParam}, std::bind(&Service::#{methodName}, this, std::placeholders::_1));|]
                  initMethodReceiveData (_,Event{..}) = [lt|/* TODO: init for event #{methodName} */|]
                  queueReceive (index,Function{..}) = [lt|queue_receive(#{index}, &#{serviceRdMember methodName}->_receivedCall->_context, &#{serviceRdMember methodName}->_receivedCall->_request, &#{serviceRdMember methodName}->_receivedCall->_responder, #{cqParam}, &#{serviceRdMember methodName}.get());|]
                  queueReceive (_,Event{..}) = [lt|/* TODO: queue event #{methodName} */|]

        serviceMethodReceiveData Function{..} = [lt|boost::optional<::bond::ext::gRPC::detail::service_unary_call_data<#{request methodInput}, #{response methodResult}>> #{serviceRdMember methodName};|]
        serviceMethodReceiveData Event{..} = [lt|/* TODO: receive data for event #{methodName} */|]

        serviceVirtualMethod Function{..} = [lt|virtual void #{methodName}(::bond::ext::gRPC::unary_call<#{request methodInput}, #{response methodResult}>) = 0;|]
        serviceVirtualMethod Event{..} = [lt|/* TODO: abstract method for event #{methodName} */|]

        serviceRdMember methodName = uniqueName ("_rd_" ++ methodName) methodNames

    grpc _ = mempty
