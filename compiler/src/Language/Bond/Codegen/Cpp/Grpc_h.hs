-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_h (grpc_h) where

import System.FilePath
import Data.Maybe(isNothing)
import Data.Monoid
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Language.Bond.Util
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping
import qualified Language.Bond.Codegen.Cpp.Util as CPP


-- | Codegen template for generating /base_name/_grpc.h containing declarations of
-- of service interface and proxy.
grpc_h :: Maybe String -> MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
grpc_h export_attribute cpp file imports declarations = ("_grpc.h", [lt|
#pragma once

#include "#{file}_reflection.h"
#include "#{file}_types.h"
#{newlineSep 0 includeImport imports}
#{includeBondReflection}
#include <bond/core/bonded.h>
#include <bond/ext/grpc/bond_utils.h>
#include <bond/ext/grpc/client_callback.h>
#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/reflection.h>
#include <bond/ext/grpc/thread_pool.h>
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

#include <grpcpp/impl/codegen/channel_interface.h>
#include <grpcpp/impl/codegen/client_context.h>
#include <grpcpp/impl/codegen/completion_queue.h>
#include <grpcpp/impl/codegen/rpc_method.h>
#include <grpcpp/impl/codegen/status.h>

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

    payload = maybe "::bond::Void" cppType

    bonded mt = bonded' (payload mt)
      where
        bonded' params =  [lt|::bond::bonded<#{padLeft}#{params}>|]
          where
            paramsText = toLazyText params
            padLeft = if L.head paramsText == ':' then [lt| |] else mempty

    includeBondReflection =
      if usesBondVoid then [lt|#include <bond/core/bond_reflection.h>|] else mempty
      where usesBondVoid = any declUses declarations
            declUses Service {serviceMethods = methods} = any methodUses methods
            declUses _ = False
            methodUses Function {methodInput = input} = isNothing input
            methodUses Event {} = True

    grpc s@Service{..} = [lt|
#{template}class #{declName} final
{
public:
    struct Schema;

    template <typename TThreadPool>
    class #{proxyName}
    {
    public:
        #{proxyName}(
            const std::shared_ptr< ::grpc::ChannelInterface>& channel,
            std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
            std::shared_ptr<TThreadPool> threadPool);

        #{doubleLineSep 2 publicProxyMethodDecl serviceMethods}

        #{proxyName}(const #{proxyName}&) = delete;
        #{proxyName}& operator=(const #{proxyName}&) = delete;

        #{proxyName}(#{proxyName}&&) = default;
        #{proxyName}& operator=(#{proxyName}&&) = default;

    private:
        std::shared_ptr< ::grpc::ChannelInterface> _channel;
        std::shared_ptr< ::bond::ext::gRPC::io_manager> _ioManager;
        std::shared_ptr<TThreadPool> _threadPool;

        #{doubleLineSep 2 privateProxyMethodDecl serviceMethods}
    };

    using Client = #{proxyName}< ::bond::ext::gRPC::thread_pool>;

    template <typename TThreadPool>
    class #{serviceName} : public ::bond::ext::gRPC::detail::service<TThreadPool>
    {
    public:
        #{serviceName}()
        {
            #{newlineSep 3 serviceAddMethod serviceMethods}
        }

        virtual ~#{serviceName}() { }
        #{serviceStartMethod}

        #{newlineSep 2 serviceVirtualMethod serviceMethods}

    private:
        #{newlineSep 2 serviceMethodReceiveData serviceMethods}
    };

    using Service = #{serviceName}< ::bond::ext::gRPC::thread_pool>;
};

#{template}template <typename TThreadPool>
inline #{className}::#{proxyName}<TThreadPool>::#{proxyName}(
    const std::shared_ptr< ::grpc::ChannelInterface>& channel,
    std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager,
    std::shared_ptr<TThreadPool> threadPool)
    : _channel(channel)
    , _ioManager(ioManager)
    , _threadPool(threadPool)
    #{newlineSep 1 proxyMethodMemberInit serviceMethods}
    { }

#{doubleLineSep 0 methodDecl serviceMethods}

#{template}struct #{className}::Schema
{
    #{export_attr}static const ::bond::Metadata metadata;

    #{newlineSep 1 methodMetadata serviceMethods}

    public: struct service
    {
        #{doubleLineSep 2 methodTemplate serviceMethods}
    };

    private: typedef boost::mpl::list<> methods0;
    #{newlineSep 1 pushMethod indexedMethods}

    public: typedef #{typename}methods#{length serviceMethods}::type methods;

    #{constructor}
};
#{onlyTemplate $ CPP.schemaMetadata cpp s}
|]
      where
        className = CPP.className s
        template = CPP.template s
        onlyTemplate x = if null declParams then mempty else x
        typename = onlyTemplate [lt|typename |]

        export_attr = optional (\a -> [lt|#{a}
        |]) export_attribute

        methodMetadataVar m = [lt|s_#{methodName m}_metadata|]

        methodMetadata m =
            [lt|private: #{export_attr}static const ::bond::Metadata #{methodMetadataVar m};|]

        -- reversed list of method names zipped with indexes
        indexedMethods :: [(String, Int)]
        indexedMethods = zipWith ((,) . methodName) (reverse serviceMethods) [0..]

        pushMethod (method, i) =
            [lt|private: typedef #{typename}boost::mpl::push_front<methods#{i}, #{typename}service::#{method}>::type methods#{i + 1};|]

        -- constructor, generated only for service templates
        constructor = onlyTemplate [lt|Schema()
        {
            // Force instantiation of template statics
            (void)metadata;
            #{newlineSep 3 static serviceMethods}
        }|]
          where
            static m = [lt|(void)#{methodMetadataVar m};|]

        methodTemplate m = [lt|typedef ::bond::ext::gRPC::reflection::MethodTemplate<
                #{className},
                #{bonded $ methodInput m},
                #{result m},
                &#{methodMetadataVar m}
            > #{methodName m};|]
          where
            result Event{} = "void"
            result Function{..} = bonded methodResult

        proxyName = "ClientCore" :: String
        serviceName = "ServiceCore" :: String

        methodNames :: [String]
        methodNames = map methodName serviceMethods

        serviceMethodsWithIndex :: [(Integer,Method)]
        serviceMethodsWithIndex = zip [0..] serviceMethods

        publicProxyMethodDecl Function{methodInput = Nothing, ..} = [lt|void Async#{methodName}(::std::shared_ptr< ::grpc::ClientContext> context, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< #{payload methodResult}>>)>& cb);
        void Async#{methodName}(const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< #{payload methodResult}>>)>& cb)
        {
            Async#{methodName}(::std::make_shared< ::grpc::ClientContext>(), cb);
        }|]
        publicProxyMethodDecl Function{..} = [lt|void Async#{methodName}(::std::shared_ptr< ::grpc::ClientContext> context, const #{bonded methodInput}& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< #{payload methodResult}>>)>& cb);
        void Async#{methodName}(::std::shared_ptr< ::grpc::ClientContext> context, const #{payload methodInput}& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< #{payload methodResult}>>)>& cb)
        {
            Async#{methodName}(context, #{bonded methodInput}{request}, cb);
        }
        void Async#{methodName}(const #{bonded methodInput}& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< #{payload methodResult}>>)>& cb)
        {
            Async#{methodName}(::std::make_shared< ::grpc::ClientContext>(), request, cb);
        }
        void Async#{methodName}(const #{payload methodInput}& request, const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< #{payload methodResult}>>)>& cb)
        {
            Async#{methodName}(::std::make_shared< ::grpc::ClientContext>(), #{bonded methodInput}{request}, cb);
        }|]
        publicProxyMethodDecl Event{methodInput = Nothing, ..} = [lt|void Async#{methodName}(::std::shared_ptr< ::grpc::ClientContext> context);
        void Async#{methodName}()
        {
            Async#{methodName}(::std::make_shared< ::grpc::ClientContext>());
        }|]
        publicProxyMethodDecl Event{..} = [lt|void Async#{methodName}(::std::shared_ptr< ::grpc::ClientContext> context, const #{bonded methodInput}& request);
        void Async#{methodName}(::std::shared_ptr< ::grpc::ClientContext> context, const #{payload methodInput}& request)
        {
            Async#{methodName}(context, #{bonded methodInput}{request});
        }
        void Async#{methodName}(const #{bonded methodInput}& request)
        {
            Async#{methodName}(::std::make_shared< ::grpc::ClientContext>(), request);
        }
        void Async#{methodName}(const #{payload methodInput}& request)
        {
            Async#{methodName}(::std::make_shared< ::grpc::ClientContext>(), #{bonded methodInput}{request});
        }|]

        privateProxyMethodDecl Function{..} = [lt|const ::grpc::internal::RpcMethod rpcmethod_#{methodName}_;|]
        privateProxyMethodDecl Event{..} = [lt|const ::grpc::internal::RpcMethod rpcmethod_#{methodName}_;|]

        proxyMethodMemberInit Function{..} = [lt|, rpcmethod_#{methodName}_("/#{getDeclTypeName idl s}/#{methodName}", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)|]
        proxyMethodMemberInit Event{..} = [lt|, rpcmethod_#{methodName}_("/#{getDeclTypeName idl s}/#{methodName}", ::grpc::internal::RpcMethod::NORMAL_RPC, channel)|]

        methodDecl Function{..} = [lt|#{template}template <typename TThreadPool>
inline void #{className}::#{proxyName}<TThreadPool>::Async#{methodName}(
    ::std::shared_ptr< ::grpc::ClientContext> context,
    #{voidParam methodInput}
    const std::function<void(std::shared_ptr< ::bond::ext::gRPC::unary_call_result< #{payload methodResult}>>)>& cb)
{
    #{voidRequest methodInput}
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< #{payload methodInput}, #{payload methodResult}, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context,
        cb);
    calldata->dispatch(rpcmethod_#{methodName}_, request);
}|]
          where
            voidRequest Nothing = [lt|auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};|]
            voidRequest _ = mempty
            voidParam Nothing = mempty
            voidParam _ = [lt|const #{bonded methodInput}& request,|]

        methodDecl Event{..} = [lt|#{template}template <typename TThreadPool>
inline void #{className}::#{proxyName}<TThreadPool>::Async#{methodName}(
    ::std::shared_ptr< ::grpc::ClientContext> context
    #{voidParam methodInput})
{
    #{voidRequest methodInput}
    auto calldata = std::make_shared< ::bond::ext::gRPC::detail::client_unary_call_data< #{payload methodInput}, #{payload Nothing}, TThreadPool>>(
        _channel,
        _ioManager,
        _threadPool,
        context);
    calldata->dispatch(rpcmethod_#{methodName}_, request);
}|]
          where
            voidRequest Nothing = [lt|auto request = ::bond::bonded< ::bond::Void>{ ::bond::Void()};|]
            voidRequest _ = mempty
            voidParam Nothing = mempty
            voidParam _ = [lt|, const #{bonded methodInput}& request|]

        serviceAddMethod Function{..} = [lt|this->AddMethod("/#{getDeclTypeName idl s}/#{methodName}");|]
        serviceAddMethod Event{..} = [lt|this->AddMethod("/#{getDeclTypeName idl s}/#{methodName}");|]

        serviceStartMethod = [lt|virtual void start(
            ::grpc::ServerCompletionQueue* #{cqParam},
            std::shared_ptr<TThreadPool> #{tpParam}) override
        {
            BOOST_ASSERT(#{cqParam});
            BOOST_ASSERT(#{tpParam});

            #{newlineSep 3 initMethodReceiveData serviceMethodsWithIndex}

            #{newlineSep 3 queueReceive serviceMethodsWithIndex}
        }|]
            where cqParam = uniqueName "cq" methodNames
                  tpParam = uniqueName "tp" methodNames
                  initMethodReceiveData (index,Function{..}) = [lt|#{serviceRdMember methodName}.emplace(
                this,
                #{index},
                #{cqParam},
                #{tpParam},
                std::bind(&#{serviceName}::#{methodName}, this, std::placeholders::_1));|]
                  initMethodReceiveData (index,Event{..}) = [lt|#{serviceRdMember methodName}.emplace(
                this,
                #{index},
                #{cqParam},
                #{tpParam},
                std::bind(&#{serviceName}::#{methodName}, this, std::placeholders::_1));|]
                  queueReceive (index,Function{..}) = [lt|this->queue_receive(
                #{index},
                &#{serviceRdMember methodName}->_receivedCall->context(),
                &#{serviceRdMember methodName}->_receivedCall->request(),
                &#{serviceRdMember methodName}->_receivedCall->responder(),
                #{cqParam},
                &#{serviceRdMember methodName}.get());|]
                  queueReceive (index,Event{..}) = [lt|this->queue_receive(
                #{index},
                &#{serviceRdMember methodName}->_receivedCall->context(),
                &#{serviceRdMember methodName}->_receivedCall->request(),
                &#{serviceRdMember methodName}->_receivedCall->responder(),
                #{cqParam},
                &#{serviceRdMember methodName}.get());|]

        serviceMethodReceiveData Function{..} = [lt|::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< #{bonded methodInput}, #{payload methodResult}, TThreadPool>> #{serviceRdMember methodName};|]
        serviceMethodReceiveData Event{..} = [lt|::boost::optional< ::bond::ext::gRPC::detail::service_unary_call_data< #{bonded methodInput}, #{payload Nothing}, TThreadPool>> #{serviceRdMember methodName};|]

        serviceVirtualMethod Function{..} = [lt|virtual void #{methodName}(::bond::ext::gRPC::unary_call< #{bonded methodInput}, #{payload methodResult}>) = 0;|]
        serviceVirtualMethod Event{..} = [lt|virtual void #{methodName}(::bond::ext::gRPC::unary_call< #{bonded methodInput}, #{payload Nothing}>) = 0;|]

        serviceRdMember methodName = uniqueName ("_rd_" ++ methodName) methodNames

    grpc _ = mempty
