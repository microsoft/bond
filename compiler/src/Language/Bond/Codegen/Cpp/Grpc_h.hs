-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_h (grpc_h) where

import System.FilePath
import Data.Maybe (isNothing)
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
#include <bond/ext/grpc/detail/client.h>
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
    includeImport (Import path) = [lt|#include "#{dropExtension (slashForward path)}_grpc.h"|]

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
            methodUses Function {methodInput = input} = isNothing (methodTypeToMaybe input)
            methodUses Event {} = True

    grpc s@Service{..} = [lt|
#{template}struct #{declName} final
{
    struct Schema
    {
        #{export_attr}static const ::bond::Metadata metadata;

        #{newlineSep 2 methodMetadata serviceMethods}

        public: struct service
        {
            #{doubleLineSep 3 methodTemplate serviceMethods}
        };

        private: typedef boost::mpl::list<> methods0;
        #{newlineSep 2 pushMethod indexedMethods}

        public: typedef #{typename}methods#{length serviceMethods}::type methods;

        #{constructor}
    };

    class #{proxyName} : public ::bond::ext::gRPC::detail::client
    {
    public:
        using ::bond::ext::gRPC::detail::client::client;

        #{doubleLineSep 2 publicProxyMethodDecl serviceMethods}

    private:
        #{newlineSep 2 privateProxyMethodDecl serviceMethods}
    };

    class #{serviceName} : public ::bond::ext::gRPC::detail::service
    {
    public:
        explicit #{serviceName}(const ::bond::ext::gRPC::Scheduler& scheduler = {})
            : ::bond::ext::gRPC::detail::service(
                scheduler,
                {
                    #{commaLineSep 5 serviceMethodName serviceMethods}
                })
        {}

        void start() override
        {
            _data.emplace(*this);
        }

        #{newlineSep 2 serviceVirtualMethod serviceMethods}

    private:
        struct data
        {
            explicit data(#{serviceName}& s)
                : _s(s)
            {}

            #{serviceName}& _s;
            #{newlineSep 3 serviceDataMember serviceMethodsWithIndex}
        };

        ::boost::optional<data> _data;
    };
};

#{onlyTemplate $ CPP.schemaMetadata cpp s}
|]
      where
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
                #{declName},
                #{bonded $ methodTypeToMaybe (methodInput m)},
                #{result m},
                &#{methodMetadataVar m}
            > #{methodName m};|]
          where
            result Event{} = "void"
            result Function{..} = bonded (methodTypeToMaybe methodResult)

        proxyName = "Client" :: String
        serviceName = "Service" :: String

        serviceMethodsWithIndex :: [(Integer,Method)]
        serviceMethodsWithIndex = zip [0..] serviceMethods

        publicProxyMethodDecl Function{methodInput = Void, ..} = [lt|void Async#{methodName}(const ::std::function<void(::bond::ext::gRPC::unary_call_result< #{payload (methodTypeToMaybe methodResult)}>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_m#{methodName}, #{bonded Nothing}{ ::bond::Void() }, std::move(context), cb);
        }|]
        publicProxyMethodDecl Function{..} = [lt|void Async#{methodName}(const #{bonded (methodTypeToMaybe methodInput)}& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< #{payload (methodTypeToMaybe methodResult)}>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch(_m#{methodName}, request, std::move(context), cb);
        }
        void Async#{methodName}(const #{payload (methodTypeToMaybe methodInput)}& request, const ::std::function<void(::bond::ext::gRPC::unary_call_result< #{payload (methodTypeToMaybe methodResult)}>)>& cb, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Async#{methodName}(#{bonded (methodTypeToMaybe methodInput)}{request}, cb, ::std::move(context));
        }|]
        publicProxyMethodDecl Event{methodInput = Void, ..} = [lt|void Async#{methodName}(::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch<#{bonded Nothing}>(_m#{methodName}, #{bonded Nothing}{ ::bond::Void() }, std::move(context));
        }|]
        publicProxyMethodDecl Event{..} = [lt|void Async#{methodName}(const #{bonded (methodTypeToMaybe methodInput)}& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            ::bond::ext::gRPC::detail::client::dispatch<#{bonded Nothing}>(_m#{methodName}, request, std::move(context));
        }
        void Async#{methodName}(const #{payload (methodTypeToMaybe methodInput)}& request, ::std::shared_ptr< ::grpc::ClientContext> context = {})
        {
            Async#{methodName}(#{bonded (methodTypeToMaybe methodInput)}{request}, ::std::move(context));
        }|]

        privateProxyMethodDecl f = [lt|const ::grpc::internal::RpcMethod _m#{methodName f}{ ::bond::ext::gRPC::detail::client::make_method("/#{getDeclTypeName idl s}/#{methodName f}") };|]

        serviceMethodName f = [lt|"/#{getDeclTypeName idl s}/#{methodName f}"|]

        serviceDataMember (index,f) = [lt|::bond::ext::gRPC::detail::service::Method<#{typename}Schema::service::#{methodName f}> _m#{index}{ _s, #{index}, ::std::bind(&#{serviceName}::#{methodName f}, &_s, ::std::placeholders::_1) };|]

        serviceVirtualMethod f = [lt|virtual void #{methodName f}(::bond::ext::gRPC::unary_call< #{bonded $ methodTypeToMaybe $ methodInput f}, #{payload $ result f}>) = 0;|]
          where
            result Function{..} = methodTypeToMaybe methodResult
            result Event{..} = Nothing

    grpc _ = mempty
