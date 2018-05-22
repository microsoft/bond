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
#include <bond/ext/grpc/reflection.h>
#include <bond/ext/grpc/detail/client.h>
#include <bond/ext/grpc/detail/service.h>

#include <boost/optional/optional.hpp>
#include <functional>
#include <memory>

#{CPP.openNamespace cpp}
#{doubleLineSep 1 grpc declarations}

#{CPP.closeNamespace cpp}

|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension (slashForward path)}_grpc.h"|]

    idl = MappingContext idlTypeMapping [] [] []

    cppType = getTypeName cpp

    payload = maybe "void" cppType

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
            #{newlineSep 3 methodTemplate serviceMethods}
        };

        private: typedef boost::mpl::list<> methods0;
        #{newlineSep 2 pushMethod indexedMethods}

        public: typedef #{typename}methods#{length serviceMethods}::type methods;

        #{constructor}
    };

    class #{proxyName} : public ::bond::ext::grpc::detail::client
    {
    public:
        using ::bond::ext::grpc::detail::client::client;

        #{doubleLineSep 2 publicProxyMethodDecl serviceMethods}

    private:
        #{newlineSep 2 privateProxyMethodDecl serviceMethods}
    };

    class #{serviceName} : public ::bond::ext::grpc::detail::service
    {
    public:
        explicit #{serviceName}(const ::bond::ext::grpc::Scheduler& scheduler)
            : ::bond::ext::grpc::detail::service(
                scheduler,
                {
                    #{commaLineSep 5 serviceMethodName serviceMethods}
                })
        {}

        #{newlineSep 2 serviceVirtualMethod serviceMethods}

    private:
        void start() override
        {
            _data.emplace(*this);
        }

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
        onlyNonTemplate x = if null declParams then x else mempty
        typename = onlyTemplate [lt|typename |]

        export_attr = onlyNonTemplate $ optional (\a -> [lt|#{a}
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

        methodTemplate m = [lt|typedef struct : ::bond::ext::grpc::reflection::MethodTemplate<#{declName}, #{payload $ methodTypeToMaybe (methodInput m)}, #{resultType m}, &#{methodMetadataVar m}> {} #{methodName m};|]

        proxyName = "Client" :: String
        serviceName = "Service" :: String

        serviceMethodsWithIndex :: [(Integer,Method)]
        serviceMethodsWithIndex = zip [0..] serviceMethods

        publicProxyMethodDecl Function{methodInput = Void, ..} = [lt|void Async#{methodName}(const ::std::function<void(::bond::ext::grpc::unary_call_result<#{payload (methodTypeToMaybe methodResult)}>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_m#{methodName}, std::move(context), cb);
        }
        ::std::future<::bond::ext::grpc::unary_call_result<#{payload (methodTypeToMaybe methodResult)}>> Async#{methodName}(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<#{payload (methodTypeToMaybe methodResult)}>(_m#{methodName}, std::move(context));
        }|]
        publicProxyMethodDecl Function{..} = [lt|void Async#{methodName}(const #{bonded (methodTypeToMaybe methodInput)}& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<#{payload (methodTypeToMaybe methodResult)}>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_m#{methodName}, std::move(context), cb, request);
        }
        std::future<::bond::ext::grpc::unary_call_result<#{payload (methodTypeToMaybe methodResult)}>> Async#{methodName}(const #{bonded (methodTypeToMaybe methodInput)}& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return ::bond::ext::grpc::detail::client::dispatch<#{payload (methodTypeToMaybe methodResult)}>(_m#{methodName}, std::move(context), request);
        }
        void Async#{methodName}(const #{payload (methodTypeToMaybe methodInput)}& request, const ::std::function<void(::bond::ext::grpc::unary_call_result<#{payload (methodTypeToMaybe methodResult)}>)>& cb, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            Async#{methodName}(#{bonded (methodTypeToMaybe methodInput)}{request}, cb, ::std::move(context));
        }
        ::std::future<::bond::ext::grpc::unary_call_result<#{payload (methodTypeToMaybe methodResult)}>> Async#{methodName}(const #{payload (methodTypeToMaybe methodInput)}& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            return Async#{methodName}(#{bonded (methodTypeToMaybe methodInput)}{request}, ::std::move(context));
        }|]
        publicProxyMethodDecl Event{methodInput = Void, ..} = [lt|void Async#{methodName}(::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_m#{methodName}, std::move(context), {});
        }|]
        publicProxyMethodDecl Event{..} = [lt|void Async#{methodName}(const #{bonded (methodTypeToMaybe methodInput)}& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            ::bond::ext::grpc::detail::client::dispatch(_m#{methodName}, std::move(context), {}, request);
        }
        void Async#{methodName}(const #{payload (methodTypeToMaybe methodInput)}& request, ::std::shared_ptr<::grpc::ClientContext> context = {})
        {
            Async#{methodName}(#{bonded (methodTypeToMaybe methodInput)}{request}, ::std::move(context));
        }|]

        privateProxyMethodDecl f = [lt|const ::bond::ext::grpc::detail::client::Method _m#{methodName f}{ ::bond::ext::grpc::detail::client::make_method("/#{getDeclTypeName idl s}/#{methodName f}") };|]

        serviceMethodName f = [lt|"/#{getDeclTypeName idl s}/#{methodName f}"|]

        serviceDataMember (index,f) = [lt|::bond::ext::grpc::detail::service::Method<#{typename}Schema::service::#{methodName f}> _m#{index}{ _s, #{index}, ::std::bind(&#{serviceName}::#{methodName f}, &_s, ::std::placeholders::_1) };|]

        serviceVirtualMethod f = [lt|virtual void #{methodName f}(::bond::ext::grpc::unary_call<#{payload $ methodTypeToMaybe $ methodInput f}, #{resultType f}>) = 0;|]

        resultType Function{..} = payload (methodTypeToMaybe methodResult)
        resultType Event{} = "::bond::reflection::nothing"

    grpc _ = mempty
