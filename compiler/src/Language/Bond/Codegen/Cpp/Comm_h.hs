-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Comm_h (comm_h) where

import System.FilePath
import Data.Monoid
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Language.Bond.Util
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping
import qualified Language.Bond.Codegen.Cpp.Util as CPP


-- | Codegen template for generating /base_name/_comm.h containing declarations of
-- of service interface and proxy.
comm_h :: Maybe String -> MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
comm_h export_attribute cpp file imports declarations = ("_comm.h", [lt|
#pragma once

#include <bond/comm/services.h>
#include "#{file}_types.h"
#{newlineSep 0 includeImport imports}

#{CPP.openNamespace cpp}
    #{doubleLineSep 1 comm declarations}

#{CPP.closeNamespace cpp}
|])
  where
    includeImport (Import path) = [lt|#include "#{dropExtension path}_comm.h"|]

    cppType = getTypeName cpp

    request mt = request' (payload mt)
      where
        payload = maybe "void" cppType
        request' params =  [lt|::bond::comm::payload<#{padLeft}#{params}>|]
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

    callback m = [lt|const std::function<void (const #{response m}&)>& callback|]

    comm s@Service {..} = [lt|#{template}class #{declName}
    {
    public:
        virtual ~#{declName}() = default;

        #{doubleLineSep 2 virtualMethod serviceMethods}

        struct Schema;
        class Proxy;

        template <template <typename> class Promise>
        class Using;
    };

    #{template}struct #{className}::Schema
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
    #{onlyTemplate $ CPP.schemaMetadata cpp s}

    #{template}class #{className}::Proxy
        : public #{className}
    {
    public:
        template <typename ServiceProxy>
        explicit
        Proxy(const ServiceProxy& proxy,
              const std::string& name = #{className}::Schema::metadata.qualified_name)
            : _impl(boost::make_shared<__Impl<ServiceProxy>>(proxy, name))
        {}

        explicit
        Proxy(const boost::shared_ptr<#{className}>& service)
            : _impl(service)
        {}

        Proxy() = default;

        #{doubleLineSep 2 proxyMethod serviceMethods}

        template <template <typename> class Promise>
        class Using;

    protected:
        boost::shared_ptr<#{className}> _impl;

        template <typename ServiceProxy>
        class __Impl
            : public #{className}
        {
        public:
            __Impl(const ServiceProxy& proxy, const std::string& name)
                : _proxy(proxy),
                  _name(name)
            {}

            virtual ~__Impl() = default;

            #{doubleLineSep 3 implMethod serviceMethods}

        private:
            ServiceProxy _proxy;
            const std::string _name;
        };
    };

    #{template}template <template <typename> class Promise>
    class #{className}::Using
        : public #{className}
    {
    public:
        #{doubleLineSep 2 virtualFutureMethod serviceMethods}

        #{doubleLineSep 2 serviceMethod serviceMethods}
    };

    #{template}template <template <typename> class Promise>
    class #{className}::Proxy::Using
        : public #{className}::Proxy
    {
    public:
        template <typename ServiceProxy>
        explicit
        Using(const ServiceProxy& proxy,
              const std::string& name = #{className}::Schema::metadata.qualified_name)
            : #{className}::Proxy(proxy, name)
        {}

        explicit
        Using(const boost::shared_ptr<#{className}>& service)
            : #{className}::Proxy(service)
        {}

        Using() = default;

        #{doubleLineSep 2 proxyFutureMethod serviceMethods}
    };
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
        constructor = onlyTemplate [lt|
            Schema()
            {
                // Force instantiation of template statics
                (void)metadata;
                #{newlineSep 4 static serviceMethods}
            }|]
          where
            static m = [lt|(void)#{methodMetadataVar m};|]

        methodTemplate m = [lt|typedef ::bond::reflection::MethodTemplate<
                #{className},
                #{request $ methodInput m},
                #{result m},
                &#{className}::#{methodName m},
                &#{methodMetadataVar m}
            > #{methodName m};|]
          where
            result Event{} = "void"
            result Function{..} = response methodResult

        methodSignature n m =
            [lt|void #{methodName m}(#{commaLineSep n id $ methodParams m})|]
          where
            methodParams Event{..} =
                [ [lt|const #{request methodInput}& input|]
                ]

            methodParams Function{..} =
                [ [lt|const #{request methodInput}& input|]
                , callback methodResult
                ]

        resultOf x f = [lt|decltype(std::declval< #{x}>().#{f}())|]

        promiseType result = [lt|Promise< #{response result}>|]

        futureType result = resultOf (promiseType result) get_future
          where
            get_future = [lt|get_future|]

        methodFutureSignature Function{..} =
            [lt|auto #{methodName}(const #{request methodInput}& input)
            -> #{futureType methodResult}|]
        methodFutureSignature Event{..} = error "No future-based signature for Event methods"

        virtualMethod m = [lt|virtual #{methodSignature 3 m} = 0;|]

        virtualFutureMethod Event{} = mempty
        virtualFutureMethod m = [lt|virtual #{methodFutureSignature m} = 0;|]

        serviceMethod Event{} = mempty
        serviceMethod m@Function{..} = [lt|#{methodSignature 3 m} override
        {
            when(#{methodName}(input), ::bond::comm::Continuation(callback));
        }|]

        proxyMethod m@Event{..} = [lt|#{methodSignature 3 m} override
        {
            _impl->#{methodName}(input);
        }#{proxyMethodOverload methodInput}|]
          where
            proxyMethodOverload Nothing = [lt|

        void #{methodName}()
        {
            _impl->#{methodName}(::bond::comm::payload<void>());
        }|]
            proxyMethodOverload (Just payload) | isStruct payload = [lt|

        void #{methodName}(const #{cppType payload}& input)
        {
            _impl->#{methodName}(boost::cref(input));
        }|]
            proxyMethodOverload _ = mempty

        proxyMethod m@Function{..} = [lt|#{methodSignature 3 m} override
        {
            _impl->#{methodName}(input, callback);
        }#{proxyMethodOverload methodInput}|]
          where
            proxyMethodOverload Nothing = [lt|

        void #{methodName}(
            #{callback methodResult})
        {
            _impl->#{methodName}(::bond::comm::payload<void>(), callback);
        }|]
            proxyMethodOverload (Just payload) | isStruct payload = [lt|

        void #{methodName}(const #{cppType payload}& input,
            #{callback methodResult})
        {
            _impl->#{methodName}(boost::cref(input), callback);
        }|]
            proxyMethodOverload _ = mempty

        proxyFutureMethod Event{} = mempty
        proxyFutureMethod m@Function{..} = [lt|using #{className}::Proxy::#{methodName};

        #{methodFutureSignature m}
        {
            auto promise = boost::make_shared<#{promiseType methodResult}>();

            _impl->#{methodName}(input,
                [=](const #{response methodResult}& result) mutable
                {
                    promise->set_value(result);
                });

            return promise->get_future();
        }#{proxyMethodOverload methodInput}|]
          where
            proxyMethodOverload Nothing = [lt|

        auto #{methodName}()
            -> #{futureType methodResult}
        {
            return #{methodName}(::bond::comm::payload<void>());
        }|]
            proxyMethodOverload (Just payload) | isStruct payload = [lt|

        auto #{methodName}(const #{cppType payload}& input)
            -> #{futureType methodResult}
        {
            return #{methodName}(#{request methodInput}(boost::cref(input)));
        }
        |]
            proxyMethodOverload _ = mempty

        implMethod m@Event{..} = [lt|#{methodSignature 4 m} override
            {
                _proxy.Send(_name, Schema::service::#{methodName}::metadata.name, input);
            }|]

        implMethod m@Function{..} = [lt|#{methodSignature 4 m} override
            {
                _proxy.Send(_name, Schema::service::#{methodName}::metadata.name, input, callback);
            }|]

    comm _ = mempty
