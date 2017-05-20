-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cpp.Grpc_cpp (grpc_cpp) where

import Data.Monoid
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Cpp.Util as CPP

-- | Codegen template for generating /base_name/_grpc.cpp containing
-- definitions of helper functions and schema metadata static variables.
grpc_cpp :: MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
grpc_cpp cpp file _imports declarations = ("_grpc.cpp", [lt|
#include "#{file}_grpc.h"

#{CPP.openNamespace cpp}
#{doubleLineSep 1 grpc declarations}
#{CPP.closeNamespace cpp}

|])
  where
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

    grpc s@Service {..} = [lt|
static const char* #{declName}_method_names[] =
{
    #{newlineSep 1 methodStrings serviceMethods}
};

#{declName}::#{declName}Client::#{declName}Client(const std::shared_ptr< ::grpc::ChannelInterface>& channel, std::shared_ptr< ::bond::ext::gRPC::io_manager> ioManager)
    : channel_(channel)
    , ioManager_(ioManager)
    #{newlineSep 1 proxyMethodMemberInit serviceMethodsWithIndex}
    { }

#{doubleLineSep 0 methodDecl serviceMethods}
|]
      where
        methodStrings Function{..} = [lt|"/#{getDeclTypeName idl s}/#{methodName}",|]
        methodStrings Event{..} = [lt|"/#{getDeclTypeName idl s}/#{methodName}",|]

        serviceMethodsWithIndex :: [(Integer,Method)]
        serviceMethodsWithIndex = zip [0..] serviceMethods

        proxyMethodMemberInit (index,Function{..}) = [lt|, rpcmethod_#{methodName}_(#{declName}_method_names[#{index}], ::grpc::RpcMethod::NORMAL_RPC, channel)|]
        proxyMethodMemberInit (_,Event{..}) = [lt|/* TODO stub ctor initialization for event #{methodName} */|]

        methodDecl Function{..} = [lt|void #{declName}::#{declName}Client::Async#{methodName}(::grpc::ClientContext* context, const #{request methodInput}& request, std::function<void(const #{response methodResult}&, const ::grpc::Status&)> cb)
{
    ::bond::ext::gRPC::detail::client_unary_call_data< #{request methodInput}, #{response methodResult} >* calldata = new ::bond::ext::gRPC::detail::client_unary_call_data< #{request methodInput}, #{response methodResult} >(cb);
    calldata->dispatch(channel_.get(), ioManager_.get(), rpcmethod_#{methodName}_, context, request);
}|]
        methodDecl Event{..} = [lt|/* TODO: stub implementation for event #{methodName} */|]

    grpc _ = mempty
