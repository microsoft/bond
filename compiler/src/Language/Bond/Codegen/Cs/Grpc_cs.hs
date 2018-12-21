-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards #-}

module Language.Bond.Codegen.Cs.Grpc_cs (
  grpc_cs)
where

import Data.Monoid
import qualified Data.Text.Lazy as L
import qualified Language.Bond.Codegen.Cs.Util as CS
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Syntax.Types hiding (methodTypeToMaybe)
import Language.Bond.Util
import Prelude
import Text.Shakespeare.Text

-- | Represents a parameter in an abstract server method.
--
-- This data definition would be in the where clause of grpc_cs if data
-- definitions were permitted in where clauses.
data CsServerMethodParam = CsServerMethodParam { name :: L.Text, csType :: L.Text }

-- | Generates a C# parameter declaration for a server method parameter.
csServerParamDecl :: CsServerMethodParam -> L.Text
csServerParamDecl (CsServerMethodParam {..}) = [lt|#{csType} #{name}|]

-- | Contains splicable text for a client method's first parameter. Create
-- one of these with the function csClientRequestParam. Some kinds of
-- methods don't need a first param, so some of the record's fields may be
-- empty. If non-empty, the correct comma and space are included so this
-- text can just be spliced in.
--
-- This data definition would be in the where clause of grpc_cs if data
-- definitions were permitted in where clauses.
data CsClientRequestParam = CsClientRequestParam
  { bareParam :: L.Text -- bare param declaration: what the caller passes in, if applicable
  , messageParam :: L.Text -- param declaration wrapped in an IMessage
  , toMessage :: L.Text -- code to create an IMessage for the request
  , forwardMessageParam :: L.Text -- code to forward the IMessage param on to the CallInvoker
  }

-- | Codegen template for generating code containing declarations of
-- of services including proxies and services files for gRPC integration.
grpc_cs :: MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
grpc_cs cs _ _ declarations = ("_grpc.cs", [lt|
#{CS.disableCscWarnings}
#{CS.disableReSharperWarnings}

namespace #{csNamespace}
{
    using System.Collections.Generic;

    #{doubleLineSep 1 grpc declarations}
} // #{csNamespace}
|])
  where
    csNamespace = sepBy "." toText $ getNamespace cs
    idl = MappingContext idlTypeMapping [] [] []

    grpc s@Service{..} = [lt|#{CS.typeAttributes cs s}public static class #{declName}#{generics} #{genericsWhere}
    {
        static readonly string ServiceName = "#{getDeclTypeName idl s}";

        #{doubleLineSep 2 methodDeclaration serviceMethods}

        public abstract class #{declName}Base
        {
            #{doubleLineSep 3 serverBaseMethods serviceMethods}
        }

        public class #{declName}Client : global::Grpc.Core.ClientBase<#{declName}Client>
        {
            public #{declName}Client(global::Grpc.Core.Channel channel) : base(channel)
            {
            }

            public #{declName}Client(global::Grpc.Core.CallInvoker callInvoker) : base(callInvoker)
            {
            }

            protected #{declName}Client() : base()
            {
            }

            protected #{declName}Client(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration) : base(configuration)
            {
            }

            #{doubleLineSep 3 clientMethods serviceMethods}

            protected override #{declName}Client NewInstance(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration)
            {
                return new #{declName}Client(configuration);
            }
        }

        public static global::Grpc.Core.ServerServiceDefinition BindService(#{declName}Base serviceImpl)
        {
            return global::Grpc.Core.ServerServiceDefinition.CreateBuilder()
                    #{newlineSep 5 serviceMethodBuilder serviceMethods}
                    .Build();
        }
    }
|]
      where
        methodNames = map methodName serviceMethods

        uniqImplName name = uniqueName (name ++ "_impl") methodNames

        csTypeOf Void = "global::Bond.Void"
        csTypeOf (Unary t) = getTypeName cs t
        csTypeOf (Streaming t) = getTypeName cs t

        csRequestTypeName (Function {..}) = csTypeOf methodInput
        csRequestTypeName (Event {..}) = csTypeOf methodInput

        csResponseTypeName (Function {..}) = csTypeOf methodResult
        csResponseTypeName Event{} = csTypeOf Void
      
        messageVoid = [lt|global::Bond.Grpc.IMessage<global::Bond.Void>|]

        messageOf :: Type -> L.Text
        messageOf t = [lt|global::Bond.Grpc.IMessage<#{getTypeName cs t}>|]

        methodTypeEnum :: Method -> L.Text
        methodTypeEnum (Function {..}) = case (methodResult, methodInput) of
            (Streaming _, Streaming _) -> [lt|global::Grpc.Core.MethodType.DuplexStreaming|]
            (Streaming _, _)           -> [lt|global::Grpc.Core.MethodType.ServerStreaming|]
            (_, Streaming _)           -> [lt|global::Grpc.Core.MethodType.ClientStreaming|]
            _                          -> [lt|global::Grpc.Core.MethodType.Unary|]
        methodTypeEnum (Event {..}) = case methodInput of
            Streaming _ -> error ("Unexpected event '" ++ methodName ++ "' with Streaming parameter") -- the parser should have rejected this
            _           -> [lt|global::Grpc.Core.MethodType.Unary|]

        generics = angles $ sepBy ", " paramName declParams

        genericsWhere = sepBy " " addWhere declParams
          where
            addWhere a = [lt|where #{paramName a} : class|]

        methodDeclaration m = [lt|static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<#{csRequestTypeName m}>, global::Bond.Grpc.IMessage<#{csResponseTypeName m}>> Method_#{methodName m} = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<#{csRequestTypeName m}>, global::Bond.Grpc.IMessage<#{csResponseTypeName m}>>(
            #{methodTypeEnum m},
            ServiceName,
            "#{methodName m}",
            global::Bond.Grpc.Marshaller<#{csRequestTypeName m}>.Instance,
            global::Bond.Grpc.Marshaller<#{csResponseTypeName m}>.Instance);|]

        serverMethodParams :: MethodType -> MethodType -> [CsServerMethodParam]
        serverMethodParams reqType respType = (forRequest reqType) ++ (forResponse respType) ++ context
          where
            forRequest Void = [(CsServerMethodParam  [lt|request|] messageVoid)]
            forRequest (Unary t) = [(CsServerMethodParam [lt|request|] [lt|#{messageOf t}|])]
            forRequest (Streaming t) = [(CsServerMethodParam [lt|requests|] [lt|Grpc.Core.IAsyncStreamReader<#{messageOf t}>|])]
            forResponse Void = []
            forResponse (Unary _) = []
            forResponse (Streaming t) = [(CsServerMethodParam [lt|responses|] [lt|Grpc.Core.IAsyncStreamWriter<#{messageOf t}>|])]
            context = [(CsServerMethodParam [lt|context|] [lt|global::Grpc.Core.ServerCallContext|])]

        -- | For functions (not events), given the result type, compute the
        -- C# abstract method return type.
        csServerFunctionReturn :: MethodType -> L.Text
        csServerFunctionReturn Void = [lt|global::System.Threading.Tasks.Task<#{messageVoid}>|]
        csServerFunctionReturn (Unary t) = [lt|global::System.Threading.Tasks.Task<#{messageOf t}>|]
        csServerFunctionReturn (Streaming _) = [lt|global::System.Threading.Tasks.Task|]

        serverBaseMethods Function{..} = [lt|#{CS.schemaAttributes 3 methodAttributes}public abstract #{csServerFunctionReturn methodResult} #{methodName}(#{csParams});|]
          where params = serverMethodParams methodInput methodResult
                csParams = commaSep csServerParamDecl params

        serverBaseMethods Event{..} = [lt|#{CS.schemaAttributes 3 methodAttributes}public abstract global::System.Threading.Tasks.Task #{methodName}(#{csParams});

            internal global::System.Threading.Tasks.Task<#{messageVoid}> #{uniqImplName methodName}(#{csParams}) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(#{handlerArgs});
            }|]
          where params = serverMethodParams methodInput Void
                csParams = commaSep csServerParamDecl params
                handlerArgs = commaSep id ((L.pack methodName):(map name params))

        -- | For functions (not events), given the input and result types,
        -- compute the C# client method return type.
        csClientFunctionReturn :: MethodType -> MethodType -> L.Text
        csClientFunctionReturn (Streaming input) (Streaming result) = [lt|global::Grpc.Core.AsyncDuplexStreamingCall<#{messageOf input}, #{messageOf result}>|]
        csClientFunctionReturn (Streaming input) (Unary result) = [lt|global::Grpc.Core.AsyncClientStreamingCall<#{messageOf input}, #{messageOf result}>|]
        csClientFunctionReturn _ (Streaming result) = [lt|global::Grpc.Core.AsyncServerStreamingCall<#{messageOf result}>|]
        csClientFunctionReturn _ (Unary result) = [lt|global::Grpc.Core.AsyncUnaryCall<#{messageOf result}>|]
        csClientFunctionReturn _ Void = [lt|global::Grpc.Core.AsyncUnaryCall<#{messageVoid}>|]

        -- | Given the input type, compute the client method parameter.
        csClientRequestParam :: MethodType -> CsClientRequestParam
        csClientRequestParam Void = CsClientRequestParam
          { bareParam = mempty
          , messageParam = [lt|#{messageVoid} request, |]
          , toMessage = [lt|global::Bond.Grpc.Message.Void, |]
          , forwardMessageParam = [lt|, request|] }
        csClientRequestParam (Unary t) = CsClientRequestParam
          { bareParam = [lt|#{getTypeName cs t} request, |]
          , messageParam = [lt|#{messageOf t} request, |]
          , toMessage = [lt|global::Bond.Grpc.Message.From(request), |]
          , forwardMessageParam = [lt|, request|] }
        csClientRequestParam (Streaming _) = CsClientRequestParam
          { bareParam = mempty
          , messageParam = mempty
          , toMessage = mempty
          , forwardMessageParam = mempty }

        -- | Given the input and result types, compute the call invoker needed.
        clientCallInvoker :: MethodType -> MethodType -> L.Text
        clientCallInvoker (Streaming _) (Streaming _) = [lt|AsyncDuplexStreamingCall|]
        clientCallInvoker (Streaming _) (Unary _)     = [lt|AsyncClientStreamingCall|]
        clientCallInvoker (Unary _) (Streaming _)     = [lt|AsyncServerStreamingCall|]
        clientCallInvoker _ _                         = [lt|AsyncUnaryCall|]

        clientMethods Function{..} = [lt|#{CS.schemaAttributes 3 methodAttributes}public virtual #{csClientFunctionReturn methodInput methodResult} #{methodName}Async(#{bareParam requestParam}global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return #{methodName}Async(#{toMessage requestParam}new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            #{CS.schemaAttributes 3 methodAttributes}public virtual #{csClientFunctionReturn methodInput methodResult} #{methodName}Async(#{messageParam requestParam}global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.#{clientCallInvoker methodInput methodResult}(Method_#{methodName}, null, options#{forwardMessageParam requestParam});
            }|]
          where requestParam = csClientRequestParam methodInput

        clientMethods Event{..} = [lt|#{CS.schemaAttributes 3 methodAttributes}public virtual void #{methodName}Async(#{bareParam requestParam}global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                #{methodName}Async(#{toMessage requestParam}new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            #{CS.schemaAttributes 3 methodAttributes}public virtual void #{methodName}Async(#{messageParam requestParam}global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_#{methodName}, null, options#{forwardMessageParam requestParam});
            }|]
          where requestParam = csClientRequestParam methodInput

        serviceMethodBuilder Function{..} = [lt|.AddMethod(Method_#{methodName}, serviceImpl.#{methodName})|]
        serviceMethodBuilder Event{..} = [lt|.AddMethod(Method_#{methodName}, serviceImpl.#{uniqImplName methodName})|]

    grpc _ = mempty
