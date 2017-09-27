-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cs.Grpc_cs (
  grpc_cs)
where

import Data.Maybe
import Data.Monoid
import Prelude
import qualified Data.Text.Lazy as L
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping
import qualified Language.Bond.Codegen.Cs.Util as CS

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

        getMessageTypeName t = maybe "global::Bond.Void" (getTypeName cs) t

        generics = angles $ sepBy ", " paramName declParams

        genericsWhere = sepBy " " addWhere declParams
          where
            addWhere a = [lt|where #{paramName a} : class|]

        methodDeclaration Function{..} = [lt|static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}>, global::Bond.Grpc.IMessage<#{getMessageTypeName methodResult}>> Method_#{methodName} = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}>, global::Bond.Grpc.IMessage<#{getMessageTypeName methodResult}>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "#{methodName}",
            global::Bond.Grpc.Marshaller<#{getMessageTypeName methodInput}>.Instance,
            global::Bond.Grpc.Marshaller<#{getMessageTypeName methodResult}>.Instance);|]

        methodDeclaration Event{..} = [lt|static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}>, global::Bond.Grpc.IMessage<#{getMessageTypeName Nothing}>> Method_#{methodName} = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}>, global::Bond.Grpc.IMessage<#{getMessageTypeName Nothing}>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "#{methodName}",
            global::Bond.Grpc.Marshaller<#{getMessageTypeName methodInput}>.Instance,
            global::Bond.Grpc.Marshaller<#{getMessageTypeName Nothing}>.Instance);|]

        serverBaseMethods Function{..} = [lt|public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<#{getMessageTypeName methodResult}>> #{methodName}(global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}> request, global::Grpc.Core.ServerCallContext context);|]

        serverBaseMethods Event{..} = [lt|public abstract global::System.Threading.Tasks.Task #{methodName}(global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}> request, global::Grpc.Core.ServerCallContext context);

            internal global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<#{getMessageTypeName Nothing}>> #{uniqImplName methodName}(global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}> request, global::Grpc.Core.ServerCallContext context) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(#{methodName}, request, context);
            }|]

        firstParam Nothing = mempty
        firstParam t = [lt|#{getMessageTypeName t} request, |]

        requestOrVoidConstructor Nothing = [lt|global::Bond.Grpc.Message.Void|]
        requestOrVoidConstructor _ = [lt|global::Bond.Grpc.Message.From(request)|]

        clientMethods Function{..} = [lt|public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<#{getMessageTypeName methodResult}>> #{methodName}Async(#{firstParam methodInput}global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                var message = #{requestOrVoidConstructor methodInput};
                return #{methodName}Async(message, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<#{getMessageTypeName methodResult}>> #{methodName}Async(global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_#{methodName}, null, options, request);
            }|]

        clientMethods Event{..} = [lt|public virtual void #{methodName}Async(#{firstParam methodInput}global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                var message = #{requestOrVoidConstructor methodInput};
                #{methodName}Async(message, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual void #{methodName}Async(global::Bond.Grpc.IMessage<#{getMessageTypeName methodInput}> request, global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_#{methodName}, null, options, request);
            }|]

        serviceMethodBuilder Function{..} = [lt|.AddMethod(Method_#{methodName}, serviceImpl.#{methodName})|]

        serviceMethodBuilder Event{..} = [lt|.AddMethod(Method_#{methodName}, serviceImpl.#{uniqImplName methodName})|]

    grpc _ = mempty
