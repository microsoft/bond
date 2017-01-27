-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cs.Comm_cs (
  comm_interface_cs,
  comm_proxy_cs,
  comm_service_cs)
where

import Data.Maybe
import Data.Monoid
import Data.List (nub)
import Prelude
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping
import qualified Language.Bond.Codegen.Cs.Util as CS


getMessageTypeName :: MappingContext -> Maybe Type -> Builder
getMessageTypeName ctx t = maybe "global::Bond.Void" (getTypeName ctx) t

getMessageProxyInputParam :: MappingContext -> Maybe Type -> Builder
getMessageProxyInputParam ctx t = maybe "" constructParam t
  where
    constructParam x = getTypeName ctx x `mappend` fromText " param"

paramOrBondVoid :: Maybe Type -> String
paramOrBondVoid t = if isNothing t then "new global::Bond.Void()" else "param"

-- | Codegen template for generating code containing declarations of
-- of services including interfaces, proxies and services files.

comm_interface_cs :: MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
comm_interface_cs cs _ _ declarations = ("_interfaces.cs", [lt|
#{CS.disableCscWarnings}
#{CS.disableReSharperWarnings}
namespace #{csNamespace}
{
    #{doubleLineSep 1 comm declarations}
} // #{csNamespace}
|])
  where
    csNamespace = sepBy "." toText $ getNamespace cs

    comm s@Service{..} = [lt|#{CS.typeAttributes cs s}public interface I#{declName}#{generics}
    {
        #{doubleLineSep 2 methodDeclaration serviceMethods}
    }
    |]
      where
        generics = angles $ sepBy ", " paramName declParams

        methodDeclaration Function{..} = [lt|global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>> #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param, global::System.Threading.CancellationToken ct);|]
          where
            getMessageResultTypeName = getMessageTypeName cs methodResult
            getMessageInputTypeName = getMessageTypeName cs methodInput

        methodDeclaration Event{..} = [lt|void #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param);|]
          where
            getMessageInputTypeName = getMessageTypeName cs methodInput

    comm _ = mempty

comm_proxy_cs :: MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
comm_proxy_cs cs _ _ declarations = ("_proxies.cs", [lt|
#{CS.disableCscWarnings}
#{CS.disableReSharperWarnings}
namespace #{csNamespace}
{
    #{doubleLineSep 1 comm declarations}
} // #{csNamespace}
|])
  where
    csNamespace = sepBy "." toText $ getNamespace cs
    idl = MappingContext idlTypeMapping [] [] []  

    comm s@Service{..} = [lt|#{CS.typeAttributes cs s}public class #{declName}Proxy<#{proxyGenerics}TConnection> : I#{declName}#{interfaceGenerics}#{connConstraint}
    {
        private readonly TConnection m_connection;

        public #{declName}Proxy(TConnection connection)
        {
            m_connection = connection;
        }

        #{doubleLineSep 2 proxyMethod serviceMethods}
    }|]
      where
        methodCapability Function {} = "global::Bond.Comm.IRequestResponseConnection"
        methodCapability Event {} = "global::Bond.Comm.IEventConnection"

        getCapabilities :: [Method] -> [String]
        getCapabilities m = nub $ map methodCapability m 
        connConstraint = " where TConnection : " ++ sepBy ", " (\p -> p) (getCapabilities serviceMethods)

        interfaceGenerics = angles $ sepBy "," paramName declParams -- of the form "<T1, T2, T3>"
        proxyGenerics = sepEndBy ", " paramName declParams -- of the form "T1, T2, T3, "

        proxyMethod Function{..} = [lt|public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>> #{methodName}Async(#{getMessageProxyInputParam cs methodInput})
        {
            var message = new global::Bond.Comm.Message<#{getMessageInputTypeName}>(#{paramOrBondVoid methodInput});
            return #{methodName}Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>> #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<#{getMessageInputTypeName}, #{getMessageResultTypeName}>(
                "#{getDeclTypeName idl s}",
                "#{methodName}",
                param,
                ct);
        }|]
          where
            getMessageResultTypeName = getMessageTypeName cs methodResult
            getMessageInputTypeName = getMessageTypeName cs methodInput

        proxyMethod Event{..} = [lt|public void #{methodName}Async(#{getMessageProxyInputParam cs methodInput})
        {
            var message = new global::Bond.Comm.Message<#{getMessageInputTypeName}>(#{paramOrBondVoid methodInput});
            #{methodName}Async(message);
        }

        public void #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param)
        {
            m_connection.FireEventAsync<#{getMessageInputTypeName}>(
                "#{getDeclTypeName idl s}",
                "#{methodName}",
                param);
        }|]
          where
            getMessageInputTypeName = getMessageTypeName cs methodInput

    comm _ = mempty

comm_service_cs :: MappingContext -> String -> [Import] -> [Declaration] -> (String, L.Text)
comm_service_cs cs _ _ declarations = ("_services.cs", [lt|
#{CS.disableCscWarnings}
#{CS.disableReSharperWarnings}
namespace #{csNamespace}
{
    #{doubleLineSep 1 comm declarations}
} // #{csNamespace}
|])
  where
    csNamespace = sepBy "." toText $ getNamespace cs
    idl = MappingContext idlTypeMapping [] [] []  

    comm s@Service{..} = [lt|#{CS.typeAttributes cs s}public abstract class #{declName}ServiceBase#{generics} : I#{declName}#{generics}, global::Bond.Comm.IService
    {
        public global::System.Collections.Generic.IEnumerable<global::Bond.Comm.ServiceMethodInfo> Methods
        {
            get
            {
                #{newlineSep 4 methodInfo serviceMethods}
            }
        }

        #{doubleLineSep 2 methodAbstract serviceMethods}

        #{doubleLineSep 2 methodGlue serviceMethods}
    }
    |]
      where
        generics = angles $ sepBy ", " paramName declParams

        methodInfo Function{..} = [lt|yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="#{getDeclTypeName idl s}.#{methodName}", Callback = #{methodName}Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};|]
        methodInfo Event{..} = [lt|yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="#{getDeclTypeName idl s}.#{methodName}", Callback = #{methodName}Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.Event};|]

        methodAbstract Function{..} = [lt|public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>> #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param, global::System.Threading.CancellationToken ct);|]
          where
            getMessageResultTypeName = getMessageTypeName cs methodResult
            getMessageInputTypeName = getMessageTypeName cs methodInput

        methodAbstract Event{..} = [lt|public abstract void #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param);|]
          where
            getMessageInputTypeName = getMessageTypeName cs methodInput

        methodGlue Function{..} = [lt|private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> #{methodName}Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>,
                                                           global::Bond.Comm.IMessage>(
                #{methodName}Async(param.Convert<#{getMessageInputTypeName}>(), ct));
        }|]
          where
            getMessageResultTypeName = getMessageTypeName cs methodResult
            getMessageInputTypeName = getMessageTypeName cs methodInput

        methodGlue Event{..} = [lt|private global::System.Threading.Tasks.Task #{methodName}Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            #{methodName}Async(param.Convert<#{getMessageInputTypeName}>());
            return global::Bond.Comm.CodegenHelpers.CompletedTask;
        }|]
          where
            getMessageInputTypeName = getMessageTypeName cs methodInput

    comm _ = mempty
