-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cs.Comm_cs (
  comm_interface_cs,
  comm_proxy_cs,
  comm_service_cs)
where

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

    comm s@Service{..} = [lt|#{CS.typeAttributes cs s}interface I#{declName}#{generics}
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
        methodDeclaration _ = mempty
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

    comm s@Service{..} = [lt|#{CS.typeAttributes cs s}public class Proxy_#{declName}<#{generics}TConnection> : I#{declName}#{connConstraint}
    {
        private readonly TConnection m_connection;

        public Proxy_#{declName}(TConnection connection)
        {
            m_connection = connection;
        }

        #{doubleLineSep 2 proxyMethod serviceMethods}
    }|]
      where
        methodCapability Function {} = "global::Bond.Comm.IRequestResponseConnection"
        methodCapability _ = ""

        getCapabilities :: [Method] -> [String]
        getCapabilities m = nub $ map methodCapability m 
        connConstraint = " where TConnection : " ++ sepBy ", " (\p -> p) (getCapabilities serviceMethods)

        generics = sepEndBy ", " paramName declParams

        proxyMethod Function{..} = [lt|public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>> #{methodName}Async(#{getMessageInputTypeName} param)
        {
            var message = new global::Bond.Comm.Message<#{getMessageInputTypeName}>(param);
            return #{methodName}Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>> #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<#{getMessageInputTypeName}, #{getMessageResultTypeName}>(
                "#{getDeclTypeName idl s}.#{methodName}",
                param,
                ct);
        }|]
          where
            getMessageResultTypeName = getMessageTypeName cs methodResult
            getMessageInputTypeName = getMessageTypeName cs methodInput
        proxyMethod _ = mempty

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

    comm s@Service{..} = [lt|#{CS.typeAttributes cs s}public abstract class #{declName}Service#{generics} : I#{declName}#{generics}, global::Bond.Comm.IService
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

        methodInfo Function{..} = [lt|yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="#{getDeclTypeName idl s}.#{methodName}", Callback = #{methodName}Async_Glue};|]
        methodInfo _ = mempty

        methodAbstract Function{..} = [lt|public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<#{getMessageResultTypeName}>> #{methodName}Async(global::Bond.Comm.IMessage<#{getMessageInputTypeName}> param, global::System.Threading.CancellationToken ct);|]
          where
            getMessageResultTypeName = getMessageTypeName cs methodResult
            getMessageInputTypeName = getMessageTypeName cs methodInput
        methodAbstract _ = mempty

        methodGlue Function{..} = [lt|private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> #{methodName}Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await #{methodName}Async(param.Convert<#{getMessageInputTypeName}>(), ct);
        }|]
          where
            getMessageInputTypeName = getMessageTypeName cs methodInput
        methodGlue _ = mempty
    comm _ = mempty
