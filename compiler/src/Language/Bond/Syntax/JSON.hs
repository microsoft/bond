-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, QuasiQuotes, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : provisional
Portability : portable
-}

module Language.Bond.Syntax.JSON
    ( -- * FromJSON and ToJSON instances
      -- $aeson
    )
    where

import Control.Applicative
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.HashMap.Strict (member)
import Data.Text.Lazy (unpack)
import Language.Bond.Syntax.Types hiding (MethodType(..))
import qualified Language.Bond.Syntax.Types as BST (MethodType(..))
import Prelude
import Text.Shakespeare.Text (lt)

-- $aeson
--
-- This module defines 'FromJSON' and 'ToJSON' instances for Bond abstract
-- syntax tree.  They allow using the <http://hackage.haskell.org/package/aeson aeson>
-- library to encode Bond AST types to <https://microsoft.github.io/bond/manual/compiler.html#schema-ast JSON format>:
--
-- > > encode (Bond [] [Namespace Nothing ["example"]] [])
-- > "{\"namespaces\":[{\"name\":[\"example\"]}],\"imports\":[],\"declarations\":[]}"
--
-- and decode Bond data types from JSON:
--
-- > > decode "{\"namespaces\":[{\"name\":[\"example\"]}],\"imports\":[],\"declarations\":[]}" :: Maybe Bond
-- > Just (Bond {bondImports = [], bondNamespaces = [Namespace {nsLanguage = Nothing, nsName = ["example"]}], bondDeclarations = []})

instance FromJSON Type where
    parseJSON (String "int8") = pure BT_Int8
    parseJSON (String "int16") = pure BT_Int16
    parseJSON (String "int32") = pure BT_Int32
    parseJSON (String "int64") = pure BT_Int64
    parseJSON (String "uint8") = pure BT_UInt8
    parseJSON (String "uint16") = pure BT_UInt16
    parseJSON (String "uint32") = pure BT_UInt32
    parseJSON (String "uint64") = pure BT_UInt64
    parseJSON (String "float") = pure BT_Float
    parseJSON (String "double") = pure BT_Double
    parseJSON (String "bool") = pure BT_Bool
    parseJSON (String "string") = pure BT_String
    parseJSON (String "wstring") = pure BT_WString
    parseJSON (String "bond_meta::name") = pure BT_MetaName
    parseJSON (String "bond_meta::full_name") = pure BT_MetaFullName
    parseJSON (String "blob") = pure BT_Blob
    parseJSON (Object o) = do
        type_ <- o .: "type"
        case type_ of
            String "maybe" -> BT_Maybe <$>
                o .: "element"
            String "list" -> BT_List <$>
                o .: "element"
            String "vector" -> BT_Vector <$>
                o .: "element"
            String "nullable" -> BT_Nullable <$>
                o .: "element"
            String "set" -> BT_Set <$>
                o .: "element"
            String "map" -> BT_Map <$>
                o .: "key" <*>
                o .: "element"
            String "bonded" -> BT_Bonded <$>
                o .: "element"
            String "constant" -> BT_IntTypeArg <$>
                o .: "value"
            String "parameter" -> BT_TypeParam <$>
                o .: "value"
            String "user" -> BT_UserDefined <$>
                o .: "declaration" <*>
                o .:? "arguments" .!= []
            _ -> modifyFailure
                    (const $ "Invalid value `" ++ show type_ ++ "` for the `type` key.")
                    empty
    parseJSON x = modifyFailure
                    (const $ "Expected a representation of Type but found: " ++ show x)
                    empty

instance ToJSON Type where
    toJSON BT_Int8 = "int8"
    toJSON BT_Int16 = "int16"
    toJSON BT_Int32 = "int32"
    toJSON BT_Int64 = "int64"
    toJSON BT_UInt8 = "uint8"
    toJSON BT_UInt16 = "uint16"
    toJSON BT_UInt32 = "uint32"
    toJSON BT_UInt64 = "uint64"
    toJSON BT_Float = "float"
    toJSON BT_Double = "double"
    toJSON BT_Bool = "bool"
    toJSON BT_String = "string"
    toJSON BT_WString = "wstring"
    toJSON BT_MetaName = "bond_meta::name"
    toJSON BT_MetaFullName = "bond_meta::full_name"
    toJSON BT_Blob = "blob"
    toJSON (BT_Maybe t) = object
        [ "type" .= String "maybe"
        , "element" .= t
        ]
    toJSON (BT_List t) = object
        [ "type" .= String "list"
        , "element" .= t
        ]
    toJSON (BT_Vector t) = object
        [ "type" .= String "vector"
        , "element" .= t
        ]
    toJSON (BT_Nullable t) = object
        [ "type" .= String "nullable"
        , "element" .= t
        ]
    toJSON (BT_Set t) = object
        [ "type" .= String "set"
        , "element" .= t
        ]
    toJSON (BT_Map k t) = object
        [ "type" .= String "map"
        , "key" .= k
        , "element" .= t
        ]
    toJSON (BT_Bonded t) = object
        [ "type" .= String "bonded"
        , "element" .= t
        ]
    toJSON (BT_IntTypeArg n) = object
        [ "type" .= String "constant"
        , "value" .= n
       ]
    toJSON (BT_TypeParam p) = object
        [ "type" .= String "parameter"
        , "value" .= p
        ]
    toJSON (BT_UserDefined decl []) = object
        [ "type" .= String "user"
        , "declaration" .= decl
        ]
    toJSON (BT_UserDefined decl args) = object
        [ "type" .= String "user"
        , "declaration" .= decl
        , "arguments" .= args
        ]

instance FromJSON Default where
    parseJSON (Object o) = do
        type_ <- o .: "type"
        case type_ of
            String "bool" -> DefaultBool <$> o .: "value"
            String "integer" -> DefaultInteger <$> o .: "value"
            String "float" -> DefaultFloat <$> o .: "value"
            String "string" -> DefaultString <$> o .: "value"
            String "enum" -> DefaultEnum <$> o .: "value"
            String "nothing" -> pure DefaultNothing
            _ -> modifyFailure
                    (const $ "Invalid value `" ++ show type_ ++ "` for the `type` key.")
                    empty
    parseJSON x = modifyFailure
                    (const $ "Expected a representation of Default but found: " ++ show x)
                    empty

instance ToJSON Default where
    toJSON (DefaultBool x) = object
        [ "type" .= String "bool"
        , "value" .= x
        ]
    toJSON (DefaultInteger x) = object
        [ "type" .= String "integer"
        , "value" .= x
        ]
    toJSON (DefaultFloat x) = object
        [ "type" .= String "float"
        , "value" .= x
        ]
    toJSON (DefaultString x) = object
        [ "type" .= String "string"
        , "value" .= x
        ]
    toJSON (DefaultEnum x) = object
        [ "type" .= String "enum"
        , "value" .= x
        ]
    toJSON DefaultNothing = object
        [ "type" .= String "nothing"
        ]

instance FromJSON Field where
    parseJSON (Object o) = Field <$>
        o .:? "fieldAttributes" .!= [] <*>
        o .:  "fieldOrdinal" <*>
        o .:? "fieldModifier" .!= Optional <*>
        o .:  "fieldType" <*>
        o .:  "fieldName" <*>
        o .:? "fieldDefault" .!= Nothing
    parseJSON x = modifyFailure
                    (const $ "Expected a representation of Field but found: " ++ show x)
                    empty

instance ToJSON Field where
    toJSON f = object
        [ "fieldAttributes" .= fieldAttributes f
        , "fieldOrdinal" .= fieldOrdinal f
        , "fieldModifier" .= fieldModifier f
        , "fieldType" .= fieldType f
        , "fieldName" .= fieldName f
        , "fieldDefault" .= fieldDefault f
        ]

instance FromJSON Constraint where
    parseJSON (String "value") = pure Value
    parseJSON x = modifyFailure
                    (const $ "Expected a representation of Constraint but found: " ++ show x)
                    empty

instance ToJSON Constraint where
    toJSON Value = "value"

instance FromJSON Namespace where
    parseJSON (Object v) =
        Namespace <$>
            v .:? "language" <*>
            v .: "name"
    parseJSON x = modifyFailure
                    (const $ "Expected an object but found: " ++ show x)
                    empty

instance ToJSON Namespace where
    toJSON (Namespace Nothing name) = object
        [ "name" .= name
        ]
    toJSON Namespace {..} = object
        [ "language" .= nsLanguage
        , "name" .= nsName
        ]

instance FromJSON Bond where
    parseJSON (Object v) =
        Bond <$>
            v .: "imports" <*>
            v .: "namespaces" <*>
            v .: "declarations"
    parseJSON x = modifyFailure
                    (const $ "Expected an object but found: " ++ show x)
                    empty

instance ToJSON Bond where
    toJSON Bond {..} = object
        [ "imports" .= bondImports
        , "namespaces" .= bondNamespaces
        , "declarations" .= bondDeclarations
        ]

instance ToJSON BST.MethodType where
    toJSON BST.Void = Null
    toJSON (BST.Unary t) = toJSON t
    toJSON (BST.Streaming t) = toJSON t

data MethodStreamingTag = Unary | Client | Server | Duplex deriving Show
$(deriveJSON defaultOptions ''MethodStreamingTag)

methodStreamingTag :: BST.MethodType -> BST.MethodType -> MethodStreamingTag
methodStreamingTag input result = case (input, result) of
  (BST.Streaming _, BST.Streaming _) -> Duplex
  (BST.Streaming _, _) -> Client
  (_, BST.Streaming _) -> Server
  _ -> Unary

instance ToJSON Method where
    toJSON Event {..} = object
        [ "tag" .= String "Event"
        , "methodName" .= methodName
        , "methodAttributes" .= methodAttributes
        , "methodInput" .= methodInput
        ]
    toJSON Function {..} = object
        [ "tag" .= String "Function"
        , "methodName" .= methodName
        , "methodAttributes" .= methodAttributes
        , "methodResult" .= methodResult
        , "methodInput" .= methodInput
        , "methodStreaming" .= (methodStreamingTag methodInput methodResult)
        ]

instance FromJSON Method where
  parseJSON = withObject "Method" (\o -> do
    tag <- o .: "tag"
    methodName :: String <- o .:? "methodName" .!= "<unknown>"
    case tag of
      (String "Event") -> modifyFailure ((unpack [lt|Parsing event '#{show methodName}' failed: |]) ++) (parseEvent o)
      (String "Function") -> modifyFailure ((unpack [lt|Parsing function '#{show methodName}' failed: |]) ++) (parseFunction o)
      _ -> modifyFailure (const $ unpack [lt|Unexpected tag '#{show tag}' when parsing method '#{show methodName}'. Expecting "Event" or "Function".|]) empty)
    where
      parseEvent :: Object -> Parser Method
      parseEvent o =
        Event <$>
          o .:? "methodAttributes" .!= [] <*>
          o .: "methodName" <*>
          methodInput
        <* ensureNoMethodStreaming
        where ensureNoMethodStreaming = if member "methodStreaming" o
                                          then fail "Encountered Event with \"methodStreaming\" member. Events cannot have this member."
                                          else pure ()
              methodInput = maybe BST.Void BST.Unary <$> (o .:? "methodInput")

      parseFunction :: Object -> Parser Method
      parseFunction o =
          Function <$>
            o .:? "methodAttributes" .!= [] <*>
            methodResult <*>
            o .: "methodName" <*>
            methodInput
        where
          streamingTag :: Parser MethodStreamingTag
          streamingTag = o .:? "methodStreaming"  .!= Unary

          methodInput :: Parser BST.MethodType
          methodInput = do
            i <- o .:? "methodInput"
            st <- streamingTag
            case (i, st) of
              (Nothing, Unary) -> pure $ BST.Void
              (Nothing, Client) -> fail $ invalidNothingComboMsg "input" Client
              (Nothing, Server) -> pure $ BST.Void
              (Nothing, Duplex) -> fail $ invalidNothingComboMsg "input" Duplex
              (Just t, Unary) -> pure $ BST.Unary t
              (Just t, Client) -> pure $ BST.Streaming t
              (Just t, Server) -> pure $ BST.Unary t
              (Just t, Duplex) -> pure $ BST.Streaming t

          methodResult :: Parser BST.MethodType
          methodResult = do
            r <- o .:? "methodResult"
            st <- streamingTag
            case (r, st) of
              (Nothing, Unary) -> pure $ BST.Void
              (Nothing, Client) -> pure $ BST.Void
              (Nothing, Server) -> fail $ invalidNothingComboMsg "result" Server
              (Nothing, Duplex) -> fail $ invalidNothingComboMsg "result" Duplex
              (Just t, Unary) -> pure $ BST.Unary t
              (Just t, Client) -> pure $ BST.Unary t
              (Just t, Server) -> pure $ BST.Streaming t
              (Just t, Duplex) -> pure $ BST.Streaming t

          invalidNothingComboMsg :: String -> MethodStreamingTag -> String
          invalidNothingComboMsg dir streaming = unpack [lt|Method marked as #{show streaming}, but has void #{dir}|]

$(deriveJSON defaultOptions ''Modifier)
$(deriveJSON defaultOptions ''Attribute)
$(deriveJSON defaultOptions ''Constant)
$(deriveJSON defaultOptions ''TypeParam)
$(deriveJSON defaultOptions ''Declaration)
$(deriveJSON defaultOptions ''Import)
$(deriveJSON defaultOptions ''Language)
