-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Util
    ( fieldTypeName
    , defaultValue
    , qualifiedName
    , generatedClassAnnotations
    ) where

import Prelude
import Data.Int
import Data.List (intercalate)
import Data.Text.Lazy (Text, pack)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util()

fieldTypeName :: Type -> Text
fieldTypeName fieldType = pack $ "com.microsoft.bond.BondDataType." ++ case fieldType of
    BT_Int8                  -> "BT_INT8"
    BT_Int16                 -> "BT_INT16"
    BT_Int32                 -> "BT_INT32"
    BT_Int64                 -> "BT_INT64"
    BT_UInt8                 -> "BT_UINT8"
    BT_UInt16                -> "BT_UINT16"
    BT_UInt32                -> "BT_UINT32"
    BT_UInt64                -> "BT_UINT64"
    BT_Float                 -> "BT_FLOAT"
    BT_Double                -> "BT_DOUBLE"
    BT_Bool                  -> "BT_BOOL"
    BT_String                -> "BT_STRING"
    BT_WString               -> "BT_WSTRING"
    BT_Blob                  -> "BT_LIST"
    BT_Nullable _            -> "BT_LIST"
    BT_List _                -> "BT_LIST"
    BT_Vector _              -> "BT_LIST"
    BT_Set _                 -> "BT_SET"
    BT_Map _ _               -> "BT_MAP"
    BT_UserDefined Enum {} _ -> "BT_INT32"
    BT_UserDefined _ _       -> "BT_STRUCT"
    -- FIXME: Marker for unsupported types that compiles.
    _                        -> "BT_UNAVAILABLE"

defaultValue :: MappingContext -> Field -> Maybe Text
defaultValue java Field {fieldDefault = Nothing, ..} = implicitDefault fieldType
  where
    newInstance t = Just [lt|new #{getInstanceTypeName java t}()|]
    implicitDefault (BT_Bonded t) = Just [lt|new Bonded<#{getTypeName java t}>(null)|]
    implicitDefault (BT_TypeParam _) = Just [lt|null|]
    implicitDefault (BT_Nullable _) = Just[lt|null|]
    implicitDefault BT_Blob = Just [lt|new byte[0]|]
    implicitDefault BT_Bool = Just [lt|false|]
    implicitDefault BT_Int8 = Just [lt|0|]
    implicitDefault BT_Int16 = Just [lt|0|]
    implicitDefault BT_Int32 = Just [lt|0|]
    implicitDefault BT_Int64 = Just [lt|0L|]
    implicitDefault BT_UInt8 = Just [lt|0|]
    implicitDefault BT_UInt16 = Just [lt|0|]
    implicitDefault BT_UInt32 = Just [lt|0|]
    implicitDefault BT_UInt64 = Just [lt|0L|]
    implicitDefault BT_Float = Just [lt|0.0f|]
    implicitDefault BT_Double = Just [lt|0.0|]
    implicitDefault BT_String = Just [lt|""|]
    implicitDefault BT_WString = Just [lt|""|]
    implicitDefault (BT_List _) = Just [lt|new java.util.LinkedList()|]
    implicitDefault (BT_Set _) = Just [lt|new java.util.HashSet()|]
    implicitDefault (BT_Map _ _) = Just [lt|new java.util.HashMap()|]
    implicitDefault (BT_Vector _) = Just [lt|new java.util.ArrayList()|]
    implicitDefault t@(BT_UserDefined a@Alias {..} args)
        | customAliasMapping java a = newInstance t
        | otherwise = implicitDefault $ resolveAlias a args
    implicitDefault t
        | isStruct t = newInstance t
    implicitDefault _ = Nothing

defaultValue java Field {fieldDefault = (Just def), ..} = explicitDefault def
  where
    explicitDefault (DefaultInteger x) = Just $ intLiteral x
      where
        intMax = toInteger (maxBound :: Int32)
        intMin = toInteger (minBound :: Int32)
        intLiteral value =
            if value > intMax || value < intMin
            then [lt|#{value}L|]
            else [lt|#{value}|]
    explicitDefault (DefaultFloat x) = Just $ floatLiteral fieldType x
      where
        floatLiteral BT_Float y = [lt|#{y}f|]
        floatLiteral BT_Double y = [lt|#{y}|]
        floatLiteral _ _ = error "Java:Float:defaultValue/floatLiteral: impossible happened."
    explicitDefault (DefaultBool True) = Just "true"
    explicitDefault (DefaultBool False) = Just "false"
    explicitDefault (DefaultString x) = Just $ strLiteral fieldType x
      where
        strLiteral BT_String value = [lt|"#{value}"|]
        strLiteral BT_WString value = [lt|"#{value}"|]
        strLiteral _ _ = error "Java:Str:defaultValue/floatLiteral: impossible happened."
    explicitDefault DefaultNothing = Just [lt|null|]
    explicitDefault (DefaultEnum x) = Just [lt|#{getTypeName java fieldType}.#{x}|]

qualifiedName :: MappingContext -> Declaration -> String
qualifiedName java s@Struct {..}  = intercalate "." $ getDeclNamespace java s ++ [declName]
qualifiedName _ _ = error "invalid declaration type for qualifiedName"

generatedClassAnnotations :: Text
generatedClassAnnotations = [lt|@javax.annotation.Generated("gbc")|]
