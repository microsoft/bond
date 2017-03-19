-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Util
    ( defaultValue
    , generatedClassAnnotations
    ) where

import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util()

defaultValue :: MappingContext -> Field -> Maybe Text
defaultValue java Field {fieldDefault = Nothing, ..} = implicitDefault fieldType
  where
    newInstance t = Just [lt|new #{getInstanceTypeName java t}()|]
    implicitDefault (BT_Bonded t) = Just [lt|new Bonded<#{getTypeName java t}>()|]
    -- We can't really initialize a default object for generic type
    -- field due to the limitation of both Java and Kotlin.
    -- I'm still looking for a solution. Please let me know if you has
    -- any suggestions.
    implicitDefault (BT_TypeParam _) = Just [lt|null|]
    implicitDefault (BT_Nullable _) = Just[lt|null|]
    implicitDefault t@BT_Blob = newInstance t
    implicitDefault BT_Bool = Just [lt|false|]
    implicitDefault BT_Int8 = Just [lt|0|]
    implicitDefault BT_Int16 = Just [lt|0|]
    implicitDefault BT_Int32 = Just [lt|0|]
    implicitDefault BT_Int64 = Just [lt|0L|]
    implicitDefault BT_UInt8 = Just [lt|0|]
    implicitDefault BT_UInt16 = Just [lt|0|]
    implicitDefault BT_UInt32 = Just [lt|0|]
    implicitDefault BT_UInt64 = Just [lt|java.math.BigInteger.ZERO|]
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
    explicitDefault (DefaultInteger x) = Just $ intLiteral fieldType x
      where
        intLiteral BT_Int8 value = [lt|#{value}|]
        intLiteral BT_Int16 value = [lt|#{value}|]
        intLiteral BT_Int32 value = [lt|#{value}|]
        intLiteral BT_Int64 value = [lt|#{value}L|]
        intLiteral BT_UInt8 value = [lt|#{value}|]
        intLiteral BT_UInt16 value = [lt|#{value}|]
        intLiteral BT_UInt32 value = [lt|#{value}|]
        intLiteral BT_UInt64 value = [lt|new java.math.BigInteger("#{value}")|]
        intLiteral _ _ = error "Java:Int:defaultValue/floatLiteral: impossible happened."
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


generatedClassAnnotations :: Text
generatedClassAnnotations = [lt|@javax.annotation.Generated("gbc")|]
