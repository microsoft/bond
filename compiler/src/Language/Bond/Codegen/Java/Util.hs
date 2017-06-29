-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Util
    ( qualifiedDeclaredTypeName
    , generatedClassAnnotations
    , modifierConstantName
    , isPrimitiveNonEnumBondType
    , isPrimitiveBondType
    , isGenericBondStructType
    ) where

import Prelude
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util()

-- returns the fully qualified name for a declaration
qualifiedDeclaredTypeName :: MappingContext -> Declaration -> String
qualifiedDeclaredTypeName java d@Struct {..}  = intercalate "." $ getDeclNamespace java d ++ [declName]
qualifiedDeclaredTypeName java d@Enum {..}  = intercalate "." $ getDeclNamespace java d ++ [declName]
qualifiedDeclaredTypeName java d@Forward {..}  = intercalate "." $ getDeclNamespace java d ++ [declName]
qualifiedDeclaredTypeName java d@Alias {..}  = intercalate "." $ getDeclNamespace java d ++ [declName]
qualifiedDeclaredTypeName java d@Service {..}  = intercalate "." $ getDeclNamespace java d ++ [declName]

-- returns the Java annotations for a generated class
generatedClassAnnotations :: Text
generatedClassAnnotations = [lt|@javax.annotation.Generated("gbc")|]

-- returns the qualified name of Modifier constant
modifierConstantName :: Modifier -> Text
modifierConstantName Optional = [lt|com.microsoft.bond.Modifier.Optional|]
modifierConstantName Required = [lt|com.microsoft.bond.Modifier.Required|]
modifierConstantName RequiredOptional = [lt|com.microsoft.bond.Modifier.RequiredOptional|]

-- returns a value indicating whether a type is a non-enum Bond primitive type
isPrimitiveNonEnumBondType :: Type -> Bool
isPrimitiveNonEnumBondType BT_Int8 = True
isPrimitiveNonEnumBondType BT_Int16 = True
isPrimitiveNonEnumBondType BT_Int32 = True
isPrimitiveNonEnumBondType BT_Int64 = True
isPrimitiveNonEnumBondType BT_UInt8 = True
isPrimitiveNonEnumBondType BT_UInt16 = True
isPrimitiveNonEnumBondType BT_UInt32 = True
isPrimitiveNonEnumBondType BT_UInt64 = True
isPrimitiveNonEnumBondType BT_Float = True
isPrimitiveNonEnumBondType BT_Double = True
isPrimitiveNonEnumBondType BT_Bool = True
isPrimitiveNonEnumBondType BT_String = True
isPrimitiveNonEnumBondType BT_WString = True
isPrimitiveNonEnumBondType _ = False

-- returns a value indicating whether a type is a Bond primitive type or enum
isPrimitiveBondType :: Type -> Bool
isPrimitiveBondType (BT_UserDefined Enum {..} _) = True
isPrimitiveBondType t = isPrimitiveNonEnumBondType t

-- returns a value indicating whether a type is a generic struct type with generic type parameters
isGenericBondStructType :: Type -> Bool
isGenericBondStructType (BT_UserDefined Struct {..} _) = not (null declParams)
isGenericBondStructType _ = False
