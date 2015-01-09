-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Bond.Schema
    ( Declaration(..)
    , Field(..)
    , makeField
    , Constant(..)
    , Modifier(..)
    , Type(..)
    , TypeParam(..)
    , Constraint(..)
    , Default(..)
    , Import(..)
    , Language(..)
    , Namespace(..)
    , Attribute(..)
    , QualifiedName(..)
    , takeName
    , takeNamespace
    , showQualifiedName
    , scalarType
    , listType
    , associativeType
    , containerType
    , stringType
    , metaType
    , structType
    , nullableType
    , duplicateDeclaration
    , isBaseField
    , foldMapFields
    , foldMapStructFields
    , foldMapType
    , metaField
    , resolveAlias
    ) where

import Data.Maybe
import Data.Word
import Data.List
import Data.Foldable (foldMap)
import Data.Monoid
import System.FilePath
import Data.Text.Lazy.Builder
import Text.Shakespeare.Text
import Bond.Util

type QualifiedName = [String]

takeName :: QualifiedName -> String
takeName = last

takeNamespace :: QualifiedName -> QualifiedName
takeNamespace = subtract 1 . length >>= take

showQualifiedName  :: QualifiedName -> String
showQualifiedName  = sepBy "." id

data Modifier = Optional | Required | RequiredOptional deriving Eq

data Type =
    BT_Int8 | BT_Int16 | BT_Int32 | BT_Int64 |
    BT_UInt8 | BT_UInt16 | BT_UInt32 | BT_UInt64 |
    BT_Float | BT_Double |
    BT_Bool |
    BT_String | BT_WString |
    BT_MetaName | BT_MetaFullName |
    BT_Blob |
    BT_Maybe Type |
    BT_List Type |
    BT_Vector Type |
    BT_Nullable Type |
    BT_Set Type |
    BT_Map Type Type |
    BT_Bonded Type |
    BT_IntTypeArg Int |
    BT_TypeParam TypeParam |
    BT_UserDefined Declaration [Type]
    deriving Eq

scalarType BT_Int8 = True
scalarType BT_Int16 = True
scalarType BT_Int32 = True
scalarType BT_Int64 = True
scalarType BT_UInt8 = True
scalarType BT_UInt16 = True
scalarType BT_UInt32 = True
scalarType BT_UInt64 = True
scalarType BT_Float = True
scalarType BT_Double = True
scalarType BT_Bool = True
scalarType (BT_TypeParam (TypeParam _ (Just Value))) = True
scalarType (BT_UserDefined Enum {..} _) = True
scalarType _ = False

metaType BT_MetaName = True
metaType BT_MetaFullName = True
metaType _ = False

stringType BT_String = True
stringType BT_WString = True
stringType _ = False

listType (BT_List _) = True
listType (BT_Vector _) = True
listType _ = False

associativeType (BT_Set _) = True
associativeType (BT_Map _ _) = True
associativeType _ = False

containerType f = listType f || associativeType f

structType (BT_UserDefined Struct {} _) = True
structType (BT_UserDefined a@Alias {} args) = structType $ resolveAlias a args
structType _ = False

nullableType (BT_Nullable _) = True
nullableType _ = False

metaField Field {..} = Any $ metaType fieldType

data Default =
    DefaultBool Bool |
    DefaultInteger Integer |
    DefaultFloat Double |
    DefaultString String |
    DefaultEnum String|
    DefaultNothing
    deriving Eq

data Attribute =
    Attribute                                                                                                                               
        { attrName :: QualifiedName         -- attribute name
        , attrValue :: String               -- value
        }
    deriving Eq

data Field =
    Field
        { fieldAttributes :: [Attribute]    -- zero or more attributes
        , fieldOrdinal :: Word16            -- ordinal
        , fieldModifier :: Modifier         -- field modifier
        , fieldType :: Type                 -- type
        , fieldName :: String               -- field name
        , fieldDefault :: Maybe Default     -- optional default value
        }
    deriving Eq

makeField a o m t n d@(Just DefaultNothing) = Field a o m (BT_Maybe t) n d
makeField a o m t n d = Field a o m t n d

data Constant =
    Constant
        { constantName :: String            -- enum constant name
        , constantValue :: Maybe Int        -- optional value
        }
    deriving Eq

data Constraint = Value deriving Eq

instance Show Constraint where
    show Value = ": value"

data TypeParam =
    TypeParam
        { paramName :: String
        , paramConstraint :: Maybe Constraint
        }
        deriving Eq

instance Show TypeParam where
    show TypeParam {..} = paramName ++ optional show paramConstraint

data Declaration =
    Struct
        { declNamespaces :: [Namespace]     -- namespace(s) in which the struct is declared
        , declAttributes :: [Attribute]     -- zero or more attributes
        , declName :: String                -- struct identifier
        , declParams :: [TypeParam]         -- type parameters for generics
        , structBase :: Maybe Type          -- optional base struct
        , structFields :: [Field]           -- zero or more fields
        }
    |
    Enum
        { declNamespaces :: [Namespace]     -- namespace(s) in which the enum is declared
        , declAttributes :: [Attribute]     -- zero or more attributes
        , declName :: String                -- enum identifier
        , enumConstants :: [Constant]       -- one or more constant values
        }
    |
    Forward
        { declNamespaces :: [Namespace]     -- namespace(s) in which the struct is declared
        , declName :: String                -- struct identifier
        , declParams :: [TypeParam]         -- type parameters for generics
        }
    |
    Alias
        { declNamespaces :: [Namespace]     -- namespace(s) in which the alias is declared
        , declName :: String                -- alias identifier
        , declParams :: [TypeParam]         -- type parameters for generics
        , aliasType :: Type                 -- aliased type
        }
    deriving Eq

showTypeParams = angles . sepBy ", " show

instance Show Declaration where
    show Struct {..} = "struct " ++ declName ++ showTypeParams declParams
    show Enum {..} = "enum " ++ declName
    show Forward {..} = "struct declaration " ++ declName ++ showTypeParams declParams
    show Alias {..} = "alias " ++ declName ++ showTypeParams declParams

mapType :: (Type -> Type) -> Type -> Type
mapType f (BT_UserDefined decl args) = BT_UserDefined decl $ map f args
mapType f (BT_Map key value) = BT_Map (f key) (f value)
mapType f (BT_List element) = BT_List $ f element
mapType f (BT_Vector element) = BT_Vector $ f element
mapType f (BT_Set element) = BT_Set $ f element
mapType f (BT_Nullable element) = BT_Nullable $ f element
mapType f (BT_Bonded struct) = BT_Bonded $ f struct
mapType f x = f x

foldMapFields :: (Monoid m) => (Field -> m) -> Type -> m
foldMapFields f t = case t of
    (BT_UserDefined s@Struct {..} _) -> optional (foldMapFields f) structBase <> foldMap f structFields
    (BT_UserDefined a@Alias {..} args) -> foldMapFields f $ resolveAlias a args
    _ -> mempty

foldMapStructFields f s = foldMapFields f $ BT_UserDefined s []

foldMapType :: (Monoid m) => (Type -> m) -> Type -> m
foldMapType f t@(BT_UserDefined decl args) = f t <> foldMap (foldMapType f) args
foldMapType f t@(BT_Map key value) = f t <> foldMapType f key <> foldMapType f value
foldMapType f t@(BT_List element) = f t <> foldMapType f element
foldMapType f t@(BT_Vector element) = f t <> foldMapType f element
foldMapType f t@(BT_Set element) = f t <> foldMapType f element
foldMapType f t@(BT_Nullable element) = f t <> foldMapType f element
foldMapType f t@(BT_Bonded struct) = f t <> foldMapType f struct
foldMapType f x = f x


resolveAlias :: Declaration -> [Type] -> Type
resolveAlias Alias {..} args = mapType resolveParam $ resolveParam aliasType
  where
    resolveParam (BT_TypeParam param) = snd.fromJust $ find ((param ==).fst) paramsArgs
    resolveParam x = x
    paramsArgs = zip declParams args

duplicateDeclaration :: Declaration -> Declaration -> Bool
duplicateDeclaration left right = 
    (declName left == declName right)
 && not (null $ intersect (declNamespaces left) (declNamespaces right))

isBaseField :: String -> Maybe Type -> Bool
isBaseField name = getAny . optional (foldMapFields (Any.(name==).fieldName))

data Import = Import FilePath

data Language = Cpp | Cs | CSharp | Java deriving (Eq)

data Namespace = 
    Namespace 
        { nsLanguage :: Maybe Language
        , nsName :: QualifiedName
        }
        deriving Eq


