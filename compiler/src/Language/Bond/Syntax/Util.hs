-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Language.Bond.Syntax.Util
    ( associativeType
    , containerType
    , foldMapFields
    , foldMapStructFields
    , foldMapType
    , listType
    , metaType
    , nullableType
    , resolveAlias
    , scalarType
    , stringType
    , structType
    ) where

import Data.Maybe
import Data.List
import qualified Data.Foldable as F
import Data.Monoid
import Prelude
import Language.Bond.Util
import Language.Bond.Syntax.Types

scalarType :: Type -> Bool
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
scalarType (BT_UserDefined a@Alias {} args) = scalarType $ resolveAlias a args
scalarType _ = False

metaType :: Type -> Bool
metaType BT_MetaName = True
metaType BT_MetaFullName = True
metaType (BT_UserDefined a@Alias {} args) = metaType $ resolveAlias a args
metaType _ = False

stringType :: Type -> Bool
stringType BT_String = True
stringType BT_WString = True
stringType (BT_UserDefined a@Alias {} args) = stringType $ resolveAlias a args
stringType _ = False

listType :: Type -> Bool
listType (BT_List _) = True
listType (BT_Vector _) = True
listType (BT_UserDefined a@Alias {} args) = listType $ resolveAlias a args
listType _ = False

associativeType :: Type -> Bool
associativeType (BT_Set _) = True
associativeType (BT_Map _ _) = True
associativeType (BT_UserDefined a@Alias {} args) = associativeType $ resolveAlias a args
associativeType _ = False

containerType :: Type -> Bool
containerType f = listType f || associativeType f

structType :: Type -> Bool
structType (BT_UserDefined Struct {} _) = True
structType (BT_UserDefined Forward {} _) = True
structType (BT_UserDefined a@Alias {} args) = structType $ resolveAlias a args
structType _ = False

nullableType :: Type -> Bool
nullableType (BT_Nullable _) = True
nullableType (BT_UserDefined a@Alias {} args) = nullableType $ resolveAlias a args
nullableType _ = False

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
    (BT_UserDefined   Struct {..} _) -> optional (foldMapFields f) structBase <> F.foldMap f structFields
    (BT_UserDefined a@Alias {..} args) -> foldMapFields f $ resolveAlias a args
    _ -> mempty

foldMapStructFields :: Monoid m => (Field -> m) -> Declaration -> m
foldMapStructFields f s = foldMapFields f $ BT_UserDefined s []

foldMapType :: (Monoid m) => (Type -> m) -> Type -> m
foldMapType f t@(BT_UserDefined _decl args) = f t <> F.foldMap (foldMapType f) args
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
resolveAlias _ _ = error "resolveAlias: impossible happened."

