-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : alpha
Portability : portable
-}

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Language.Bond.Syntax.Util
    ( -- * Type classification
      -- | Functions that test if a type belongs to a particular category. These
      -- functions will resolve type aliases and return answer based on the type
      -- the alias resolves to.
      isScalar
    , isUnsigned
    , isSigned
    , isFloat
    , isString
    , isContainer
    , isList
    , isAssociative
    , isNullable
    , isStruct
    , isMetaName
      -- * Type mapping
    , fmapType
      -- * Folds
    , foldMapFields
    , foldMapStructFields
    , foldMapType
      -- * Helper functions
    , resolveAlias
    ) where

import Data.Maybe
import Data.List
import qualified Data.Foldable as F
import Data.Monoid
import Prelude
import Language.Bond.Util
import Language.Bond.Syntax.Types

-- | Returns 'True' if the type represents a scalar.
isScalar :: Type -> Bool
isScalar BT_Int8 = True
isScalar BT_Int16 = True
isScalar BT_Int32 = True
isScalar BT_Int64 = True
isScalar BT_UInt8 = True
isScalar BT_UInt16 = True
isScalar BT_UInt32 = True
isScalar BT_UInt64 = True
isScalar BT_Float = True
isScalar BT_Double = True
isScalar BT_Bool = True
isScalar (BT_TypeParam (TypeParam _ (Just Value))) = True
isScalar (BT_UserDefined Enum {..} _) = True
isScalar (BT_UserDefined a@Alias {} args) = isScalar $ resolveAlias a args
isScalar _ = False

-- | Returns 'True' if the type represents unsigned integer
isUnsigned :: Type -> Bool
isUnsigned BT_UInt8 = True
isUnsigned BT_UInt16 = True
isUnsigned BT_UInt32 = True
isUnsigned BT_UInt64 = True
isUnsigned (BT_UserDefined a@Alias {} args) = isUnsigned $ resolveAlias a args
isUnsigned _ = False

-- | Returns 'True' if the type represents signed integer
isSigned :: Type -> Bool
isSigned BT_Int8 = True
isSigned BT_Int16 = True
isSigned BT_Int32 = True
isSigned BT_Int64 = True
isSigned (BT_UserDefined a@Alias {} args) = isSigned $ resolveAlias a args
isSigned _ = False

-- | Returns 'True' if the type represents floating point number
isFloat :: Type -> Bool
isFloat BT_Float = True
isFloat BT_Double = True
isFloat (BT_UserDefined a@Alias {} args) = isFloat $ resolveAlias a args
isFloat _ = False

-- | Returns 'True' if the type represents a meta-name type.
isMetaName :: Type -> Bool
isMetaName BT_MetaName = True
isMetaName BT_MetaFullName = True
isMetaName (BT_UserDefined a@Alias {} args) = isMetaName $ resolveAlias a args
isMetaName _ = False

-- | Returns 'True' if the type represents a string.
isString :: Type -> Bool
isString BT_String = True
isString BT_WString = True
isString (BT_UserDefined a@Alias {} args) = isString $ resolveAlias a args
isString _ = False

-- | Returns 'True' if the type represents a list or a vector.
isList :: Type -> Bool
isList (BT_List _) = True
isList (BT_Vector _) = True
isList (BT_UserDefined a@Alias {} args) = isList $ resolveAlias a args
isList _ = False

-- | Returns 'True' if the type represents a map or a set.
isAssociative :: Type -> Bool
isAssociative (BT_Set _) = True
isAssociative (BT_Map _ _) = True
isAssociative (BT_UserDefined a@Alias {} args) = isAssociative $ resolveAlias a args
isAssociative _ = False

-- | Returns 'True' if the type represents a container.
isContainer :: Type -> Bool
isContainer f = isList f || isAssociative f

-- | Returns 'True' if the type represents a struct.
isStruct :: Type -> Bool
isStruct (BT_UserDefined Struct {} _) = True
isStruct (BT_UserDefined Forward {} _) = True
isStruct (BT_UserDefined a@Alias {} args) = isStruct $ resolveAlias a args
isStruct _ = False

-- | Returns 'True' if the type represents a nullable type.
isNullable :: Type -> Bool
isNullable (BT_Nullable _) = True
isNullable (BT_UserDefined a@Alias {} args) = isNullable $ resolveAlias a args
isNullable _ = False

-- | Recursively map a 'Type' into another 'Type'
--
-- ==== __Examples__
--
-- Change lists into vectors:
--
-- listToVector = fmapType f
--   where
--     f (BT_List x) = BT_Vector x
--     f x = x
fmapType :: (Type -> Type) -> Type -> Type
fmapType f (BT_UserDefined decl args) = f $ BT_UserDefined decl $ map (fmapType f) args
fmapType f (BT_Maybe element) = f $ BT_Maybe $ fmapType f element
fmapType f (BT_Map key value) = f $ BT_Map (fmapType f key) (fmapType f value)
fmapType f (BT_List element) = f $ BT_List $ fmapType f element
fmapType f (BT_Vector element) = f $ BT_Vector $ fmapType f element
fmapType f (BT_Set element) = f $ BT_Set $ fmapType f element
fmapType f (BT_Nullable element) = f $ BT_Nullable $ fmapType f element
fmapType f (BT_Bonded struct) = f $ BT_Bonded $ fmapType f struct
fmapType f x = f x

-- | Maps all fields, including fields of the base, to a 'Monoid', and combines
-- the results.
foldMapFields :: (Monoid m) => (Field -> m) -> Type -> m
foldMapFields f t = case t of
    (BT_UserDefined   Struct {..} _) -> optional (foldMapFields f) structBase <> F.foldMap f structFields
    (BT_UserDefined a@Alias {..} args) -> foldMapFields f $ resolveAlias a args
    _ -> mempty

-- | Like 'foldMapFields' but takes a 'Declaration' as an argument instead of 'Type'.
foldMapStructFields :: Monoid m => (Field -> m) -> Declaration -> m
foldMapStructFields f s = foldMapFields f $ BT_UserDefined s []

-- | Maps all parts of a 'Type' to a 'Monoid' and combines the results.
--
-- ==== __Examples__
--
-- For a type:
--
-- @list\<nullable\<int32>>@
--
-- the result is:
--
-- @ f (BT_List (BT_Nullable BT_Int32)) <> f (BT_Nullable BT_Int32) <> f BT_Int32@
--
-- 'foldMapType' resolves type aliases so given the following type alias
-- declaration (Bond IDL syntax):
--
-- @using Array<T, N> = vector<T>;
--
-- the result for the following type:
--
-- @Array<int32, 10>@
--
-- is:
--
-- @ f (BT_UserDefined Alias{..} [BT_Int32, BT_IntTypeArg 10]) <> f (BT_Vector BT_Int32) <> f BT_Int32@
foldMapType :: (Monoid m) => (Type -> m) -> Type -> m
foldMapType f t@(BT_UserDefined a@Alias {} args) = f t <> foldMapType f (resolveAlias a args)
foldMapType f t@(BT_UserDefined _ args) = f t <> F.foldMap (foldMapType f) args
foldMapType f t@(BT_Maybe element) = f t <> foldMapType f element
foldMapType f t@(BT_Map key value) = f t <> foldMapType f key <> foldMapType f value
foldMapType f t@(BT_List element) = f t <> foldMapType f element
foldMapType f t@(BT_Vector element) = f t <> foldMapType f element
foldMapType f t@(BT_Set element) = f t <> foldMapType f element
foldMapType f t@(BT_Nullable element) = f t <> foldMapType f element
foldMapType f t@(BT_Bonded struct) = f t <> foldMapType f struct
foldMapType f x = f x


-- | Resolves a type alias declaration with given type arguments. Note that the
-- function resolves one level of aliasing and thus may return a type alias.
resolveAlias :: Declaration -> [Type] -> Type
resolveAlias Alias {..} args = fmapType resolveParam aliasType
  where
    resolveParam (BT_TypeParam param) = snd.fromJust $ find ((param ==).fst) paramsArgs
    resolveParam x = x
    paramsArgs = zip declParams args
resolveAlias _ _ = error "resolveAlias: impossible happened."

