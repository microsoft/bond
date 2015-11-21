-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-|
Copyright   : (c) Microsoft
License     : MIT
Maintainer  : adamsap@microsoft.com
Stability   : alpha
Portability : portable
-}

{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Language.Bond.Syntax.SchemaDef
    ( -- * Runtime schema (aka SchemaDef) support
       encodeSchemaDef
    ) where

import Data.Word
import Data.List
import Data.Maybe
import Data.Function
import Data.Text.Lazy.Builder
import qualified Data.Foldable as F
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as L
import Data.Monoid
import Control.Applicative hiding (optional)
import Prelude
import Data.Aeson
import Data.Aeson.TH
import Language.Bond.Util
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Codegen.TypeMapping

-- | Return an instance of SchemaDef for the specified type. The SchemaDef is
-- encoded using Bond SimpleJSON protocol and returned as a lazy 'ByteString'.
encodeSchemaDef :: Type -> BL.ByteString
encodeSchemaDef = encode . makeSchemaDef

data SchemaDef =
    SchemaDef
        { structs :: [StructDef]
        }

data StructDef =
    StructDef
        { metadata :: Metadata
        , base_def :: Maybe [TypeDef]
        , fields :: [FieldDef]
        }

data FieldDef =
    FieldDef
        { _metadata :: Metadata
        , _id :: Word16
        , _type :: TypeDef
        }

data TypeDef =
    TypeDef
        { id :: Maybe Int
        , struct_def :: Maybe Int
        , element :: Maybe [TypeDef]
        , key :: Maybe [TypeDef]
        , bonded_type :: Maybe Bool
        }

data Metadata =
    Metadata
        { name :: String
        , qualified_name :: Maybe String
        , attributes :: Maybe [String]
        , modifier :: Maybe Int
        , default_value :: Maybe Variant
        }

data Variant =
    Variant
        { uint_value :: Maybe Integer
        , int_value :: Maybe Integer
        , double_value :: Maybe Double
        , string_value :: Maybe String
        , wstring_value :: Maybe String
        , nothing :: Maybe Bool
        }

-- Returns BondDataType enum value for a 'Type'. Applies only to scalars/strings
typeId :: Type -> Int
typeId t = case t of
    BT_Bool       -> 2
    BT_UInt8      -> 3
    BT_UInt16     -> 4
    BT_UInt32     -> 5
    BT_UInt64     -> 6
    BT_Float      -> 7
    BT_Double     -> 8
    BT_String     -> 9
    BT_Int8       -> 14
    BT_Int16      -> 15
    BT_Int32      -> 16
    BT_Int64      -> 17
    BT_WString    -> 18
    BT_MetaName   -> typeId BT_String
    BT_MetaFullName -> typeId BT_String
    (BT_UserDefined Enum {} _) -> typeId BT_Int32
    _ -> error "typeId: unexpected type"


makeSchemaDef :: Type -> SchemaDef
makeSchemaDef root = SchemaDef $ map structDef structs
  where
    ctx = MappingContext idlTypeMapping [] [] []
    -- list of structs in the schema
    structs = nub $ f root
      where
        f t@(BT_UserDefined Struct{..} declArgs) = [t]
            <|> optional ((foldMapType f) . resolve) structBase
            <|> F.foldMap ((foldMapType f) . resolve . fieldType) structFields
          where
            resolve = resolveType declParams declArgs
        f _ = mempty
    -- index of a struct in the schema
    structIdx typ@(BT_UserDefined f fArgs) = case findIndex matchingStruct structs of
        Nothing -> error $ "makeSchemaDef.structIdx: struct not found " ++ show typ
        Just n -> n
      where
        matchingStruct (BT_UserDefined s@Struct{} sArgs) =
               declName s == declName f
            && not (null $ intersect (declNamespaces s) (declNamespaces f))
            && sArgs == fArgs
        matchingStruct t = typ == t
    structIdx typ = error $ "makeSchemaDef.structIdx: undefined struct: " ++ show typ
    -- StructDef for the specified struct type
    structDef typ@(BT_UserDefined s@Struct{..} declArgs) = StructDef metadata base fields
      where
        resolve = resolveType declParams declArgs
        structQualifiedName = L.unpack $ toLazyText $ getTypeName ctx typ
        structName = drop (1 + (length $ qualifiedName $ getDeclNamespace ctx s)) structQualifiedName
        metadata = Metadata structName (Just structQualifiedName) (attr declAttributes) Nothing Nothing
        base = pure . typeDef . resolve <$> structBase
        fields = map fieldDef structFields
        fieldDef Field {..} = FieldDef fieldMetadata fieldOrdinal (typeDef schemaFieldType)
          where
            schemaFieldType = resolve fieldType
            fieldMetadata = Metadata fieldName Nothing (attr fieldAttributes) modifier
                (defaultValue schemaFieldType <$> fieldDefault)
            modifier = case fieldModifier of
                Optional -> Nothing
                Required -> Just 1
                RequiredOptional -> Just 2
            defaultValue BT_WString (DefaultString x) = variant {wstring_value = Just x}
            defaultValue BT_String (DefaultString x)  = variant {string_value = Just x}
            defaultValue t (DefaultFloat x)
                | isFloat t                           = variant {double_value = Just x}
            defaultValue t (DefaultInteger x)
                | isSigned t                          = variant {int_value = Just x}
                | isUnsigned t                        = variant {uint_value = Just x}
                | isFloat t                           = variant {double_value = Just $ fromInteger x}
            defaultValue BT_Maybe{} (DefaultNothing)  = variant {nothing = Just True}
            defaultValue BT_Bool (DefaultBool x)      = variant {uint_value = Just $ if x then 1 else 0}
            defaultValue (BT_UserDefined e _) (DefaultEnum x) = variant {int_value = Just $ resolveEnum e x}
            defaultValue _ _ = error $ "makeSchemaDef.defaultValue: invalid default value for field "
                                     ++ structName ++ "." ++ fieldName
    structDef _ = error "makeSchemaDef.structDef: Not a struct type"
    -- TypeDef for specified type
    typeDef typ
        | isScalar typ || isString typ || isMetaName typ
                             = TypeDef (Just $ typeId typ) Nothing Nothing Nothing Nothing
        | otherwise = case typ of
            BT_Blob         -> listDef BT_Int8
            (BT_List t)     -> listDef t
            (BT_Vector t)   -> listDef t
            (BT_Nullable t) -> listDef t
            (BT_Set t)      -> TypeDef (Just 12) Nothing (Just [typeDef t]) Nothing Nothing
            (BT_Map k t)    -> TypeDef (Just 13) Nothing (Just [typeDef t]) (Just [typeDef k]) Nothing
            (BT_Bonded t)   -> (typeDef t) {bonded_type = Just True}
            (BT_Maybe t)    -> typeDef t
            t               -> TypeDef Nothing (Just (structIdx t)) Nothing Nothing Nothing
      where
        listDef t = TypeDef (Just 11) Nothing (Just [typeDef t]) Nothing Nothing
    variant = Variant Nothing Nothing Nothing Nothing Nothing Nothing
    attr [] = Nothing
    attr xs = Just $ concatMap (\a -> [qualifiedName $ attrName a, attrValue a]) xs
    qualifiedName = L.unpack . toLazyText . getQualifiedName ctx
    -- resolve type parameters into type arguments and aliases into aliased types
    resolveType typeParams typeArgs = fmapType resolve
      where
        resolve (BT_TypeParam p) = snd . fromJust $ find ((p ==) . fst) $ zip typeParams typeArgs
        resolve (BT_UserDefined a@Alias{} args) = resolve $ resolveAlias a args
        resolve t = t
    -- resolve value of an enum constant
    resolveEnum Enum{..} n = fromIntegral . snd . fromJust $ find ((n ==) . fst) $ nameValues 0 enumConstants
      where
        -- fill in values for constants w/o explicitly specified value
        nameValues _ [] = []
        nameValues _ ((Constant name (Just value)):xs) = (name, value) : nameValues (value + 1) xs
        nameValues next ((Constant name Nothing):xs)   = (name, next) : nameValues (next + 1) xs
    resolveEnum _ _ = error "makeSchemaDef.resolveEnum: not a enum"

$(deriveToJSON defaultOptions {omitNothingFields = True} ''SchemaDef)
$(deriveToJSON defaultOptions {omitNothingFields = True} ''StructDef)
$(deriveToJSON defaultOptions {omitNothingFields = True, fieldLabelModifier = dropWhile ('_' ==)} ''FieldDef)
$(deriveToJSON defaultOptions {omitNothingFields = True} ''TypeDef)
$(deriveToJSON defaultOptions {omitNothingFields = True} ''Metadata)
$(deriveToJSON defaultOptions {omitNothingFields = True} ''Variant)
