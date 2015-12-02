-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wwarn #-}

module Language.Bond.Codegen.Cpp.Util
    ( openNamespace
    , closeNamespace
    , structName
    , structParams
    , template
    , modifierTag
    , defaultValue
    , attributeInit
    , schemaMetadata
    , ifndef
    , defaultedFunctions
    , rvalueReferences
    , enumDefinition
    ) where

import Data.Monoid
import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Util
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.TypeMapping

-- open namespaces
openNamespace :: MappingContext -> Text
openNamespace cpp = newlineSep 0 open $ getNamespace cpp
  where
    open n = [lt|namespace #{n}
{|]

-- close namespaces in reverse order
closeNamespace :: MappingContext -> Text
closeNamespace cpp = newlineSep 0 close (reverse $ getNamespace cpp)
  where
    close n = [lt|} // namespace #{n}|]

structName :: Declaration -> String
structName s@Struct {..} = declName <> structParams s
structName _ = error "structName: impossible happened."

structParams :: Declaration -> String
structParams Struct {..} = angles $ sepBy ", " paramName declParams
structParams _ = error "structName: impossible happened."

template :: Declaration -> Text
template d = if null $ declParams d then mempty else [lt|template <typename #{params}>
    |]
  where
    params = sepBy ", typename " paramName $ declParams d

-- attribute initializer
attributeInit :: [Attribute] -> Text
attributeInit [] = "bond::reflection::Attributes()"
attributeInit xs = [lt|boost::assign::map_list_of<std::string, std::string>#{newlineBeginSep 5 attrNameValue xs}|]
  where
    attrNameValue Attribute {..} = [lt|("#{getIdlQualifiedName attrName}", "#{attrValue}")|]


-- modifier tag type for a field
modifierTag :: Field -> Text
modifierTag Field {..} = [lt|bond::reflection::#{modifier fieldType fieldModifier}_field_modifier|]
  where
    modifier BT_MetaName _ = [lt|required_optional|]
    modifier BT_MetaFullName _ = [lt|required_optional|]
    modifier _ RequiredOptional = [lt|required_optional|]
    modifier _ Required = [lt|required|]
    modifier _ _ = [lt|optional|]

defaultValue :: MappingContext -> Type -> Default -> Text
defaultValue _ BT_WString (DefaultString x) = [lt|L"#{x}"|]
defaultValue _ BT_String (DefaultString x) = [lt|"#{x}"|]
defaultValue _ BT_Float (DefaultFloat x) = [lt|#{x}f|]
defaultValue _ BT_Int64 (DefaultInteger (-9223372036854775808)) = [lt|-9223372036854775807LL-1|]
defaultValue _ BT_Int64 (DefaultInteger x) = [lt|#{x}LL|]
defaultValue _ BT_UInt64 (DefaultInteger x) = [lt|#{x}ULL|]
defaultValue _ BT_Int32 (DefaultInteger (-2147483648)) = [lt|-2147483647-1|]
defaultValue m t (DefaultEnum x) = enumValue m t x
defaultValue _ _ (DefaultBool True) = "true"
defaultValue _ _ (DefaultBool False) = "false"
defaultValue _ _ (DefaultInteger x) = [lt|#{x}|]
defaultValue _ _ (DefaultFloat x) = [lt|#{x}|]
defaultValue _ _ (DefaultNothing) = mempty
defaultValue m (BT_UserDefined a@Alias {..} args) d = defaultValue m (resolveAlias a args) d
defaultValue _ _ _ = error "defaultValue: impossible happened."

enumValue :: ToText a => MappingContext -> Type -> a -> Text
enumValue cpp (BT_UserDefined e@Enum {..} _) x =
    [lt|#{getGlobalQualifiedName cpp $ getDeclNamespace cpp e}::_bond_enumerators::#{declName}::#{x}|]
enumValue _ _ _ = error "enumValue: impossible happened."

-- schema metadata static member definitions
schemaMetadata :: MappingContext -> Declaration -> Text
schemaMetadata cpp s@Struct {..} = [lt|
    #{template s}const bond::Metadata #{structName s}::Schema::metadata
        = #{structName s}::Schema::GetMetadata();#{newlineBeginSep 1 staticDef structFields}|]
  where
    -- static member definition for field metadata
    staticDef f@Field {..}
        | fieldModifier == Optional && null fieldAttributes = [lt|
    #{template s}const bond::Metadata #{structName s}::Schema::s_#{fieldName}_metadata
        = bond::reflection::MetadataInit(#{defaultInit f}"#{fieldName}");|]
        | otherwise = [lt|
    #{template s}const bond::Metadata #{structName s}::Schema::s_#{fieldName}_metadata
        = bond::reflection::MetadataInit(#{defaultInit f}"#{fieldName}", #{modifierTag f}::value,
            #{attributeInit fieldAttributes});|]
      where
        defaultInit Field {fieldDefault = (Just def)} = [lt|#{explicitDefault def}, |]
        defaultInit _ = mempty
        explicitDefault (DefaultNothing) = "bond::nothing"
        explicitDefault d@(DefaultInteger _) = staticCast d
        explicitDefault d@(DefaultFloat _) = staticCast d
        explicitDefault d = defaultValue cpp fieldType d
        staticCast d = [lt|static_cast<#{getTypeName cpp fieldType}>(#{defaultValue cpp fieldType d})|]
schemaMetadata _ _ = error "schemaMetadata: impossible happened."

defaultedFunctions, rvalueReferences :: Text
defaultedFunctions = [lt|BOND_NO_CXX11_DEFAULTED_FUNCTIONS|]
rvalueReferences = [lt|BOND_NO_CXX11_RVALUE_REFERENCES|]

ifndef :: ToText a => a -> Text -> Text
ifndef m = between [lt|
#ifndef #{m}|] [lt|
#endif|]

enumDefinition :: Declaration -> Text
enumDefinition Enum {..} = [lt|enum #{declName}
        {
            #{commaLineSep 3 constant enumConstants}
        };|]
  where
    constant Constant {..} = [lt|#{constantName}#{optional value constantValue}|]
    value (-2147483648) = [lt| = -2147483647-1|]
    value x = [lt| = #{x}|]
enumDefinition _ = error "enumDefinition: impossible happened."

