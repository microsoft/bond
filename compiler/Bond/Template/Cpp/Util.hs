-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Bond.Template.Cpp.Util
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

import Text.Shakespeare.Text
import Data.Monoid
import Bond.Schema
import Bond.Util
import Bond.Template.Util
import Bond.Template.TypeMapping

-- open namespaces
openNamespace cpp = newlineSep 0 open $ getNamespace cpp
  where
    open n = [lt|namespace #{n}
{|]

-- close namespaces in reverse order
closeNamespace cpp = newlineSep 0 close (reverse $ getNamespace cpp)
  where
    close n = [lt|} // namespace #{n}|]

structName s@Struct {..} = declName <> structParams s

structParams Struct {..} = angles $ sepBy ", " paramName declParams

template d = if null $ declParams d then mempty else [lt|template <typename #{params}>
    |]
  where
    params = sepBy ", typename " paramName $ declParams d

-- attribute initializer
attributeInit [] = "bond::reflection::Attributes()"
attributeInit xs = [lt|boost::assign::map_list_of<std::string, std::string>#{newlineBeginSep 5 attrNameValue xs}|]
  where
    attrNameValue Attribute {..} = [lt|("#{getIdlQualifiedName attrName}", "#{attrValue}")|]


-- modifier tag type for a field
modifierTag Field {..} = [lt|bond::reflection::#{modifier fieldType fieldModifier}_field_modifier|]
  where
    modifier BT_MetaName _ = [lt|required_optional|]
    modifier BT_MetaFullName _ = [lt|required_optional|]
    modifier _ RequiredOptional = [lt|required_optional|]
    modifier _ Required = [lt|required|]
    modifier _ _ = [lt|optional|]

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

enumValue cpp (BT_UserDefined e@Enum {..} _) x =
    [lt|#{getGlobalQualifiedName cppTypeMapping $ getDeclNamespace cpp e}::_bond_enumerators::#{declName}::#{x}|]

-- schema metadata static member definitions
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
        defaultInit Field {fieldDefault = (Just def), ..} = [lt|#{explicitDefault def}, |]
        defaultInit _ = mempty
        explicitDefault (DefaultNothing) = "bond::nothing"
        explicitDefault d@(DefaultInteger _) = staticCast d
        explicitDefault d@(DefaultFloat _) = staticCast d
        explicitDefault d = defaultValue cpp fieldType d
        staticCast d = [lt|static_cast<#{getTypeName cpp fieldType}>(#{defaultValue cpp fieldType d})|]

defaultedFunctions = [lt|BOND_NO_CXX11_DEFAULTED_FUNCTIONS|]
rvalueReferences = [lt|BOND_NO_CXX11_RVALUE_REFERENCES|]

ifndef m = between [lt|
#ifndef #{m}|] [lt|
#endif|]

enumDefinition Enum {..} = [lt|enum #{declName}
        {
            #{commaLineSep 3 constant enumConstants}
        };|]
  where
    constant Constant {..} = [lt|#{constantName}#{optional value constantValue}|]
    value x = [lt| = #{x}|]

