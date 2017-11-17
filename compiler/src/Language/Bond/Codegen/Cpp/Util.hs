-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wwarn #-}

module Language.Bond.Codegen.Cpp.Util
    ( openNamespace
    , closeNamespace
    , className
    , classParams
    , qualifiedClassName
    , template
    , modifierTag
    , defaultValue
    , attributeInit
    , schemaMetadata
    , enumDefinition
    , isEnumDeclaration
    , enumValueToNameInitList
    , enumNameToValueInitList
    ) where

import Data.Int (Int64)
import Data.List (sortOn)
import Data.Monoid
import Prelude
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Builder (toLazyText)
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

className :: Declaration -> String
className decl = declName decl <> classParams decl

classParams :: Declaration -> String
classParams = angles . sepBy ", " paramName . declParams

qualifiedClassName :: MappingContext -> Declaration -> String
qualifiedClassName cpp s@Struct {..} = qualifiedName <> classParams s
  where
    qualifiedName = unpack $ toLazyText $ getDeclTypeName cpp s
qualifiedClassName _ _ = error "qualifiedClassName: impossible happened."

template :: Declaration -> Text
template d = if null $ declParams d then mempty else [lt|template <typename #{params}>
    |]
  where
    params = sepBy ", typename " paramName $ declParams d

-- attribute initializer
attributeInit :: [Attribute] -> Text
attributeInit [] = "::bond::reflection::Attributes()"
attributeInit xs = [lt|{
                    #{commaLineSep 5 attrNameValueText sortedAttributes}
                }|]
  where
    idl = MappingContext idlTypeMapping [] [] []
    attrNameValue Attribute {..} = (getQualifiedName idl attrName, attrValue)
    sortedAttributes = sortOn fst $ map attrNameValue xs
    attrNameValueText (name, value) = [lt|{ "#{name}", "#{value}" }|]


-- modifier tag type for a field
modifierTag :: Field -> Text
modifierTag Field {..} = [lt|::bond::reflection::#{modifier fieldType fieldModifier}_field_modifier|]
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
    [lt|#{getQualifiedName cpp $ getDeclNamespace cpp e}::_bond_enumerators::#{declName}::#{x}|]
enumValue cpp (BT_UserDefined a@Alias {..} args) e = enumValue cpp (resolveAlias a args) e
enumValue _ _ _ = error "enumValue: impossible happened."

-- schema metadata static member definitions
schemaMetadata :: MappingContext -> Declaration -> Text
schemaMetadata cpp s@Struct {..} = [lt|
    #{template s}const ::bond::Metadata #{className s}::Schema::metadata
        = #{className s}::Schema::GetMetadata();#{newlineBeginSep 1 staticDef structFields}|]
  where
    -- static member definition for field metadata
    staticDef f@Field {..}
        | fieldModifier == Optional && null fieldAttributes = [lt|
    #{template s}const ::bond::Metadata #{className s}::Schema::s_#{fieldName}_metadata
        = ::bond::reflection::MetadataInit(#{defaultInit f}"#{fieldName}");|]
        | otherwise = [lt|
    #{template s}const ::bond::Metadata #{className s}::Schema::s_#{fieldName}_metadata
        = ::bond::reflection::MetadataInit(#{defaultInit f}"#{fieldName}", #{modifierTag f}::value,
                #{attributeInit fieldAttributes});|]
      where
        defaultInit Field {fieldDefault = (Just def)} = [lt|#{explicitDefault def}, |]
        defaultInit _ = mempty
        explicitDefault (DefaultNothing) = "::bond::nothing"
        explicitDefault d@(DefaultInteger _) = staticCast d
        explicitDefault d@(DefaultFloat _) = staticCast d
        explicitDefault d = defaultValue cpp fieldType d
        staticCast d = [lt|static_cast<#{getTypeName cpp fieldType}>(#{defaultValue cpp fieldType d})|]
schemaMetadata _ s@Service {..} = [lt|
    #{template s}const ::bond::Metadata #{className s}::Schema::metadata
        = ::bond::reflection::MetadataInit#{metadataInitArgs}("#{declName}", "#{idlNamespace}",
                #{attributeInit declAttributes});#{newlineBeginSep 1 staticDef serviceMethods}|]
  where
    idl = MappingContext idlTypeMapping [] [] []
    idlNamespace = getDeclTypeName idl s
    metadataInitArgs = if null declParams then mempty else [lt|<boost::mpl::list#{classParams s} >|]
    -- static member definition for method metadata
    staticDef m = [lt|
    #{template s}const ::bond::Metadata #{className s}::Schema::s_#{methodName m}_metadata
        = ::bond::reflection::MetadataInit("#{methodName m}"#{attributes $ methodAttributes m}|]
      where
        attributes [] = [lt|);|]
        attributes a = [lt|,
                #{attributeInit a});|]
schemaMetadata _ _ = error "schemaMetadata: impossible happened."

enumDefinition :: Declaration -> Text
enumDefinition Enum {..} = [lt|enum #{declName}
        {
            #{commaLineSep 3 constant enumConstants}
        };|]
  where
    constant Constant {..} = [lt|#{constantName}#{optional value constantValue}|]
    value (-2147483648) = [lt| = static_cast<int32_t>(-2147483647-1)|]
    value x = [lt| = static_cast<int32_t>(#{x})|]
enumDefinition _ = error "enumDefinition: impossible happened."

isEnumDeclaration :: Declaration -> Bool
isEnumDeclaration Enum {} = True
isEnumDeclaration _ = False


enumValueToNameInitList :: Int64 -> Declaration -> Text
enumValueToNameInitList n Enum {..} = commaLineSep n valueNameConst enumConstByValue
  where
    valueNameConst (name, _) = [lt|{ #{name}, "#{name}" }|]
    enumConstByValue = sortOn snd $ reifyEnumValues enumConstants
enumValueToNameInitList _ _ = error "enumValueToNameInitList: impossible happened."

enumNameToValueInitList :: Int64 -> Declaration -> Text
enumNameToValueInitList n Enum {..} = commaLineSep n nameValueConst enumConstByName
  where
    nameValueConst Constant {..} = [lt|{ "#{constantName}", #{constantName} }|]
    enumConstByName = sortOn constantName enumConstants
enumNameToValueInitList _ _ = error "enumNameToValueInitList: impossible happened."
