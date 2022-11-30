-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Cs.Util
    ( typeAttributes
    , propertyAttributes
    , schemaAttributes
    , paramConstraints
    , defaultValue
    , disableCscWarnings
    , disableReSharperWarnings
    ) where

import Data.Int (Int64)
import Data.Monoid
import Prelude
import Data.Text.Lazy (Text, isPrefixOf)
import Data.Text.Lazy.Builder (toLazyText)
import Text.Shakespeare.Text
import Paths_bond (version)
import Data.Version (showVersion)
import Language.Bond.Syntax.Types
import Language.Bond.Syntax.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util

disableCscWarnings :: Text
disableCscWarnings = [lt|
// suppress "Missing XML comment for publicly visible type or member"
#pragma warning disable 1591
|]

disableReSharperWarnings :: Text
disableReSharperWarnings = [lt|
#region ReSharper warnings
// ReSharper disable PartialTypeWithSinglePart
// ReSharper disable RedundantNameQualifier
// ReSharper disable InconsistentNaming
// ReSharper disable CheckNamespace
// ReSharper disable UnusedParameter.Local
// ReSharper disable RedundantUsingDirective
#endregion
|]

-- C# field/property attributes
propertyAttributes :: MappingContext -> Field -> Text
propertyAttributes cs Field {..} =
    schemaAttributes 2 fieldAttributes
 <> [lt|[global::Bond.Id(#{fieldOrdinal})#{typeAttribute}#{modifierAttribute fieldType fieldModifier}]
        |]
        where
            annotatedType = getAnnotatedTypeName cs fieldType
            propertyType = getTypeName cs fieldType
            typeAttribute = if annotatedType /= propertyType
                then [lt|, global::Bond.Type(typeof(#{annotatedType}))|]
                else mempty
            modifierAttribute BT_MetaName _ = [lt|, global::Bond.RequiredOptional|]
            modifierAttribute BT_MetaFullName _ = [lt|, global::Bond.RequiredOptional|]
            modifierAttribute _ Required = [lt|, global::Bond.Required|]
            modifierAttribute _ RequiredOptional = [lt|, global::Bond.RequiredOptional|]
            modifierAttribute _ _ = mempty

-- C# class/struct/interface attributes
typeAttributes :: MappingContext -> Declaration -> Text
typeAttributes cs s@Struct {..} =
    optionalTypeAttributes cs s
 <> [lt|[global::Bond.Schema]
    |]
 <> generatedCodeAttr

-- C# enum attributes
typeAttributes cs e@Enum {..} =
    optionalTypeAttributes cs e
 <> generatedCodeAttr

-- C# service attributes
typeAttributes cs s@Service {..} =
    optionalTypeAttributes cs s
    <> generatedCodeAttr

typeAttributes _ _ = error "typeAttributes: impossible happened."

generatedCodeAttr :: Text
generatedCodeAttr = [lt|[System.CodeDom.Compiler.GeneratedCode("gbc", "#{showVersion version}")]
    |]

idl :: MappingContext
idl = MappingContext idlTypeMapping [] [] []

optionalTypeAttributes :: MappingContext -> Declaration -> Text
optionalTypeAttributes cs decl =
    schemaAttributes 1 (declAttributes decl)
 <> namespaceAttribute
  where
    namespaceAttribute = if getDeclNamespace idl decl == getDeclNamespace cs decl
        then mempty
        else [lt|[global::Bond.Namespace("#{getQualifiedName idl $ getDeclNamespace idl decl}")]
    |]

-- Attributes defined by the user in the schema
schemaAttributes :: Int64 -> [Attribute] -> Text
schemaAttributes indent_ = newlineSepEnd indent_ schemaAttribute
  where
    schemaAttribute Attribute {..} =
        [lt|[global::Bond.Attribute("#{getQualifiedName idl attrName}", "#{attrValue}")]|]

-- generic type parameter constraints
paramConstraints :: [TypeParam] -> Text
paramConstraints = newlineBeginSep 2 constraint
  where
    constraint (TypeParam _ Nothing) = mempty
    constraint (TypeParam name (Just Value)) = [lt|where #{name} : struct|]

isImmutableCollection :: MappingContext -> Type -> Bool
isImmutableCollection cs t = [lt|System.Collections.Immutable.Immutable|] `isPrefixOf` toLazyText (getInstanceTypeName cs t)

-- Initial value for C# field/property or Nothing if C# implicit default is OK
defaultValue :: MappingContext -> Field -> Maybe Text
defaultValue cs Field {fieldDefault = Nothing, ..} = implicitDefault fieldType
  where
    newInstance t = Just [lt|new #{getInstanceTypeName cs t}()|]
    staticEmptyField t = Just [lt|#{getInstanceTypeName cs t}.Empty|]
    implicitDefault (BT_Bonded t) = Just [lt|global::Bond.Bonded<#{getTypeName cs t}>.Empty|]
    implicitDefault t@(BT_TypeParam _) = Just [lt|global::Bond.GenericFactory.Create<#{getInstanceTypeName cs t}>()|]
    implicitDefault t@BT_Blob = newInstance t
    implicitDefault t@(BT_UserDefined a@Alias {..} args)
        | isImmutableCollection cs t = staticEmptyField t
        | customAliasMapping cs a = newInstance t
        | otherwise = implicitDefault $ resolveAlias a args
    implicitDefault t
        | isString t = Just [lt|""|]
        | isContainer t || isStruct t = newInstance t
    implicitDefault _ = Nothing

defaultValue cs Field {fieldDefault = (Just def), ..} = explicitDefault def
  where
    explicitDefault (DefaultInteger x) = Just [lt|#{x}|]
    explicitDefault (DefaultFloat x) = Just $ floatLiteral fieldType x
      where
        floatLiteral BT_Float y = [lt|#{y}F|]
        floatLiteral BT_Double y = [lt|#{y}|]
        floatLiteral _ _ = error "defaultValue/floatLiteral: impossible happened."
    explicitDefault (DefaultBool True) = Just "true"
    explicitDefault (DefaultBool False) = Just "false"
    explicitDefault (DefaultString x) = Just [lt|"#{x}"|]
    explicitDefault (DefaultEnum x) = Just [lt|#{getTypeName cs fieldType}.#{x}|]
    explicitDefault _ = Nothing
