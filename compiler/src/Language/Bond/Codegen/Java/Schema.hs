-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Schema
    ( schema
    ) where

import Prelude
import Data.List (intercalate)
import Data.Text.Lazy (Text, pack)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Internal (showQualifiedName)
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Java.SerializationMethods

schema :: MappingContext -> Declaration -> Text
schema java decl@Struct {..} = [lt|
    public static final com.microsoft.bond.StructDef #{structDefMember} = new com.microsoft.bond.StructDef();

    static {
        #{structDefMember}.metadata.name = "#{declName}";
        #{structDefMember}.metadata.qualified_name = "#{qualifiedName}";
        #{newlineSep 2 (initAttr structDefMember) declAttributes}
        // TODO: .metadata.modifier - what should this be for a struct?
        // TODO: .base_def
        #{doubleLineSep 2 (fieldDef java) structFields}
    }|]
    where
        qualifiedName = intercalate "." $ getDeclNamespace java decl ++ [declName]

schema _ _ = error "java: Can only generate static schema for struct decls."

initAttr :: Text -> Attribute -> Text
initAttr target Attribute {..} =
    [lt|#{target}.metadata.attributes.put(#{name}, #{value});|]
    where
        name = showQualifiedName attrName
        value = attrValue

fieldDef :: MappingContext -> Field -> Text
fieldDef java field@Field {..} =
    [lt|final com.microsoft.bond.FieldDef #{fieldDefLocal} = new com.microsoft.bond.FieldDef();
        #{fieldDefMetadata}.name = "#{fieldName}";
        // TODO: Do fields have qualified_names?
        #{newlineSep 2 (initAttr $ pack fieldDefMetadata) fieldAttributes}
        #{fieldDefLocal}.id = #{fieldOrdinal};
        #{fieldTypeDef field fieldDefLocal}
        #{structDefMember}.fields.add(#{fieldDefLocal});|]
    where
        fieldDefLocal = fieldName ++ "FieldDef"
        fieldDefMetadata = fieldDefLocal ++ ".metadata"

fieldTypeDef :: Field -> String -> Text
fieldTypeDef Field {..} fieldDefLocal =
    [lt|#{fieldDefLocal}.type.id = #{typeName};
        // TODO: .type.struct_def
        // TODO: .type.element
        // TODO: .type.key
        // TODO: .type.bonded|]
    where
        typeName = fieldTypeName fieldType
