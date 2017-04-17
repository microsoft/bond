-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Schema
    ( schema
    ) where

import Prelude
import Data.List (intercalate)
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Internal (showQualifiedName)
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Java.StaticFields
import Language.Bond.Codegen.Java.Util

schema :: MappingContext -> Declaration -> Text
schema java decl@Struct {..} = [lt|
    // TODO: SchemaDef
    public static final com.microsoft.bond.StructDef #{structDefMember} = new com.microsoft.bond.StructDef();
    #{newlineSep 1 fieldDefMemberDecl structFields}

    static {
        #{structDefMember}.metadata.name = "#{declName}";
        #{structDefMember}.metadata.qualified_name = "#{qualifiedName}";
        #{structDefMember}.metadata.modifier = com.microsoft.bond.Modifier.Optional;
        #{newlineSep 2 (initAttr structDefMember) declAttributes}
        // TODO: .base_def
        #{doubleLineSep 2 initFieldDef structFields}
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

fieldDefMemberDecl :: Field -> Text
fieldDefMemberDecl field@Field {..} =
    [lt|private static final com.microsoft.bond.FieldDef #{fieldDefMember field} = new com.microsoft.bond.FieldDef();|]

initFieldDef :: Field -> Text
initFieldDef field@Field {..} =
    [lt|#{fieldDefMetadata}.name = "#{fieldName}";
        #{fieldDefMetadata}.qualified_name = "";
        #{newlineSep 2 (initAttr fieldDefMetadata) fieldAttributes}
        #{fieldDefRef}.id = #{fieldOrdinal};
        #{fieldDefRef}.type.id = #{fieldTypeName fieldType};
        // TODO: .type.struct_def
        // TODO: .type.element
        // TODO: .type.key
        #{structDefMember}.fields.add(#{fieldDefRef});|]
    where
        fieldDefRef = fieldDefMember field
        fieldDefMetadata = [lt|#{fieldDefRef}.metadata|]

-- TODO: Completely wrong. Need singleton typedefs for primitives and complex
-- ones for structs.
--elementKeyFieldDef :: Field -> Text -> Text
--elementKeyFieldDef Field {..} fieldDefRef =
--    case fieldType of
--        BT_List e    -> [lt|#{fieldDefRef}.type.element = #{fieldTypeName e};|]
--        BT_Vector e  -> [lt|#{fieldDefRef}.type.element = #{fieldTypeName e};|]
--        BT_Set e     -> [lt|#{fieldDefRef}.type.element = #{fieldTypeName e};|]
--        BT_Map k v   -> [lt|#{fieldDefRef}.type.element = #{fieldTypeName v};|]
--        _            -> mempty
