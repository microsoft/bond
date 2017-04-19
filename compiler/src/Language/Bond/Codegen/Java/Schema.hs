-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Schema
    ( schema
    ) where

import Prelude
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
    #{schemaDefMemberDecl}

    #{newlineSep 1 fieldDefMemberDecl structFields}

    static {
        #{structDefLocalDecl decl}
        #{schemaDefMember}.structs.add(0, #{structDefRef});
        #{schemaDefMember}.root.id = #{fieldTypeName $ BT_UserDefined decl []};
        #{schemaDefMember}.root.struct_def = 0;
        #{schemaDefMember}.root.element = null;
        #{schemaDefMember}.root.key = null;
        #{schemaDefMember}.root.bonded_type = false;

        #{structDefRef}.metadata.name = "#{declName}";
        #{structDefRef}.metadata.qualified_name = "#{qualifiedName java decl}";
        #{structDefRef}.metadata.modifier = com.microsoft.bond.Modifier.Optional;
        #{newlineSep 2 (initAttr structDefRef) declAttributes}
        // TODO: .base_def
        #{doubleLineSep 2 (initFieldDef java structDefRef) structFields}
    }|]
    where
        structDefRef = structDefLocal decl

schema _ _ = error "java: Can only generate static schema for struct decls."

initAttr :: Text -> Attribute -> Text
initAttr target Attribute {..} =
    [lt|#{target}.metadata.attributes.put(#{name}, #{value});|]
    where
        name = showQualifiedName attrName
        value = attrValue

schemaDefMemberDecl :: Text
schemaDefMemberDecl =
    [lt|public static final com.microsoft.bond.SchemaDef #{schemaDefMember} = new com.microsoft.bond.SchemaDef();|]

fieldDefMemberDecl :: Field -> Text
fieldDefMemberDecl field@Field {..} =
    [lt|private static final com.microsoft.bond.FieldDef #{fieldDefMember field} = new com.microsoft.bond.FieldDef();|]

structDefLocal :: Declaration -> Text
structDefLocal Struct {..} = [lt|#{declName}_struct_def|]
structDefLocal _ = error "java: Can only generate struct def locals for struct decls."

structDefLocalDecl :: Declaration -> Text
structDefLocalDecl s@Struct {..} =
    [lt|final com.microsoft.bond.StructDef #{structDefLocal s} = new com.microsoft.bond.StructDef();|]
structDefLocalDecl _ = error "java: Can only generate struct def locals for struct decls."

initFieldDef :: MappingContext -> Text -> Field -> Text
initFieldDef java structDefRef field@Field {..} =
    [lt|#{fieldDefMetadata}.name = "#{fieldName}";
        #{fieldDefMetadata}.qualified_name = "";
        #{newlineSep 2 (initAttr fieldDefMetadata) fieldAttributes}
        #{fieldDefRef}.id = #{fieldOrdinal};
        #{fieldDefRef}.type.id = #{fieldTypeName fieldType};
        #{fieldDefRef}.type.struct_def = #{typeDefStructDef fieldType};
        // TODO: .type.element
        // TODO: .type.key
        #{structDefRef}.fields.add(#{fieldDefRef});|]
    where
        fieldDefRef = fieldDefMember field
        fieldDefMetadata = [lt|#{fieldDefRef}.metadata|]
        -- FIXME: Thread MappingContext through?
        typeDefStructDef :: Type -> Text
        typeDefStructDef (BT_UserDefined s@Struct {..} _) =
            [lt|com.microsoft.bond.schema.SchemaUtils.add(#{schemaDefMember}, #{qualifiedName java s}.#{schemaDefMember}.structs.get(0))|]
        typeDefStructDef _ = [lt|0|]

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
