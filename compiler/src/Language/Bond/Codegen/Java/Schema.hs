-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Schema
    ( schema
    ) where

import Prelude
import Data.Text.Lazy (Text, pack)
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
    #{structDefMemberDecl}
    #{newlineSep 1 fieldDefMemberDecl structFields}
    private static boolean #{schemaInitField} = false;

    public static synchronized void initSchema() {
        if (#{schemaInitField}) { return; }

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
        #{schemaDefMember}.structs.add(0, #{structDefRef});

        #{doubleLineSep 2 (initFieldDef java structDefRef declName) structFields}

        #{schemaInitField} = true;
    }

    public static com.microsoft.bond.SchemaDef getSchema() {
        initSchema();
        return #{schemaDefMember};
    }

    public static com.microsoft.bond.StructDef getStructDef() {
        initSchema();
        return #{structDefMember};
    }
    |]
    where
        structDefRef = structDefMember
        schemaInitField = pack "schemaInitialized"

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

structDefMemberDecl :: Text
structDefMemberDecl =
    [lt|public static final com.microsoft.bond.StructDef #{structDefMember} = new com.microsoft.bond.StructDef();|]

fieldDefMemberDecl :: Field -> Text
fieldDefMemberDecl field@Field {..} =
    [lt|private static final com.microsoft.bond.FieldDef #{fieldDefMember field} = new com.microsoft.bond.FieldDef();|]

initFieldDef :: MappingContext -> Text -> String -> Field -> Text
initFieldDef java structDefRef structClassName field@Field {..} =
    [lt|#{fieldDefMetadata}.name = "#{fieldName}";
        #{fieldDefMetadata}.qualified_name = "";
        #{newlineSep 2 (initAttr fieldDefMetadata) fieldAttributes}
        #{fieldDefRef}.id = #{fieldOrdinal};
        #{initTypeDef java fieldType fieldDefTypeDefRef structClassName}
        #{structDefRef}.fields.add(#{fieldDefRef});|]
    where
        fieldDefRef = fieldDefMember field
        fieldDefTypeDefRef = [lt|#{fieldDefRef}.type|]
        fieldDefMetadata = [lt|#{fieldDefRef}.metadata|]

initTypeDef :: MappingContext -> Type -> Text -> String -> Text
initTypeDef java t@(BT_UserDefined decl@Struct {..} _) typeDefRef structClassName =
    [lt|#{typeDefRef}.id = #{fieldTypeName t};
        try {
            Class<#{fieldClassName}> #{fieldClassLocal} = #{fieldClassName}.class;
            com.microsoft.bond.StructDef #{fieldStructDefLocal} = (com.microsoft.bond.StructDef) (#{fieldClassLocal}.getDeclaredField("#{structDefMember}").get(null));
            #{typeDefRef}.struct_def = com.microsoft.bond.schema.SchemaUtils.add(#{schemaDefMember}, #{fieldStructDefLocal}, #{fieldClassLocal});
        } catch (NoSuchFieldException nsfe) {
            throw new RuntimeException(nsfe);
        } catch (IllegalAccessException iae) {
            throw new RuntimeException(iae);
        }
        // TODO: .type.element
        // TODO: .type.key
        #{typeDefRef}.bonded_type = false;|]
    where
        fieldClassLocal = declName ++ "StructClass"
        fieldStructDefLocal = declName ++ "StructDef"
        fieldClassName = qualifiedName java decl

initTypeDef java t typeDefRef _ =
    [lt|#{typeDefRef}.id = #{fieldTypeName t};
        #{typeDefRef}.struct_def = 0;
        #{elementTypeDef java t elementTypeDefRef "unusedElement"}
        #{keyTypeDef java t keyTypeDefRef "unusedKey"}
        #{typeDefRef}.bonded_type = false;|]
    where
        elementTypeDefRef = [lt|#{typeDefRef}.element|]
        keyTypeDefRef = [lt|#{typeDefRef}.key|]

elementTypeDef :: MappingContext -> Type -> Text -> String -> Text
elementTypeDef java (BT_List e) typeDefRef structClassName =
    [lt|#{typeDefRef} = new com.microsoft.bond.TypeDef();
        #{initTypeDef java e typeDefRef structClassName}|]
-- Every other type with an element is the same as BT_List.
elementTypeDef j (BT_Maybe e) t s = elementTypeDef j (BT_List e) t s
elementTypeDef j (BT_Vector e) t s = elementTypeDef j (BT_List e) t s
elementTypeDef j (BT_Nullable e) t s = elementTypeDef j (BT_List e) t s
elementTypeDef j (BT_Set e) t s = elementTypeDef j (BT_List e) t s
elementTypeDef j (BT_Map _ v) t s = elementTypeDef j (BT_List v) t s
elementTypeDef _ _ typeDefRef _ = [lt|#{typeDefRef} = null;|]

keyTypeDef :: MappingContext -> Type -> Text -> String -> Text
keyTypeDef java (BT_Map k _) typeDefRef structClassName =
    [lt|#{typeDefRef} = new com.microsoft.bond.TypeDef();
        #{initTypeDef java k typeDefRef structClassName}|]
keyTypeDef _ _ typeDefRef _ = [lt|#{typeDefRef} = null;|]
