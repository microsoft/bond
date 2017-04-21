-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.SerializationMethods
    ( marshal_ProtocolWriter
    , serialize_ProtocolWriter
    , fieldTypeName
    ) where

import Prelude
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Java.StaticFields
import Language.Bond.Codegen.Java.Util

marshal_ProtocolWriter :: Text
marshal_ProtocolWriter = [lt|
    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }|]

serialize_ProtocolWriter :: MappingContext -> Declaration -> Text
serialize_ProtocolWriter java declaration = [lt|
    @Override
    public void serialize(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        initSchema();

        writer.writeStructBegin(#{schemaDefMember}.structs.get(0).metadata);
        #{newlineSepEnd 2 writeField fields}
        writer.writeStructEnd();
    }|]
    where
        fields = structFields declaration
        writeField field@Field {..} = [lt|
        writer.writeFieldBegin(#{fieldTypeName fieldType}, #{fieldOrdinal}, #{fieldDefMember field}.metadata);
        #{writeFieldValue java fieldType fieldName}
        writer.writeFieldEnd();|]

writeFieldValue :: MappingContext -> Type -> String -> Text
writeFieldValue java fieldType fieldName = writeValue java fieldType ("this." ++ fieldName)

writeValue :: MappingContext -> Type -> String -> Text
writeValue java fieldType varName = case fieldType of
    BT_Int8                  -> [lt|writer.writeInt8(#{varName});|]
    BT_Int16                 -> [lt|writer.writeInt16(#{varName});|]
    BT_Int32                 -> [lt|writer.writeInt32(#{varName});|]
    BT_Int64                 -> [lt|writer.writeInt64(#{varName});|]
    BT_UInt8                 -> [lt|writer.writeUInt8(#{varName});|]
    BT_UInt16                -> [lt|writer.writeUInt16(#{varName});|]
    BT_UInt32                -> [lt|writer.writeUInt32(#{varName});|]
    BT_UInt64                -> [lt|writer.writeUInt64(#{varName});|]
    BT_Float                 -> [lt|writer.writeFloat(#{varName});|]
    BT_Double                -> [lt|writer.writeDouble(#{varName});|]
    BT_Bool                  -> [lt|writer.writeBool(#{varName});|]
    BT_String                -> [lt|writer.writeString(#{varName});|]
    BT_WString               -> [lt|writer.writeWString(#{varName});|]
    BT_Blob                  -> [lt|writer.writeBytes(#{varName});|]
    BT_Nullable e            -> writeNullable java e varName
    BT_List e                -> writeSequence java e varName
    BT_Vector e              -> writeSequence java e varName
    BT_Set e                 -> writeSequence java e varName
    BT_Map k v               -> writeMap java k v varName
    BT_UserDefined Enum {} _ -> [lt|writer.writeInt32(#{varName}.value);|]
    -- FIXME: Recursive types will cause infinite recursion.
    BT_UserDefined _ _       -> [lt|#{varName}.serialize(writer);|]
    _                        -> [lt|// FIXME: Not implemented.|]

writeNullable :: MappingContext -> Type -> String -> Text
writeNullable java elemType fieldName =
    [lt|if (#{fieldName} == null) {
            writer.writeContainerBegin(0, #{fieldTypeName elemType});
        } else {
            writer.writeContainerBegin(1, #{fieldTypeName elemType});
            #{writeValue java elemType fieldName}
        }
        writer.writeContainerEnd();|]

writeSequence :: MappingContext -> Type -> String -> Text
writeSequence java elemType fieldName = [lt|writer.writeContainerBegin(#{fieldName}.size(), #{fieldTypeName elemType});
        for (#{getTypeName java elemType} e : #{fieldName}) {
            #{writeValue java elemType "e"}
        }
        writer.writeContainerEnd();|]

writeMap :: MappingContext -> Type -> Type -> String -> Text
writeMap java keyType valueType fieldName = [lt|writer.writeContainerBegin(#{fieldName}.size(), #{fieldTypeName keyType}, #{fieldTypeName valueType});
        for (java.util.Map.Entry<#{getTypeName javaBoxed keyType}, #{getTypeName javaBoxed valueType}> e : #{fieldName}.entrySet()) {
            #{writeValue java keyType "e.getKey()"}
            #{writeValue java valueType "e.getValue()"}
        }
        writer.writeContainerEnd();|]
    where javaBoxed = java { typeMapping = javaBoxedTypeMapping }
