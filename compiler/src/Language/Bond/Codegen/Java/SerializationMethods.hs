-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.SerializationMethods
    ( marshal_ProtocolWriter
    , serialize_ProtocolWriter
    , serializeFields
    , deserializationHelperMembers
    , deserialize_ProtocolWriter
    , deserializeFields
    , fieldTypeName
    ) where

import Prelude
import Data.Monoid
import Data.Text.Lazy (Text)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import Language.Bond.Codegen.Java.StaticFields
import Language.Bond.Codegen.Java.Util

--
-- serialization
--
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
        this.serializeFields(writer);
        writer.writeStructEnd();
    }|]

serializeFields :: MappingContext -> Declaration -> Text
serializeFields java declaration = [lt|
    protected void serializeFields(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        #{serializeBase java (structBase declaration)}
        #{newlineSepEnd 2 writeField fields}
    }|]
    where
        fields = structFields declaration
        writeField field@Field {..} = [lt|
        writer.writeFieldBegin(#{fieldTypeName fieldType}, #{fieldOrdinal}, #{fieldDefMember field}.metadata);
        #{writeFieldValue java fieldType fieldName 0}
        writer.writeFieldEnd();|]

serializeBase :: MappingContext -> Maybe Type -> Text
serializeBase _ Nothing = mempty
serializeBase java (Just (BT_UserDefined baseDecl _)) =
    [lt|writer.writeBaseBegin(#{qualifiedBase}.#{schemaDefMember}.structs.get(0).metadata);
        super.serializeFields(writer);
        writer.writeBaseEnd();|]
    where
        qualifiedBase = qualifiedName java baseDecl
serializeBase _ _ = error "Java: base type was not a UserDefined"

writeFieldValue :: MappingContext -> Type -> String -> Int -> Text
writeFieldValue java fieldType fieldName depth =
    writeValue java fieldType ("this." ++ fieldName) depth

writeValue :: MappingContext -> Type -> String -> Int -> Text
writeValue java fieldType varName depth = case fieldType of
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
    BT_Blob                  -> writeBlob varName
    BT_Nullable e            -> writeNullable java e varName depth
    BT_List e                -> writeSequence java e varName depth
    BT_Vector e              -> writeSequence java e varName depth
    BT_Set e                 -> writeSequence java e varName depth
    BT_Map k v               -> writeMap java k v varName depth
    BT_UserDefined Enum {} _ -> [lt|writer.writeInt32(#{varName}.value);|]
    -- FIXME: Recursive types will cause infinite recursion.
    BT_UserDefined _ _       -> [lt|#{varName}.serialize(writer);|]
    _                        -> [lt|// FIXME: Not implemented.|]

writeBlob :: String -> Text
writeBlob fieldName =
    [lt|writer.writeContainerBegin(#{fieldName}.length, #{fieldTypeName BT_Int8});
        writer.writeBytes(#{fieldName});
        writer.writeContainerEnd();|]

writeNullable :: MappingContext -> Type -> String -> Int -> Text
writeNullable java elemType fieldName depth =
    [lt|if (#{fieldName} == null) {
            writer.writeContainerBegin(0, #{fieldTypeName elemType});
        } else {
            writer.writeContainerBegin(1, #{fieldTypeName elemType});
            #{writeValue java elemType fieldName (depth + 1)}
        }
        writer.writeContainerEnd();|]

writeSequence :: MappingContext -> Type -> String -> Int -> Text
writeSequence java elemType fieldName depth =
    [lt|writer.writeContainerBegin(#{fieldName}.size(), #{fieldTypeName elemType});
        for (#{getTypeName java elemType} #{iterLocal} : #{fieldName}) {
            #{writeValue java elemType iterLocal (depth + 1)}
        }
        writer.writeContainerEnd();|]
    where
        iterLocal = "e" ++ show depth

writeMap :: MappingContext -> Type -> Type -> String -> Int -> Text
writeMap java keyType valueType fieldName depth =
    [lt|writer.writeContainerBegin(#{fieldName}.size(), #{fieldTypeName keyType}, #{fieldTypeName valueType});
        for (java.util.Map.Entry<#{getTypeName javaBoxed keyType}, #{getTypeName javaBoxed valueType}> #{iterLocal} : #{fieldName}.entrySet()) {
            #{writeValue java keyType iterLocalKey (depth + 1)}
            #{writeValue java valueType iterLocalValue (depth + 1)}
        }
        writer.writeContainerEnd();|]
    where
        javaBoxed = java { typeMapping = javaBoxedTypeMapping }
        iterLocal = "e" ++ show depth
        iterLocalKey = iterLocal ++ ".getKey()"
        iterLocalValue = iterLocal ++ ".getValue()"

--
-- deserialization
--
readFieldResultMember :: String
readFieldResultMember = "__readFieldResult"

readContainerResultMember :: String
readContainerResultMember = "__readContainerResult"

deserializationHelperMembers :: Text
deserializationHelperMembers = [lt|
    private final #{readFieldResultType} #{readFieldResultMember} = new #{readFieldResultType}();
    private final #{readContainerResultType} #{readContainerResultMember} = new #{readContainerResultType}();|]
    where
        readFieldResultType =
            [lt|com.microsoft.bond.protocol.TaggedProtocolReader.ReadFieldResult|]
        readContainerResultType =
            [lt|com.microsoft.bond.protocol.TaggedProtocolReader.ReadContainerResult|]

deserialize_ProtocolWriter :: MappingContext -> Declaration -> Text
deserialize_ProtocolWriter java declaration = [lt|
    @Override
    public void deserialize(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        initSchema();

        reader.readStructBegin();
        this.deserializeFields(reader);
        reader.readStructEnd();
    }|]

deserializeFields :: MappingContext -> Declaration -> Text
deserializeFields java declaration = [lt|
    protected void deserializeFields(com.microsoft.bond.protocol.TaggedProtocolReader reader) throws java.io.IOException {
        #{deserializeBase java (structBase declaration)}
        #{newlineSepEnd 2 readField fields}
    }|]
    where
        fields = structFields declaration
        -- TODO: Check that field id and type are as expected.
        readField field@Field {..} = [lt|
        reader.readFieldBegin(this.#{readFieldResultMember});
        #{readFieldValue java fieldType fieldName 0}
        reader.readFieldEnd();|]

deserializeBase :: MappingContext -> Maybe Type -> Text
deserializeBase _ Nothing = mempty
deserializeBase java (Just (BT_UserDefined baseDecl _)) =
    [lt|reader.readBaseBegin();
        super.deserializeFields(reader);
        reader.readBaseEnd();|]
    where
        qualifiedBase = qualifiedName java baseDecl
deserializeBase _ _ = error "Java: base type was not a UserDefined"

readFieldValue :: MappingContext -> Type -> String -> Int -> Text
readFieldValue java fieldType fieldName depth =
    readValue java fieldType ("this." ++ fieldName) depth

readValue :: MappingContext -> Type -> String -> Int -> Text
readValue java fieldType varName depth = case fieldType of
    BT_Int8                  -> [lt|#{varName} = reader.readInt8();|]
    BT_Int16                 -> [lt|#{varName} = reader.readInt16();|]
    BT_Int32                 -> [lt|#{varName} = reader.readInt32();|]
    BT_Int64                 -> [lt|#{varName} = reader.readInt64();|]
    BT_UInt8                 -> [lt|#{varName} = reader.readUInt8();|]
    BT_UInt16                -> [lt|#{varName} = reader.readUInt16();|]
    BT_UInt32                -> [lt|#{varName} = reader.readUInt32();|]
    BT_UInt64                -> [lt|#{varName} = reader.readUInt64();|]
    BT_Float                 -> [lt|#{varName} = reader.readFloat();|]
    BT_Double                -> [lt|#{varName} = reader.readDouble();|]
    BT_Bool                  -> [lt|#{varName} = reader.readBool();|]
    BT_String                -> [lt|#{varName} = reader.readString();|]
    BT_WString               -> [lt|#{varName} = reader.readWString();|]
    BT_Blob                  -> readBlob varName
    BT_Nullable e            -> readNullable java e varName depth
    BT_List e                -> readSequence java e varName depth
    BT_Vector e              -> readSequence java e varName depth
    BT_Set e                 -> readSequence java e varName depth
    BT_Map k v               -> readMap java k v varName depth
    BT_UserDefined Enum {} _ ->
        [lt|#{varName} = new #{getTypeName java fieldType}(reader.readInt32());|]
    -- FIXME: Recursive types will cause infinite recursion.
    BT_UserDefined _ _       ->
        [lt|#{varName} = new #{getTypeName java fieldType}(); #{varName}.deserialize(reader);|]
    _                        -> [lt|// FIXME: Not implemented.|]

readBlob :: String -> Text
readBlob fieldName =
    [lt|reader.readListBegin(this.#{readContainerResultMember});
        #{fieldName} = reader.readBytes(#{readContainerResultMember}.count);
        reader.readContainerEnd();|]

readNullable :: MappingContext -> Type -> String -> Int -> Text
readNullable java elemType fieldName depth =
    -- TODO: What if .count > 1?
    [lt|reader.readListBegin(this.#{readContainerResultMember});
        if (this.#{readContainerResultMember}.count == 0) {
            #{fieldName} = null;
        } else {
            #{readValue java elemType fieldName (depth + 1)}
        }
        reader.readContainerEnd();|]

readSequence :: MappingContext -> Type -> String -> Int -> Text
readSequence java elemType fieldName depth =
    [lt|reader.readListBegin(this.#{readContainerResultMember});
        {
            long #{countLocal} = this.#{readContainerResultMember}.count;
            for (long #{iN} = 0; #{iN} < #{countLocal}; #{iN}++) {
                #{getTypeName java elemType} #{elemLocal};
                #{readValue java elemType elemLocal (depth + 1)}
                #{fieldName}.add(#{elemLocal});
            }
        }
        reader.readContainerEnd();|]
    where
        iN = "i" ++ show depth
        countLocal = "count" ++ show depth
        elemLocal = "e" ++ show depth

readMap :: MappingContext -> Type -> Type -> String -> Int -> Text
readMap java keyType valueType fieldName depth =
    [lt|reader.readMapBegin(#{readContainerResultMember});
        {
            long #{countLocal} = #{readContainerResultMember}.count;
            for (long #{iN} = 0; #{iN} < #{countLocal}; #{iN}++) {
                #{getTypeName java keyType} #{keyLocal};
                #{getTypeName java valueType} #{valueLocal};
                #{readValue java keyType keyLocal (depth + 1)}
                #{readValue java valueType valueLocal (depth + 1)}
                #{fieldName}.put(#{keyLocal}, #{valueLocal});
            }
        }
        reader.readContainerEnd();|]
    where
        iN = "i" ++ show depth
        countLocal = "count" ++ show depth
        keyLocal = "k" ++ show depth
        valueLocal = "v" ++ show depth
