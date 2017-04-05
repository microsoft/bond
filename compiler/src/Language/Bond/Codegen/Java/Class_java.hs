-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Class_java
    ( class_java
    , JavaFieldMapping(..)
    ) where

import Prelude
import Data.Text.Lazy (Text, pack)
import Text.Shakespeare.Text
import Language.Bond.Syntax.Types
import Language.Bond.Util
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Util
import qualified Language.Bond.Codegen.Java.Util as Java

-- field -> public field
data JavaFieldMapping = JavaPublicFields deriving Eq

-- Template for struct -> Java class.
class_java :: MappingContext -> [Import] -> Declaration -> Text
class_java java _ declaration = [lt|
package #{javaPackage};

#{typeDefinition declaration}
|]
    where
        javaType = getTypeName java
        javaPackage = sepBy "." toText $ getNamespace java

        -- struct -> Java class
        typeDefinition Struct {..} = [lt|
#{Java.generatedClassAnnotations}
public class #{declName}#{params}#{maybe interface baseClass structBase} {
    #{doubleLineSep 1 publicField structFields}
#{serialize_ProtocolWriter java declaration}
#{marshal_ProtocolWriter}
}|]
            where
                interface = [lt| implements com.microsoft.bond.BondSerializable|]
                params = angles $ sepBy ", " paramName declParams
                baseClass x = [lt| extends #{javaType x}()|]
                javaDefault = Java.defaultValue java

                -- FIXME: nullable<int32> -> Integer?
                publicField f@Field {..} = [lt|public #{javaType fieldType} #{fieldName} #{optional initializerValue $ javaDefault f};|]

                initializerValue x = [lt|= #{x}|]

        typeDefinition _ = mempty


serialize_ProtocolWriter :: MappingContext -> Declaration -> Text
serialize_ProtocolWriter java declaration = [lt|
    @Override
    public void serialize(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
// FIXME: Where is my metadata?
        writer.writeStructBegin(null);
        #{newlineSepEnd 2 writeField fields}
        writer.writeStructEnd();
    }|]
    where
        fields = structFields declaration
        writeField Field {..} = [lt|
// FIXME: Where is my metadata?
        writer.writeFieldBegin(#{fieldTypeName fieldType}, #{fieldOrdinal}, null);
        #{writeFieldValue java fieldType fieldName}
        writer.writeFieldEnd();|]

fieldTypeName :: Type -> Text
fieldTypeName fieldType = pack $ "com.microsoft.bond.BondDataType." ++ case fieldType of
    BT_Int8                  -> "BT_INT8"
    BT_Int16                 -> "BT_INT16"
    BT_Int32                 -> "BT_INT32"
    BT_Int64                 -> "BT_INT64"
    BT_UInt8                 -> "BT_UINT8"
    BT_UInt16                -> "BT_UINT16"
    BT_UInt32                -> "BT_UINT32"
    BT_UInt64                -> "BT_UINT64"
    BT_Float                 -> "BT_FLOAT"
    BT_Double                -> "BT_DOUBLE"
    BT_Bool                  -> "BT_BOOL"
    BT_String                -> "BT_STRING"
    BT_WString               -> "BT_WSTRING"
    BT_Blob                  -> "BT_BLOB"
    BT_List _                -> "BT_LIST"
    BT_Vector _              -> "BT_LIST"
    BT_Set _                 -> "BT_SET"
    BT_Map _ _               -> "BT_MAP"
    BT_UserDefined Enum {} _ -> "BT_INT32"
    BT_UserDefined _ _       -> "BT_STRUCT"
    -- FIXME: Marker for unsupported types that compiles.
    _                        -> "BT_UNAVAILABLE"

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
    BT_List e                -> writeSequence java e varName
    BT_Vector e              -> writeSequence java e varName
    BT_Set e                 -> writeSequence java e varName
    BT_Map k v               -> writeMap java k v varName
    BT_UserDefined Enum {} _ -> [lt|writer.writeInt32(#{varName}.value);|]
    -- FIXME: Recursive types will cause infinite recursion.
    BT_UserDefined _ _       -> [lt|#{varName}.serialize(writer);|]
    _                        -> [lt|// FIXME: Not implemented.|]


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

marshal_ProtocolWriter :: Text
marshal_ProtocolWriter = [lt|
    @Override
    public void marshal(com.microsoft.bond.protocol.ProtocolWriter writer) throws java.io.IOException {
        writer.writeVersion();
        serialize(writer);
    }|]
