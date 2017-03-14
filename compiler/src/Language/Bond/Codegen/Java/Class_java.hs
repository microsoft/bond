-- Copyright (c) Microsoft. All rights reserved.
-- Licensed under the MIT license. See LICENSE file in the project root
-- for full license information.

{-# LANGUAGE QuasiQuotes, OverloadedStrings, RecordWildCards #-}

module Language.Bond.Codegen.Java.Class_java
    ( class_java
    , JavaFieldMapping(..)
    ) where

import Prelude
import Data.Text.Lazy (Text)
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

// Standard imports used by Bond.
import java.math.BigInteger;
import java.util.*;
import java.io.IOException;

// Bond lib imports.
import com.microsoft.bond.*;
import com.microsoft.bond.protocol.*;

// Imports for other generated code.

#{typeDefinition declaration}
|]
    where
        javaType = getTypeName java
        javaPackage = sepBy "." toText $ getNamespace java

        -- struct -> Java class
        typeDefinition Struct {..} = [lt|
public class #{declName}#{params}#{maybe interface baseClass structBase} {
    #{doubleLineSep 1 publicField structFields}

#{marshal_ProtocolWriter declaration}
}|]
            where
                interface = [lt| implements BondSerializable|]
                params = angles $ sepBy ", " paramName declParams
                baseClass x = [lt| extends #{javaType x}()|]
                javaDefault = Java.defaultValue java

                -- FIXME: nullable<int32> -> Integer?
                publicField f@Field {..} = [lt|public #{javaType fieldType} #{fieldName} #{optional initializerValue $ javaDefault f};|]

                initializerValue x = [lt|= #{x}|]

        typeDefinition _ = mempty

marshal_ProtocolWriter :: Declaration -> Text
marshal_ProtocolWriter declaration = [lt|
    @Override
    public void marshal(ProtocolWriter writer) throws IOException {
        writer.writeVersion();

// FIXME: Where is my metadata?
        writer.writeStructBegin(null);
        #{newlineSepEnd 2 writeField fields}
        writer.writeStructEnd();
    }|]
    where
        fields = structFields declaration
        writeField Field {..} = [lt|
// FIXME: Where is my metadata?
        writer.writeFieldBegin(#{fieldTypeName}, #{fieldOrdinal}, null);
        #{fieldValue}
        writer.writeFieldEnd();|]
            where
                fieldTypeName = case fieldType of
                    BT_Int8    -> [lt|BondDataType.BT_INT8|]
                    BT_Int16   -> [lt|BondDataType.BT_INT16|]
                    BT_Int32   -> [lt|BondDataType.BT_INT32|]
                    BT_Int64   -> [lt|BondDataType.BT_INT64|]
                    BT_UInt8   -> [lt|BondDataType.BT_UINT8|]
                    BT_UInt16  -> [lt|BondDataType.BT_UINT16|]
                    BT_UInt32  -> [lt|BondDataType.BT_UINT32|]
                    BT_UInt64  -> [lt|BondDataType.BT_UINT64|]
                    BT_Float   -> [lt|BondDataType.BT_FLOAT|]
                    BT_Double  -> [lt|BondDataType.BT_DOUBLE|]
                    BT_Bool    -> [lt|BondDataType.BT_BOOL|]
                    BT_String  -> [lt|BondDataType.BT_STRING|]
                    BT_WString -> [lt|BondDataType.BT_WSTRING|]
                    BT_Blob    -> [lt|BondDataType.BT_BLOB|]
                    -- FIXME: Totally broken, but builds.
                    _          -> [lt|BondDataType.BT_UNAVAILABLE|]
                fieldValue = case fieldType of
                    BT_Int8    -> [lt|writer.writeInt8(this.#{fieldName});|]
                    BT_Int16   -> [lt|writer.writeInt16(this.#{fieldName});|]
                    BT_Int32   -> [lt|writer.writeInt32(this.#{fieldName});|]
                    BT_Int64   -> [lt|writer.writeInt64(this.#{fieldName});|]
                    BT_UInt8   -> [lt|writer.writeUInt8(this.#{fieldName});|]
                    BT_UInt16  -> [lt|writer.writeUInt16(this.#{fieldName});|]
                    BT_UInt32  -> [lt|writer.writeUInt32(this.#{fieldName});|]
                    BT_UInt64  -> [lt|writer.writeUInt64(this.#{fieldName});|]
                    BT_Float   -> [lt|writer.writeFloat(this.#{fieldName});|]
                    BT_Double  -> [lt|writer.writeDouble(this.#{fieldName});|]
                    BT_Bool    -> [lt|writer.writeBool(this.#{fieldName});|]
                    BT_String  -> [lt|writer.writeString(this.#{fieldName});|]
                    BT_WString -> [lt|writer.writeWString(this.#{fieldName});|]
                    BT_Blob    -> [lt|writer.writeBytes(this.#{fieldName});|]
                    _          -> [lt|// FIXME: Not implemented.|]
