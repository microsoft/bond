// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.testmodel;

import com.microsoft.bond.*;
import com.microsoft.bond.protocol.FastBinaryReader;
import com.microsoft.bond.protocol.FastBinaryWriter;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;

public class Tests {

    @Test
    @SuppressWarnings("unchecked")
    public void testSerializeDeserialize() throws Exception {
        E<Boolean> obj = new E<Boolean>(BondTypes.BOOL);
        obj.bs1.t = "obj.bs1.t";
        obj.bs1.ny = "obj.bs1.ny";
        obj.bs1.at = new A<String, String>(A.struct.resolve(BondTypes.STRING, BondTypes.STRING));
        obj.bs1.at.lx.add("obj.bs1.at.lx[0]");
        obj.bs1.at.lx.add("obj.bs1.at.lx[1]");
        obj.bs1.at.lx.add("obj.bs1.at.lx[2]");
        obj.bs1.at.nax32 = new A<String, Integer>(A.struct.resolve(BondTypes.STRING, BondTypes.INT32));
        obj.bs1.at.nax32.lx.add("obj.bs1.at.nax32.lx[0]");
        obj.bs1.at.nax32.lx.add("obj.bs1.at.nax32.lx[1]");
        obj.bs1.at.nax32.nax32 = new A<String, Integer>(A.struct.resolve(BondTypes.STRING, BondTypes.INT32));
        obj.bs1.at.nax32.nax32.lx.add("obj.bs1.at.nax32.nax32.lx[0]");
        obj.bs1.at.nax32.nax32.nax32 = null;
        obj.bs1.at.nax32.nax32.ly.add(0);
        obj.bs1.at.nax32.nax32.ly.add(1);
        obj.bs1.at.nax32.nax32.ly.add(2);
        obj.bs1.at.nax32.nax32.ly.add(3);
        obj.bs1.at.nax32.nax32.ly.add(4);
        obj.bs1.at.nax32.nax32.nay64 = new A<Integer, Long>(A.struct.resolve(BondTypes.INT32, BondTypes.INT64));
        obj.bs1.at.nax32.nax32.nay64.ly.add(Long.MAX_VALUE);
        obj.bs1.at.nax32.nax32.nay64.ly.add(Long.MIN_VALUE);

        byte[] bytes = serialize(obj, (StructBondType<? super E<Boolean>>) obj.getStruct());
        E<Boolean> deserializedObj = deserialize(bytes, (StructBondType<E<Boolean>>) obj.getStruct());

        TestHelper.assertStructMemberwiseEquals(obj, deserializedObj);
    }

    private static <TStruct extends BondSerializable> byte[] serialize(
            TStruct obj, StructBondType<? super TStruct> struct) throws IOException {
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        FastBinaryWriter writer = new FastBinaryWriter(baos, (short) 1);
        Serializer<TStruct> serializer = new Serializer<TStruct>();
        serializer.serialize(obj, writer);
        return baos.toByteArray();
    }

    private static <TStruct extends BondSerializable> TStruct deserialize(
            byte[] bytes, StructBondType<TStruct> struct) throws IOException {
        ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
        FastBinaryReader reader = new FastBinaryReader(bais, (short) 1);
        Deserializer<TStruct> deserializer = new Deserializer<TStruct>(struct);
        return deserializer.deserialize(reader);
    }
}
