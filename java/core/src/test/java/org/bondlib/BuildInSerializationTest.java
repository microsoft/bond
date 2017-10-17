// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.bondlib.test.*;
import org.junit.Assert;
import org.junit.Test;

import java.io.*;

/**
 * Tests for Java's build-in serialization per {@link java.io.Serializable} specification.
 * Only classes that customize the default serialization (i.e. add any serialization code)
 * are tested here. Classes that only use the default object read/write rules do not add any
 * executable code (except perhaps the serialVersionUID field) and thus do not need such tests.
 */
public class BuildInSerializationTest {

    @Test
    public void testSerializeAndDeserialize() throws IOException {
        // test the tester, at least to make sure it doesn't return the same reference
        int[] inObj = new int[]{1, 1, 2, 3, 5, 8, 13, 21, 34, 55};
        int[] outObj = TestHelper.serializeAndDeserialize(inObj);
        Assert.assertNotSame(inObj, outObj);
        Assert.assertArrayEquals(inObj, outObj);
    }

    @Test
    public void testUInt8BondType_resolvesToSingleton() throws IOException {
        UInt8BondType deserializedBondType = TestHelper.serializeAndDeserialize(UInt8BondType.INSTANCE);
        Assert.assertSame(UInt8BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testUInt16BondType_resolvesToSingleton() throws IOException {
        UInt16BondType deserializedBondType = TestHelper.serializeAndDeserialize(UInt16BondType.INSTANCE);
        Assert.assertSame(UInt16BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testUInt32BondType_resolvesToSingleton() throws IOException {
        UInt32BondType deserializedBondType = TestHelper.serializeAndDeserialize(UInt32BondType.INSTANCE);
        Assert.assertSame(UInt32BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testUInt64BondType_resolvesToSingleton() throws IOException {
        UInt64BondType deserializedBondType = TestHelper.serializeAndDeserialize(UInt64BondType.INSTANCE);
        Assert.assertSame(UInt64BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testInt8BondType_resolvesToSingleton() throws IOException {
        Int8BondType deserializedBondType = TestHelper.serializeAndDeserialize(Int8BondType.INSTANCE);
        Assert.assertSame(Int8BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testInt16BondType_resolvesToSingleton() throws IOException {
        Int16BondType deserializedBondType = TestHelper.serializeAndDeserialize(Int16BondType.INSTANCE);
        Assert.assertSame(Int16BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testInt32BondType_resolvesToSingleton() throws IOException {
        Int32BondType deserializedBondType = TestHelper.serializeAndDeserialize(Int32BondType.INSTANCE);
        Assert.assertSame(Int32BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testInt64BondType_resolvesToSingleton() throws IOException {
        Int64BondType deserializedBondType = TestHelper.serializeAndDeserialize(Int64BondType.INSTANCE);
        Assert.assertSame(Int64BondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testBoolBondType_resolvesToSingleton() throws IOException {
        BoolBondType deserializedBondType = TestHelper.serializeAndDeserialize(BoolBondType.INSTANCE);
        Assert.assertSame(BoolBondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testFloatBondType_resolvesToSingleton() throws IOException {
        FloatBondType deserializedBondType = TestHelper.serializeAndDeserialize(FloatBondType.INSTANCE);
        Assert.assertSame(FloatBondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testDoubleBondType_resolvesToSingleton() throws IOException {
        DoubleBondType deserializedBondType = TestHelper.serializeAndDeserialize(DoubleBondType.INSTANCE);
        Assert.assertSame(DoubleBondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testStringBondType_resolvesToSingleton() throws IOException {
        StringBondType deserializedBondType = TestHelper.serializeAndDeserialize(StringBondType.INSTANCE);
        Assert.assertSame(StringBondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testWStringBondType_resolvesToSingleton() throws IOException {
        WStringBondType deserializedBondType = TestHelper.serializeAndDeserialize(WStringBondType.INSTANCE);
        Assert.assertSame(WStringBondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testBlobBondType_resolvesToSingleton() throws IOException {
        BlobBondType deserializedBondType = TestHelper.serializeAndDeserialize(BlobBondType.INSTANCE);
        Assert.assertSame(BlobBondType.INSTANCE, deserializedBondType);
    }

    @Test
    public void testEnumBondType_resolvesToSingleton() throws IOException {
        EnumBondType deserializedBondType = TestHelper.serializeAndDeserialize(MonthOfYear.BOND_TYPE);
        Assert.assertSame(MonthOfYear.BOND_TYPE, deserializedBondType);
    }

    @Test
    public void testNullableBondType_resolvesToCached() throws IOException {
        NullableBondType[] inputBondTypes = new NullableBondType[]{
                BondType.nullableOf(BondTypes.INT32),
                BondType.nullableOf(BondTypes.STRING),
                BondType.nullableOf(MonthOfYear.BOND_TYPE),
                BondType.nullableOf(Empty.BOND_TYPE),
                BondType.nullableOf(Base.BOND_TYPE),
                BondType.nullableOf(Derived.BOND_TYPE),
                BondType.nullableOf(GenericValueContainer.BOND_TYPE.makeGenericType(BondTypes.INT32)),
                BondType.nullableOf(BondType.nullableOf(BondTypes.INT32)),
                BondType.nullableOf(BondType.vectorOf(BondTypes.INT32)),
                BondType.nullableOf(BondType.listOf(BondTypes.INT32)),
                BondType.nullableOf(BondType.setOf(BondTypes.INT32)),
                BondType.nullableOf(BondType.mapOf(BondTypes.INT32, BondTypes.STRING))
        };
        for (NullableBondType inputBondType : inputBondTypes) {
            NullableBondType deserializedBondType = TestHelper.serializeAndDeserialize(inputBondType);
            Assert.assertSame(inputBondType, deserializedBondType);
        }
    }

    @Test
    public void testVectorBondType_resolvesToCached() throws IOException {
        VectorBondType[] inputBondTypes = new VectorBondType[]{
                BondType.vectorOf(BondTypes.INT32),
                BondType.vectorOf(BondTypes.STRING),
                BondType.vectorOf(MonthOfYear.BOND_TYPE),
                BondType.vectorOf(Empty.BOND_TYPE),
                BondType.vectorOf(Base.BOND_TYPE),
                BondType.vectorOf(Derived.BOND_TYPE),
                BondType.vectorOf(GenericValueContainer.BOND_TYPE.makeGenericType(BondTypes.INT32)),
                BondType.vectorOf(BondType.nullableOf(BondTypes.INT32)),
                BondType.vectorOf(BondType.vectorOf(BondTypes.INT32)),
                BondType.vectorOf(BondType.listOf(BondTypes.INT32)),
                BondType.vectorOf(BondType.setOf(BondTypes.INT32)),
                BondType.vectorOf(BondType.mapOf(BondTypes.INT32, BondTypes.STRING))
        };
        for (VectorBondType inputBondType : inputBondTypes) {
            VectorBondType deserializedBondType = TestHelper.serializeAndDeserialize(inputBondType);
            Assert.assertSame(inputBondType, deserializedBondType);
        }
    }

    @Test
    public void testListBondType_resolvesToCached() throws IOException {
        ListBondType[] inputBondTypes = new ListBondType[]{
                BondType.listOf(BondTypes.INT32),
                BondType.listOf(BondTypes.STRING),
                BondType.listOf(MonthOfYear.BOND_TYPE),
                BondType.listOf(Empty.BOND_TYPE),
                BondType.listOf(Base.BOND_TYPE),
                BondType.listOf(Derived.BOND_TYPE),
                BondType.listOf(GenericValueContainer.BOND_TYPE.makeGenericType(BondTypes.INT32)),
                BondType.listOf(BondType.nullableOf(BondTypes.INT32)),
                BondType.listOf(BondType.vectorOf(BondTypes.INT32)),
                BondType.listOf(BondType.listOf(BondTypes.INT32)),
                BondType.listOf(BondType.setOf(BondTypes.INT32)),
                BondType.listOf(BondType.mapOf(BondTypes.INT32, BondTypes.STRING))
        };
        for (ListBondType inputBondType : inputBondTypes) {
            ListBondType deserializedBondType = TestHelper.serializeAndDeserialize(inputBondType);
            Assert.assertSame(inputBondType, deserializedBondType);
        }
    }

    @Test
    public void testSetBondType_resolvesToCached() throws IOException {
        SetBondType[] inputBondTypes = new SetBondType[]{
                BondType.setOf(BondTypes.INT32),
                BondType.setOf(BondTypes.STRING),
                BondType.setOf(MonthOfYear.BOND_TYPE)
        };
        for (SetBondType inputBondType : inputBondTypes) {
            SetBondType deserializedBondType = TestHelper.serializeAndDeserialize(inputBondType);
            Assert.assertSame(inputBondType, deserializedBondType);
        }
    }

    @Test
    public void testMapBondType_resolvesToCached() throws IOException {
        MapBondType[] inputBondTypes = new MapBondType[]{
                BondType.mapOf(BondTypes.INT32, BondTypes.INT32),
                BondType.mapOf(BondTypes.STRING, BondTypes.STRING),
                BondType.mapOf(MonthOfYear.BOND_TYPE, MonthOfYear.BOND_TYPE),
                BondType.mapOf(BondTypes.WSTRING, Empty.BOND_TYPE),
                BondType.mapOf(BondTypes.WSTRING, Base.BOND_TYPE),
                BondType.mapOf(BondTypes.WSTRING, Derived.BOND_TYPE),
                BondType.mapOf(BondTypes.WSTRING, GenericValueContainer.BOND_TYPE.makeGenericType(BondTypes.INT32)),
                BondType.mapOf(BondTypes.WSTRING, BondType.nullableOf(BondTypes.INT32)),
                BondType.mapOf(BondTypes.WSTRING, BondType.vectorOf(BondTypes.INT32)),
                BondType.mapOf(BondTypes.WSTRING, BondType.listOf(BondTypes.INT32)),
                BondType.mapOf(BondTypes.WSTRING, BondType.setOf(BondTypes.INT32)),
                BondType.mapOf(BondTypes.WSTRING, BondType.mapOf(BondTypes.INT32, BondTypes.STRING))
        };
        for (MapBondType inputBondType : inputBondTypes) {
            MapBondType deserializedBondType = TestHelper.serializeAndDeserialize(inputBondType);
            Assert.assertSame(inputBondType, deserializedBondType);
        }
    }

    @Test
    public void testStructBondType_resolvesToCached() throws IOException {
        StructBondType[] inputBondTypes = new StructBondType[]{
                Empty.BOND_TYPE,
                Base.BOND_TYPE,
                Derived.BOND_TYPE,
                GenericValueContainer.BOND_TYPE.makeGenericType(BondTypes.INT32),
                GenericValueContainer.BOND_TYPE.makeGenericType(BondTypes.STRING),
                GenericValueContainer.BOND_TYPE.makeGenericType(MonthOfYear.BOND_TYPE),
                GenericValueContainer.BOND_TYPE.makeGenericType(BondType.nullableOf(BondTypes.INT32)),
                GenericValueContainer.BOND_TYPE.makeGenericType(BondType.vectorOf(BondTypes.INT32)),
                GenericValueContainer.BOND_TYPE.makeGenericType(BondType.listOf(BondTypes.INT32)),
                GenericValueContainer.BOND_TYPE.makeGenericType(BondType.setOf(BondTypes.INT32)),
                GenericValueContainer.BOND_TYPE.makeGenericType(BondType.mapOf(BondTypes.INT32, BondTypes.STRING))
        };
        for (StructBondType inputBondType : inputBondTypes) {
            StructBondType deserializedBondType = TestHelper.serializeAndDeserialize(inputBondType);
            Assert.assertSame(inputBondType, deserializedBondType);
        }
    }
}
