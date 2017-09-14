// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.bondlib.test.*;
import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.util.List;

import static org.junit.Assert.*;

public class GenericContainerTest {

    @Test
    public void testGenericValueContainer() {
        genericValueContainerTestHelper(BondTypes.STRING, "test");
        genericValueContainerTestHelper(BondTypes.BOOL, true);
        genericValueContainerTestHelper(BondTypes.INT8, (byte)1);
        genericValueContainerTestHelper(BondTypes.INT32, Integer.MIN_VALUE);
        genericValueContainerTestHelper(BondTypes.UINT64, Long.MAX_VALUE);
        genericValueContainerTestHelper(BondTypes.DOUBLE, 3.14);

        genericValueContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.August);
        genericValueContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.get(100));

        Empty empty = new Empty();
        genericValueContainerTestHelper(Empty.BOND_TYPE, empty);

        Derived derived = new Derived();
        derived.baseInt = 123;
        derived.derivedInt = 456;
        genericValueContainerTestHelper(Derived.BOND_TYPE, derived);
    }

    private static <T> void genericValueContainerTestHelper(BondType<T> elementBondType, T elementValue) {
        StructBondType<GenericValueContainer<T>> containerBondType =
                GenericValueContainer.BOND_TYPE.makeGenericType(elementBondType);
        GenericValueContainer<T> container = new GenericValueContainer<T>(containerBondType);
        Assert.assertEquals(elementBondType.newDefaultValue(), container.valueField);
        container.valueField = elementValue;
        Assert.assertEquals(elementValue, container.valueField);
        marshalUnmarshalAndCompareUsingMultipleProtocols(container);
    }

    @Test
    public void testGenericNullableContainer() {
        genericNullableContainerTestHelper(BondTypes.STRING, "test");
        genericNullableContainerTestHelper(BondTypes.BOOL, true);
        genericNullableContainerTestHelper(BondTypes.INT8, (byte)1);
        genericNullableContainerTestHelper(BondTypes.INT32, Integer.MIN_VALUE);
        genericNullableContainerTestHelper(BondTypes.UINT64, Long.MAX_VALUE);
        genericNullableContainerTestHelper(BondTypes.DOUBLE, 3.14);

        genericNullableContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.August);
        genericNullableContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.get(100));

        Empty empty = new Empty();
        genericNullableContainerTestHelper(Empty.BOND_TYPE, empty);

        Derived derived = new Derived();
        derived.baseInt = 123;
        derived.derivedInt = 456;
        genericNullableContainerTestHelper(Derived.BOND_TYPE, derived);
    }

    private static <T> void genericNullableContainerTestHelper(BondType<T> elementBondType, T elementValue) {
        StructBondType<GenericNullableContainer<T>> containerBondType =
                GenericNullableContainer.BOND_TYPE.makeGenericType(elementBondType);
        GenericNullableContainer<T> container = new GenericNullableContainer<T>(containerBondType);
        Assert.assertEquals(null, container.nullableField);
        container.nullableField = elementValue;
        Assert.assertEquals(elementValue, container.nullableField);
        marshalUnmarshalAndCompareUsingMultipleProtocols(container);
    }

    @Test
    public void testGenericVectorContainer() {
        genericVectorContainerTestHelper(BondTypes.STRING, "test");
        genericVectorContainerTestHelper(BondTypes.BOOL, true);
        genericVectorContainerTestHelper(BondTypes.INT8, (byte)1);
        genericVectorContainerTestHelper(BondTypes.INT32, Integer.MIN_VALUE);
        genericVectorContainerTestHelper(BondTypes.UINT64, Long.MAX_VALUE);
        genericVectorContainerTestHelper(BondTypes.DOUBLE, 3.14);

        genericVectorContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.August);
        genericVectorContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.get(100));

        Empty empty = new Empty();
        genericVectorContainerTestHelper(Empty.BOND_TYPE, empty);

        Derived derived = new Derived();
        derived.baseInt = 123;
        derived.derivedInt = 456;
        genericVectorContainerTestHelper(Derived.BOND_TYPE, derived);
    }

    private static <T> void genericVectorContainerTestHelper(BondType<T> elementBondType, T elementValue) {
        StructBondType<GenericVectorContainer<T>> containerBondType =
                GenericVectorContainer.BOND_TYPE.makeGenericType(elementBondType);
        GenericVectorContainer<T> container = new GenericVectorContainer<T>(containerBondType);
        Assert.assertEquals(0, container.vectorField.size());
        container.vectorField.add(elementValue);
        Assert.assertEquals(1, container.vectorField.size());
        Assert.assertEquals(elementValue, container.vectorField.get(0));
        marshalUnmarshalAndCompareUsingMultipleProtocols(container);
    }

    @Test
    public void testGenericListContainer() {
        genericListContainerTestHelper(BondTypes.STRING, "test");
        genericListContainerTestHelper(BondTypes.BOOL, true);
        genericListContainerTestHelper(BondTypes.INT8, (byte)1);
        genericListContainerTestHelper(BondTypes.INT32, Integer.MIN_VALUE);
        genericListContainerTestHelper(BondTypes.UINT64, Long.MAX_VALUE);
        genericListContainerTestHelper(BondTypes.DOUBLE, 3.14);

        genericListContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.August);
        genericListContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.get(100));

        Empty empty = new Empty();
        genericListContainerTestHelper(Empty.BOND_TYPE, empty);

        Derived derived = new Derived();
        derived.baseInt = 123;
        derived.derivedInt = 456;
        genericListContainerTestHelper(Derived.BOND_TYPE, derived);
    }

    private static <T> void genericListContainerTestHelper(BondType<T> elementBondType, T elementValue) {
        StructBondType<GenericListContainer<T>> containerBondType =
                GenericListContainer.BOND_TYPE.makeGenericType(elementBondType);
        GenericListContainer<T> container = new GenericListContainer<T>(containerBondType);
        Assert.assertEquals(0, container.listField.size());
        container.listField.add(elementValue);
        Assert.assertEquals(1, container.listField.size());
        Assert.assertEquals(elementValue, container.listField.get(0));
        marshalUnmarshalAndCompareUsingMultipleProtocols(container);
    }

    @Test
    public void testGenericSetContainer() {
        genericSetContainerTestHelper(BondTypes.STRING, "test");
        genericSetContainerTestHelper(BondTypes.BOOL, true);
        genericSetContainerTestHelper(BondTypes.INT8, (byte)1);
        genericSetContainerTestHelper(BondTypes.INT32, Integer.MIN_VALUE);
        genericSetContainerTestHelper(BondTypes.UINT64, Long.MAX_VALUE);
        genericSetContainerTestHelper(BondTypes.DOUBLE, 3.14);

        genericSetContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.August);
        genericSetContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.get(100));
    }

    private static <T> void genericSetContainerTestHelper(BondType<T> elementBondType, T elementValue) {
        StructBondType<GenericSetContainer<T>> containerBondType =
                GenericSetContainer.BOND_TYPE.makeGenericType(elementBondType);
        GenericSetContainer<T> container = new GenericSetContainer<T>(containerBondType);
        Assert.assertEquals(0, container.setField.size());
        container.setField.add(elementValue);
        Assert.assertEquals(1, container.setField.size());
        Assert.assertTrue(container.setField.contains(elementValue));
        marshalUnmarshalAndCompareUsingMultipleProtocols(container);
    }

    @Test
    public void testGenericSetContainerWithInvalidElementType() {
        try {
            StructBondType<GenericSetContainer<Derived>> containerBondType =
                    GenericSetContainer.BOND_TYPE.makeGenericType(Derived.BOND_TYPE);
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }

        try {
            StructBondType<GenericSetContainer<List<Integer>>> containerBondType =
                    GenericSetContainer.BOND_TYPE.makeGenericType(BondType.listOf(BondTypes.INT32));
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }

        try {
            StructBondType<GenericSetContainer<Integer>> containerBondType =
                    GenericSetContainer.BOND_TYPE.makeGenericType(BondType.nullableOf(BondTypes.INT32));
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }
    }

    @Test
    public void testGenericMapContainer() {
        genericMapContainerTestHelper(BondTypes.STRING, "test");
        genericMapContainerTestHelper(BondTypes.BOOL, true);
        genericMapContainerTestHelper(BondTypes.INT8, (byte)1);
        genericMapContainerTestHelper(BondTypes.INT32, Integer.MIN_VALUE);
        genericMapContainerTestHelper(BondTypes.UINT64, Long.MAX_VALUE);
        genericMapContainerTestHelper(BondTypes.DOUBLE, 3.14);

        genericMapContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.August);
        genericMapContainerTestHelper(MonthOfYear.BOND_TYPE, MonthOfYear.get(100));
    }

    private static <T> void genericMapContainerTestHelper(BondType<T> elementBondType, T elementValue) {
        StructBondType<GenericMapContainer<T>> containerBondType =
                GenericMapContainer.BOND_TYPE.makeGenericType(elementBondType);
        GenericMapContainer<T> container = new GenericMapContainer<T>(containerBondType);
        Assert.assertEquals(0, container.mapField.size());
        container.mapField.put(elementValue, elementValue);
        Assert.assertEquals(1, container.mapField.size());
        Assert.assertTrue(container.mapField.containsKey(elementValue));
        Assert.assertTrue(container.mapField.containsValue(elementValue));
        marshalUnmarshalAndCompareUsingMultipleProtocols(container);
    }

    @Test
    public void testGenericMapContainerWithInvalidKeyType() {
        try {
            StructBondType<GenericMapContainer<Derived>> containerBondType =
                    GenericMapContainer.BOND_TYPE.makeGenericType(Derived.BOND_TYPE);
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }

        try {
            StructBondType<GenericMapContainer<List<Integer>>> containerBondType =
                    GenericMapContainer.BOND_TYPE.makeGenericType(BondType.listOf(BondTypes.INT32));
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }

        try {
            StructBondType<GenericMapContainer<Integer>> containerBondType =
                    GenericMapContainer.BOND_TYPE.makeGenericType(BondType.nullableOf(BondTypes.INT32));
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }
    }

    @Test
    public void testGenericBondedContainer() {
        Empty empty = new Empty();
        genericBondedContainerTestHelper(Empty.BOND_TYPE, empty);

        Derived derived = new Derived();
        derived.baseInt = 123;
        derived.derivedInt = 456;
        genericBondedContainerTestHelper(Derived.BOND_TYPE, derived);
    }

    private static <T extends BondSerializable> void genericBondedContainerTestHelper(
            StructBondType<T> elementBondType, T elementValue) {
        StructBondType<GenericBondedContainer<T>> containerBondType =
                GenericBondedContainer.BOND_TYPE.makeGenericType(elementBondType);
        GenericBondedContainer<T> container = new GenericBondedContainer<T>(containerBondType);
        try {
            Assert.assertEquals(elementBondType.newDefaultValue(), container.bondedField.deserialize());
            container.bondedField = Bonded.fromObject(elementValue, elementBondType);
            Assert.assertEquals(elementValue, container.bondedField.deserialize());
            marshalUnmarshalAndCompareUsingMultipleProtocols(container);
        } catch (IOException e) {
            Assert.fail(e.toString());
        }
    }

    @Test
    public void testGenericBondedContainerWithInvalidValueType() {
        try {
            StructBondType<GenericBondedContainer<Integer>> containerBondType =
                    GenericBondedContainer.BOND_TYPE.makeGenericType(BondTypes.INT32);
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }

        try {
            StructBondType<GenericBondedContainer<List<Integer>>> containerBondType =
                    GenericBondedContainer.BOND_TYPE.makeGenericType(BondType.listOf(BondTypes.INT32));
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }

        try {
            StructBondType<GenericMapContainer<Integer>> containerBondType =
                    GenericMapContainer.BOND_TYPE.makeGenericType(BondType.nullableOf(BondTypes.INT32));
            Assert.fail("Bond type must not be created");
        } catch (IllegalArgumentException e) {
            // success
        }
    }

    private static void marshalUnmarshalAndCompareUsingMultipleProtocols(BondSerializable obj) {
        try {
            TestHelper.marshalUnmarshalAndCompare(obj, ProtocolType.FAST_PROTOCOL, 1);
            TestHelper.marshalUnmarshalAndCompare(obj, ProtocolType.COMPACT_PROTOCOL, 1);
            TestHelper.marshalUnmarshalAndCompare(obj, ProtocolType.COMPACT_PROTOCOL, 2);
            TestHelper.marshalUnmarshalAndCompare(obj, ProtocolType.SIMPLE_PROTOCOL, 1);
            TestHelper.marshalUnmarshalAndCompare(obj, ProtocolType.SIMPLE_PROTOCOL, 2);
        } catch (IOException e) {
            // shouldn't happen
            Assert.fail(e.toString());
        }
    }
}
