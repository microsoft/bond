// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.bondlib.test.*;
import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.EnumMap;

import static org.junit.Assert.*;

public class RandomRoundTripTest {

    private static final int testIterationCount = 3;

    @Test
    public void testSchemaDef() throws IOException {
        testRoundTrip(SchemaDef.BOND_TYPE, 1);
    }

    @Test
    public void testEmpty() throws IOException {
        testRoundTrip(Empty.BOND_TYPE, 1);
    }

    @Test
    public void testBase() throws IOException {
        testRoundTrip(Base.BOND_TYPE, 1);
    }

    @Test
    public void testDerived() throws IOException {
        testRoundTrip(Derived.BOND_TYPE, 1);
    }

    @Test
    public void testHasBondedField() throws IOException {
        testRoundTrip(HasBondedField.BOND_TYPE, 1, Derived.BOND_TYPE);
    }

    @Test
    public void testBondedCollection() throws IOException {
        testRoundTrip(BondedCollection.BOND_TYPE, 1, Derived.BOND_TYPE);
    }

    @Test
    public void testGenericContainerOfContainersForPrimitives() throws IOException {
        PrimitiveBondType[] primitiveBondTypes = new PrimitiveBondType[]{
                BondTypes.UINT8,
                BondTypes.UINT16,
                BondTypes.UINT32,
                BondTypes.UINT64,
                BondTypes.INT8,
                BondTypes.INT16,
                BondTypes.INT32,
                BondTypes.INT64,
                BondTypes.BOOL,
                BondTypes.FLOAT,
                BondTypes.DOUBLE,
                BondTypes.STRING,
                BondTypes.WSTRING,
                MonthOfYear.BOND_TYPE
        };
        for (PrimitiveBondType primitiveBondType : primitiveBondTypes) {
            StructBondType testedBondType =
                    GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(primitiveBondType);
            testRoundTrip(testedBondType, 1);
        }
    }

    @Test
    public void testGenericContainerOfContainersForStructs() throws IOException {
        StructBondType[] structBondTypes = new StructBondType[]{
                SchemaDef.BOND_TYPE,
                Empty.BOND_TYPE,
                Base.BOND_TYPE,
                Derived.BOND_TYPE,
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT8),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT16),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT32),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT64),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT8),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT16),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT32),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT64),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.BOOL),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.FLOAT),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.DOUBLE),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.STRING),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.WSTRING),
                GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(MonthOfYear.BOND_TYPE)
        };
        for (StructBondType structBondType : structBondTypes) {
            StructBondType testedBondType =
                    GenericContainerOfContainersForStructs.BOND_TYPE.makeGenericType(structBondType);
            testRoundTrip(testedBondType, 1, Derived.BOND_TYPE);
        }
    }

    @Test
    public void testGenericContainerOfContainersForCollections() throws IOException {
        BondType[] collectionBondTypes = new BondType[]{
                BondType.nullableOf(SchemaDef.BOND_TYPE),
                BondType.nullableOf(Empty.BOND_TYPE),
                BondType.nullableOf(Base.BOND_TYPE),
                BondType.nullableOf(Derived.BOND_TYPE),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT8)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT16)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT32)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT64)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT8)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT16)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT32)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT64)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.BOOL)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.FLOAT)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.DOUBLE)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.STRING)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.WSTRING)),
                BondType.nullableOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(MonthOfYear.BOND_TYPE)),
                BondType.nullableOf(BondTypes.UINT8),
                BondType.nullableOf(BondTypes.UINT16),
                BondType.nullableOf(BondTypes.UINT32),
                BondType.nullableOf(BondTypes.UINT64),
                BondType.nullableOf(BondTypes.INT8),
                BondType.nullableOf(BondTypes.INT16),
                BondType.nullableOf(BondTypes.INT32),
                BondType.nullableOf(BondTypes.INT64),
                BondType.nullableOf(BondTypes.BOOL),
                BondType.nullableOf(BondTypes.FLOAT),
                BondType.nullableOf(BondTypes.DOUBLE),
                BondType.nullableOf(BondTypes.STRING),
                BondType.nullableOf(BondTypes.WSTRING),
                BondType.nullableOf(MonthOfYear.BOND_TYPE),
                BondType.listOf(SchemaDef.BOND_TYPE),
                BondType.listOf(Empty.BOND_TYPE),
                BondType.listOf(Base.BOND_TYPE),
                BondType.listOf(Derived.BOND_TYPE),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT8)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT16)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT32)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT64)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT8)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT16)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT32)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT64)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.BOOL)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.FLOAT)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.DOUBLE)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.STRING)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.WSTRING)),
                BondType.listOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(MonthOfYear.BOND_TYPE)),
                BondType.listOf(BondTypes.UINT8),
                BondType.listOf(BondTypes.UINT16),
                BondType.listOf(BondTypes.UINT32),
                BondType.listOf(BondTypes.UINT64),
                BondType.listOf(BondTypes.INT8),
                BondType.listOf(BondTypes.INT16),
                BondType.listOf(BondTypes.INT32),
                BondType.listOf(BondTypes.INT64),
                BondType.listOf(BondTypes.BOOL),
                BondType.listOf(BondTypes.FLOAT),
                BondType.listOf(BondTypes.DOUBLE),
                BondType.listOf(BondTypes.STRING),
                BondType.listOf(BondTypes.WSTRING),
                BondType.listOf(MonthOfYear.BOND_TYPE),
                BondType.vectorOf(SchemaDef.BOND_TYPE),
                BondType.vectorOf(Empty.BOND_TYPE),
                BondType.vectorOf(Base.BOND_TYPE),
                BondType.vectorOf(Derived.BOND_TYPE),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT8)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT16)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT32)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.UINT64)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT8)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT16)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT32)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.INT64)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.BOOL)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.FLOAT)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.DOUBLE)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.STRING)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(BondTypes.WSTRING)),
                BondType.vectorOf(GenericContainerOfContainersForPrimitives.BOND_TYPE.makeGenericType(MonthOfYear.BOND_TYPE)),
                BondType.vectorOf(BondTypes.UINT8),
                BondType.vectorOf(BondTypes.UINT16),
                BondType.vectorOf(BondTypes.UINT32),
                BondType.vectorOf(BondTypes.UINT64),
                BondType.vectorOf(BondTypes.INT8),
                BondType.vectorOf(BondTypes.INT16),
                BondType.vectorOf(BondTypes.INT32),
                BondType.vectorOf(BondTypes.INT64),
                BondType.vectorOf(BondTypes.BOOL),
                BondType.vectorOf(BondTypes.FLOAT),
                BondType.vectorOf(BondTypes.DOUBLE),
                BondType.vectorOf(BondTypes.STRING),
                BondType.vectorOf(BondTypes.WSTRING),
                BondType.vectorOf(MonthOfYear.BOND_TYPE),
                BondType.setOf(BondTypes.UINT8),
                BondType.setOf(BondTypes.UINT16),
                BondType.setOf(BondTypes.UINT32),
                BondType.setOf(BondTypes.UINT64),
                BondType.setOf(BondTypes.INT8),
                BondType.setOf(BondTypes.INT16),
                BondType.setOf(BondTypes.INT32),
                BondType.setOf(BondTypes.INT64),
                BondType.setOf(BondTypes.BOOL),
                BondType.setOf(BondTypes.FLOAT),
                BondType.setOf(BondTypes.DOUBLE),
                BondType.setOf(BondTypes.STRING),
                BondType.setOf(BondTypes.WSTRING),
                BondType.setOf(MonthOfYear.BOND_TYPE),
                BondType.mapOf(BondTypes.UINT8, BondTypes.UINT8),
                BondType.mapOf(BondTypes.UINT16, BondTypes.UINT16),
                BondType.mapOf(BondTypes.UINT32, BondTypes.UINT32),
                BondType.mapOf(BondTypes.UINT64, BondTypes.UINT64),
                BondType.mapOf(BondTypes.INT8, BondTypes.INT8),
                BondType.mapOf(BondTypes.INT16, BondTypes.INT16),
                BondType.mapOf(BondTypes.INT32, BondTypes.INT32),
                BondType.mapOf(BondTypes.INT64, BondTypes.INT64),
                BondType.mapOf(BondTypes.BOOL, BondTypes.BOOL),
                BondType.mapOf(BondTypes.FLOAT, BondTypes.FLOAT),
                BondType.mapOf(BondTypes.DOUBLE, BondTypes.DOUBLE),
                BondType.mapOf(BondTypes.STRING, BondTypes.STRING),
                BondType.mapOf(BondTypes.WSTRING, BondTypes.WSTRING),
                BondType.mapOf(MonthOfYear.BOND_TYPE, MonthOfYear.BOND_TYPE)
        };
        for (BondType collectionBondType : collectionBondTypes) {
            StructBondType testedBondType =
                    GenericContainerOfContainersForCollections.BOND_TYPE.makeGenericType(collectionBondType);
            testRoundTrip(testedBondType, 1, Derived.BOND_TYPE);
        }
    }

    private static <TStruct extends BondSerializable> void testRoundTrip(
            StructBondType<TStruct> bondType, long randomSeed, StructBondType... polymorphicBondTypes)
            throws IOException {
        // set the polymorphic types and use default generator settings for the rest
        StructGeneratorSettings settings = new StructGeneratorSettings();
        settings.polymorphicBondTypes.addAll(Arrays.asList(polymorphicBondTypes));
        testRoundTrip(bondType, randomSeed, settings);
    }

    private static <TStruct extends BondSerializable> void testRoundTrip(
            StructBondType<TStruct> bondType, long randomSeed, StructGeneratorSettings settings)
            throws IOException {
        StructGenerator structGenerator = new StructGenerator(settings, randomSeed);

        // test using multiple iterations, to expand randomized coverage
        for (int i = 0; i < testIterationCount; ++i) {
            TStruct structValue = structGenerator.generateStruct(bondType);
            TestHelper.marshalUnmarshalAndCompare(structValue, ProtocolType.FAST_PROTOCOL, 1);
            TestHelper.marshalUnmarshalAndCompare(structValue, ProtocolType.COMPACT_PROTOCOL, 1);
            TestHelper.marshalUnmarshalAndCompare(structValue, ProtocolType.COMPACT_PROTOCOL, 2);
            TestHelper.marshalUnmarshalAndCompare(structValue, ProtocolType.SIMPLE_PROTOCOL, 1);
            TestHelper.marshalUnmarshalAndCompare(structValue, ProtocolType.SIMPLE_PROTOCOL, 2);
        }
    }
}
