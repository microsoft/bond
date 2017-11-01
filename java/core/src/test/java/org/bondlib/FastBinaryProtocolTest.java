// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class FastBinaryProtocolTest {

    private static final int PROTOCOL_VERSION = 1;

    private static final TaggedBinaryTestHelper.ProtocolImplementationFactory FACTORY =
            new TaggedBinaryTestHelper.ProtocolImplementationFactory(
                    new TaggedBinaryTestHelper.CreateWriter() {
                        @Override
                        public ProtocolWriter newWriter(ByteArrayOutputStream outputStream) {
                            return new FastBinaryWriter(outputStream, PROTOCOL_VERSION);
                        }
                    },
                    new TaggedBinaryTestHelper.CreateReader() {
                        @Override
                        public TaggedProtocolReader newReader(ByteArrayInputStream inputStream) {
                            return new FastBinaryReader(inputStream, PROTOCOL_VERSION);
                        }
                    });

    @Test
    public void testEmptyStruct() {
        byte[] payload = new byte[]{
                (byte) BondDataType.BT_STOP.value,
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.EmptyStruct, FACTORY);
    }

    @Test
    public void testEmptyStructWithEmptyBase() {
        byte[] payload = new byte[]{
                (byte) BondDataType.BT_STOP_BASE.value,

                (byte) BondDataType.BT_STOP.value,
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.EmptyStructWithEmptyBase, FACTORY);
    }

    @Test
    public void testSingleFieldStruct() {
        byte[] payload = new byte[]{
                // 1 : int16 = 0x1234
                (byte) BondDataType.BT_INT16.value,
                1, 0,
                (byte) 0x34, (byte) 0x12,

                (byte) BondDataType.BT_STOP.value,
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.SingleFieldStruct, FACTORY);
    }

    @Test
    public void testEmptyStructWithSingleFieldBase() {
        byte[] payload = new byte[]{
                // 1 : int16 = 0x1234
                (byte) BondDataType.BT_INT16.value,
                1, 0,
                (byte) 0x34, (byte) 0x12,

                (byte) BondDataType.BT_STOP_BASE.value,

                (byte) BondDataType.BT_STOP.value
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.EmptyStructWithSingleFieldBase, FACTORY);
    }

    @Test
    public void testSingleFieldStructWithSingleFieldBase() {
        byte[] payload = new byte[]{
                // 1 : int16 = 0x1234
                (byte) BondDataType.BT_INT16.value,
                1, 0,
                (byte) 0x34, (byte) 0x12,

                (byte) BondDataType.BT_STOP_BASE.value,

                // 2 : uint32 = 0xABCDEF98
                (byte) BondDataType.BT_UINT32.value,
                2, 0,
                (byte) 0x98, (byte) 0xEF, (byte) 0xCD, (byte) 0xAB,

                (byte) BondDataType.BT_STOP.value
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.SingleFieldStructWithSingleFieldBase, FACTORY);
    }

    @Test
    public void testMultiFieldStruct() {
        byte[] payload = new byte[]{
                // 20 : int8 = 0x45
                (byte) BondDataType.BT_INT8.value,
                20, 0,
                (byte) 0x45,

                // 21 : int8 = 0xCA
                (byte) BondDataType.BT_INT8.value,
                21, 0,
                (byte) 0xCA,

                // 22 : int16 = 0x57AD
                (byte) BondDataType.BT_INT16.value,
                22, 0,
                (byte) 0xAD, (byte) 0x57,

                // 23 : int16 = 0xB63A
                (byte) BondDataType.BT_INT16.value,
                23, 0,
                (byte) 0x3A, (byte) 0xB6,

                // 24 : int32 = 0x5172A3D4
                (byte) BondDataType.BT_INT32.value,
                24, 0,
                (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x51,

                // 25 : int32 = 0xBA6B3CAD
                (byte) BondDataType.BT_INT32.value,
                25, 0,
                (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xBA,

                // 26 : int64 = 0x59187726A534D342
                (byte) BondDataType.BT_INT64.value,
                26, 0,
                (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5, (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x59,

                // 27 : int64 = 0xB0A162B334C5A6D7
                (byte) BondDataType.BT_INT64.value,
                27, 0,
                (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34, (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xB0,

                // 30 : uint8 = 0x55
                (byte) BondDataType.BT_UINT8.value,
                30, 0,
                (byte) 0x55,

                // 31 : uint8 = 0xDA
                (byte) BondDataType.BT_UINT8.value,
                31, 0,
                (byte) 0xDA,

                // 32 : uint16 = 0x67AD
                (byte) BondDataType.BT_UINT16.value,
                32, 0,
                (byte) 0xAD, (byte) 0x67,

                // 33 : uint16 = 0xC63A
                (byte) BondDataType.BT_UINT16.value,
                33, 0,
                (byte) 0x3A, (byte) 0xC6,

                // 34 : uint32 = 0x6172A3D4
                (byte) BondDataType.BT_UINT32.value,
                34, 0,
                (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x61,

                // 35 : uint32 = 0xCA6B3CAD
                (byte) BondDataType.BT_UINT32.value,
                35, 0,
                (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xCA,

                // 36 : uint64 = 0x69187726A534D342
                (byte) BondDataType.BT_UINT64.value,
                36, 0,
                (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5, (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x69,

                // 37 : uint64 = 0xC0A162B334C5A6D7
                (byte) BondDataType.BT_UINT64.value,
                37, 0,
                (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34, (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xC0,

                // 40 : bool = false
                (byte) BondDataType.BT_BOOL.value,
                40, 0,
                (byte) 0x00,

                // 41 : bool = true
                (byte) BondDataType.BT_BOOL.value,
                41, 0,
                (byte) 0x01,

                // 50 : float = 5.12 (bits: 40A3D70A)
                (byte) BondDataType.BT_FLOAT.value,
                50, 0,
                (byte) 0x0A, (byte) 0xD7, (byte) 0xA3, (byte) 0x40,

                // 51 : float = -1001 (bits: C47A4000)
                (byte) BondDataType.BT_FLOAT.value,
                51, 0,
                (byte) 0x00, (byte) 0x40, (byte) 0x7A, (byte) 0xC4,

                // 60 : double = 5.12 (bits: 40147AE147AE147B)
                (byte) BondDataType.BT_DOUBLE.value,
                60, 0,
                (byte) 0x7B, (byte) 0x14, (byte) 0xAE, (byte) 0x47, (byte) 0xE1, (byte) 0x7A, (byte) 0x14, (byte) 0x40,

                // 61 : double = -1001 (bits: C08F480000000000)
                (byte) BondDataType.BT_DOUBLE.value,
                61, 0,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x48, (byte) 0x8F, (byte) 0xC0,

                // 70 : string = ""
                (byte) BondDataType.BT_STRING.value,
                70, 0,
                (byte) 0,

                // 71 : string = "Привет World!"
                (byte) BondDataType.BT_STRING.value,
                71, 0,
                (byte) 19,
                (byte) 0xD0, (byte) 0x9F,
                (byte) 0xD1, (byte) 0x80,
                (byte) 0xD0, (byte) 0xB8,
                (byte) 0xD0, (byte) 0xB2,
                (byte) 0xD0, (byte) 0xB5,
                (byte) 0xD1, (byte) 0x82,
                (byte) 0x20,
                (byte) 0x57,
                (byte) 0x6F,
                (byte) 0x72,
                (byte) 0x6C,
                (byte) 0x64,
                (byte) 0x21,

                // 80 : wstring = ""
                (byte) BondDataType.BT_WSTRING.value,
                80, 0,
                (byte) 0,

                // 81 : wstring = "Привет World!"
                (byte) BondDataType.BT_WSTRING.value,
                81, 0,
                (byte) 13,
                (byte) 0x1F, (byte) 0x04,
                (byte) 0x40, (byte) 0x04,
                (byte) 0x38, (byte) 0x04,
                (byte) 0x32, (byte) 0x04,
                (byte) 0x35, (byte) 0x04,
                (byte) 0x42, (byte) 0x04,
                (byte) 0x20, (byte) 0x00,
                (byte) 0x57, (byte) 0x00,
                (byte) 0x6F, (byte) 0x00,
                (byte) 0x72, (byte) 0x00,
                (byte) 0x6C, (byte) 0x00,
                (byte) 0x64, (byte) 0x00,
                (byte) 0x21, (byte) 0x00,

                (byte) BondDataType.BT_STOP.value
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.MultiFieldStruct, FACTORY);
    }

    @Test
    public void testMultiFieldStructWithSmallIds() {
        byte[] payload = new byte[]{
                // 0 : int8 = 0x45
                (byte) BondDataType.BT_INT8.value,
                0, 0,
                (byte) 0x45,

                // 1 : int8 = 0xCA
                (byte) BondDataType.BT_INT8.value,
                1, 0,
                (byte) 0xCA,

                // 2 : int16 = 0x57AD
                (byte) BondDataType.BT_INT16.value,
                2, 0,
                (byte) 0xAD, (byte) 0x57,

                // 3 : int16 = 0xB63A
                (byte) BondDataType.BT_INT16.value,
                3, 0,
                (byte) 0x3A, (byte) 0xB6,

                // 4 : int32 = 0x5172A3D4
                (byte) BondDataType.BT_INT32.value,
                4, 0,
                (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x51,

                // 5 : int32 = 0xBA6B3CAD
                (byte) BondDataType.BT_INT32.value,
                5, 0,
                (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xBA,

                // 6 : int64 = 0x59187726A534D342
                (byte) BondDataType.BT_INT64.value,
                6, 0,
                (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5, (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x59,

                // 7 : int64 = 0xB0A162B334C5A6D7
                (byte) BondDataType.BT_INT64.value,
                7, 0,
                (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34, (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xB0,

                // 8 : uint8 = 0x55
                (byte) BondDataType.BT_UINT8.value,
                8, 0,
                (byte) 0x55,

                // 9 : uint8 = 0xDA
                (byte) BondDataType.BT_UINT8.value,
                9, 0,
                (byte) 0xDA,

                // 10 : uint16 = 0x67AD
                (byte) BondDataType.BT_UINT16.value,
                10, 0,
                (byte) 0xAD, (byte) 0x67,

                // 11 : uint16 = 0xC63A
                (byte) BondDataType.BT_UINT16.value,
                11, 0,
                (byte) 0x3A, (byte) 0xC6,

                // 12 : uint32 = 0x6172A3D4
                (byte) BondDataType.BT_UINT32.value,
                12, 0,
                (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x61,

                // 13 : uint32 = 0xCA6B3CAD
                (byte) BondDataType.BT_UINT32.value,
                13, 0,
                (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xCA,

                // 14 : uint64 = 0x69187726A534D342
                (byte) BondDataType.BT_UINT64.value,
                14, 0,
                (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5, (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x69,

                // 15 : uint64 = 0xC0A162B334C5A6D7
                (byte) BondDataType.BT_UINT64.value,
                15, 0,
                (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34, (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xC0,

                // 16 : bool = false
                (byte) BondDataType.BT_BOOL.value,
                16, 0,
                (byte) 0x00,

                // 17 : bool = true
                (byte) BondDataType.BT_BOOL.value,
                17, 0,
                (byte) 0x01,

                (byte) BondDataType.BT_STOP.value
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.MultiFieldStructWithSmallIds, FACTORY);
    }

    @Test
    public void testMultiFieldStructWithLargeIds() {
        byte[] payload = new byte[]{
                // 200 : int8 = 0x45
                (byte) BondDataType.BT_INT8.value,
                (byte) 200, (byte) (200 >>> 8),
                (byte) 0x45,

                // 210 : int8 = 0xCA
                (byte) BondDataType.BT_INT8.value,
                (byte) 210, (byte) (210 >>> 8),
                (byte) 0xCA,

                // 220 : int16 = 0x57AD
                (byte) BondDataType.BT_INT16.value,
                (byte) 220, (byte) (220 >>> 8),
                (byte) 0xAD, (byte) 0x57,

                // 230 : int16 = 0xB63A
                (byte) BondDataType.BT_INT16.value,
                (byte) 230, (byte) (230 >>> 8),
                (byte) 0x3A, (byte) 0xB6,

                // 240 : int32 = 0x5172A3D4
                (byte) BondDataType.BT_INT32.value,
                (byte) 240, (byte) (240 >>> 8),
                (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x51,

                // 250 : int32 = 0xBA6B3CAD
                (byte) BondDataType.BT_INT32.value,
                (byte) 250, (byte) (250 >>> 8),
                (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xBA,

                // 260 : int64 = 0x59187726A534D342
                (byte) BondDataType.BT_INT64.value,
                (byte) 260, (byte) (260 >>> 8),
                (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5, (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x59,

                // 270 : int64 = 0xB0A162B334C5A6D7
                (byte) BondDataType.BT_INT64.value,
                (byte) 270, (byte) (270 >>> 8),
                (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34, (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xB0,

                // 3000 : uint8 = 0x55
                (byte) BondDataType.BT_UINT8.value,
                (byte) 3000, (byte) (3000 >>> 8),
                (byte) 0x55,

                // 3100 : uint8 = 0xDA
                (byte) BondDataType.BT_UINT8.value,
                (byte) 3100, (byte) (3100 >>> 8),
                (byte) 0xDA,

                // 3200 : uint16 = 0x67AD
                (byte) BondDataType.BT_UINT16.value,
                (byte) 3200, (byte) (3200 >>> 8),
                (byte) 0xAD, (byte) 0x67,

                // 3300 : uint16 = 0xC63A
                (byte) BondDataType.BT_UINT16.value,
                (byte) 3300, (byte) (3300 >>> 8),
                (byte) 0x3A, (byte) 0xC6,

                // 3400 : uint32 = 0x6172A3D4
                (byte) BondDataType.BT_UINT32.value,
                (byte) 3400, (byte) (3400 >>> 8),
                (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x61,

                // 3500 : uint32 = 0xCA6B3CAD
                (byte) BondDataType.BT_UINT32.value,
                (byte) 3500, (byte) (3500 >>> 8),
                (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xCA,

                // 3600 : uint64 = 0x69187726A534D342
                (byte) BondDataType.BT_UINT64.value,
                (byte) 3600, (byte) (3600 >>> 8),
                (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5, (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x69,

                // 3700 : uint64 = 0xC0A162B334C5A6D7
                (byte) BondDataType.BT_UINT64.value,
                (byte) 3700, (byte) (3700 >>> 8),
                (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34, (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xC0,

                // 40000 : bool = false
                (byte) BondDataType.BT_BOOL.value,
                (byte) 40000, (byte) (40000 >>> 8),
                (byte) 0x00,

                // 41000 : bool = true
                (byte) BondDataType.BT_BOOL.value,
                (byte) 41000, (byte) (41000 >>> 8),
                (byte) 0x01,

                (byte) BondDataType.BT_STOP.value
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.MultiFieldStructWithLargeIds, FACTORY);
    }

    @Test
    public void testMultiListFieldStruct() {
        byte[] payload = new byte[]{
                // 10 : list<int8> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 10, (byte) (10 >>> 8),
                (byte) BondDataType.BT_INT8.value,
                (byte) 0,

                // 11 : list<int8> = {0x01}
                (byte) BondDataType.BT_LIST.value,
                (byte) 11, (byte) (11 >>> 8),
                (byte) BondDataType.BT_INT8.value,
                (byte) 1,
                (byte) 0x01,

                // 12 : list<int8> = {0x02, 0x03}
                (byte) BondDataType.BT_LIST.value,
                (byte) 12, (byte) (12 >>> 8),
                (byte) BondDataType.BT_INT8.value,
                (byte) 2,
                (byte) 0x02,
                (byte) 0x03,

                // 20 : list<int16> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 20, (byte) (20 >>> 8),
                (byte) BondDataType.BT_INT16.value,
                (byte) 0,

                // 21 : list<int16> = {0x0001}
                (byte) BondDataType.BT_LIST.value,
                (byte) 21, (byte) (21 >>> 8),
                (byte) BondDataType.BT_INT16.value,
                (byte) 1,
                (byte) 0x01, (byte) 0x00,

                // 22 : list<int16> = {0x0002, 0x0003}
                (byte) BondDataType.BT_LIST.value,
                (byte) 22, (byte) (22 >>> 8),
                (byte) BondDataType.BT_INT16.value,
                (byte) 2,
                (byte) 0x02, (byte) 0x00,
                (byte) 0x03, (byte) 0x00,

                // 30 : list<int32> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 30, (byte) (30 >>> 8),
                (byte) BondDataType.BT_INT32.value,
                (byte) 0,

                // 31 : list<int32> = {0x00000001}
                (byte) BondDataType.BT_LIST.value,
                (byte) 31, (byte) (31 >>> 8),
                (byte) BondDataType.BT_INT32.value,
                (byte) 1,
                (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 32 : list<int32> = {0x00000002, 0x00000003}
                (byte) BondDataType.BT_LIST.value,
                (byte) 32, (byte) (32 >>> 8),
                (byte) BondDataType.BT_INT32.value,
                (byte) 2,
                (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 40 : list<int64> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 40, (byte) (40 >>> 8),
                (byte) BondDataType.BT_INT64.value,
                (byte) 0,

                // 41 : list<int64> = {0x0000000000000001}
                (byte) BondDataType.BT_LIST.value,
                (byte) 41, (byte) (41 >>> 8),
                (byte) BondDataType.BT_INT64.value,
                (byte) 1,
                (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 42 : list<int64> = {0x0000000000000002, 0x0000000000000003}
                (byte) BondDataType.BT_LIST.value,
                (byte) 42, (byte) (42 >>> 8),
                (byte) BondDataType.BT_INT64.value,
                (byte) 2,
                (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 50 : list<uint8> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 50, (byte) (50 >>> 8),
                (byte) BondDataType.BT_UINT8.value,
                (byte) 0,

                // 51 : list<uint8> = {0x01}
                (byte) BondDataType.BT_LIST.value,
                (byte) 51, (byte) (51 >>> 8),
                (byte) BondDataType.BT_UINT8.value,
                (byte) 1,
                (byte) 0x01,

                // 52 : list<uint8> = {0x02, 0x03}
                (byte) BondDataType.BT_LIST.value,
                (byte) 52, (byte) (52 >>> 8),
                (byte) BondDataType.BT_UINT8.value,
                (byte) 2,
                (byte) 0x02,
                (byte) 0x03,

                // 60 : list<uint16> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 60, (byte) (60 >>> 8),
                (byte) BondDataType.BT_UINT16.value,
                (byte) 0,

                // 61 : list<uint16> = {0x0001}
                (byte) BondDataType.BT_LIST.value,
                (byte) 61, (byte) (61 >>> 8),
                (byte) BondDataType.BT_UINT16.value,
                (byte) 1,
                (byte) 0x01, (byte) 0x00,

                // 62 : list<uint16> = {0x0002, 0x0003}
                (byte) BondDataType.BT_LIST.value,
                (byte) 62, (byte) (62 >>> 8),
                (byte) BondDataType.BT_UINT16.value,
                (byte) 2,
                (byte) 0x02, (byte) 0x00,
                (byte) 0x03, (byte) 0x00,

                // 70 : list<uint32> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 70, (byte) (70 >>> 8),
                (byte) BondDataType.BT_UINT32.value,
                (byte) 0,

                // 71 : list<uint32> = {0x00000001}
                (byte) BondDataType.BT_LIST.value,
                (byte) 71, (byte) (71 >>> 8),
                (byte) BondDataType.BT_UINT32.value,
                (byte) 1,
                (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 72 : list<uint32> = {0x00000002, 0x00000003}
                (byte) BondDataType.BT_LIST.value,
                (byte) 72, (byte) (72 >>> 8),
                (byte) BondDataType.BT_UINT32.value,
                (byte) 2,
                (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 80 : list<uint64> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 80, (byte) (80 >>> 8),
                (byte) BondDataType.BT_UINT64.value,
                (byte) 0,

                // 81 : list<uint64> = {0x0000000000000001}
                (byte) BondDataType.BT_LIST.value,
                (byte) 81, (byte) (81 >>> 8),
                (byte) BondDataType.BT_UINT64.value,
                (byte) 1,
                (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 82 : list<uint64> = {0x0000000000000002, 0x0000000000000003}
                (byte) BondDataType.BT_LIST.value,
                (byte) 82, (byte) (82 >>> 8),
                (byte) BondDataType.BT_UINT64.value,
                (byte) 2,
                (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 90 : list<bool> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 90, (byte) (90 >>> 8),
                (byte) BondDataType.BT_BOOL.value,
                (byte) 0,

                // 91 : list<bool> = {false}
                (byte) BondDataType.BT_LIST.value,
                (byte) 91, (byte) (91 >>> 8),
                (byte) BondDataType.BT_BOOL.value,
                (byte) 1,
                (byte) 0x00,

                // 92 : list<bool> = {false, true}
                (byte) BondDataType.BT_LIST.value,
                (byte) 92, (byte) (92 >>> 8),
                (byte) BondDataType.BT_BOOL.value,
                (byte) 2,
                (byte) 0x00,
                (byte) 0x01,

                // 100 : list<float> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 100, (byte) (100 >>> 8),
                (byte) BondDataType.BT_FLOAT.value,
                (byte) 0,

                // 101 : list<float> = {0}
                (byte) BondDataType.BT_LIST.value,
                (byte) 101, (byte) (101 >>> 8),
                (byte) BondDataType.BT_FLOAT.value,
                (byte) 1,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 102 : list<float> = {0, -0}
                (byte) BondDataType.BT_LIST.value,
                (byte) 102, (byte) (102 >>> 8),
                (byte) BondDataType.BT_FLOAT.value,
                (byte) 2,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x80,

                // 110 : list<double> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 110, (byte) (110 >>> 8),
                (byte) BondDataType.BT_DOUBLE.value,
                (byte) 0,

                // 111 : list<double> = {0}
                (byte) BondDataType.BT_LIST.value,
                (byte) 111, (byte) (111 >>> 8),
                (byte) BondDataType.BT_DOUBLE.value,
                (byte) 1,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // 112 : list<double> = {0, -0}
                (byte) BondDataType.BT_LIST.value,
                (byte) 112, (byte) (112 >>> 8),
                (byte) BondDataType.BT_DOUBLE.value,
                (byte) 2,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x80,

                // 120 : list<string> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 120, (byte) (120 >>> 8),
                (byte) BondDataType.BT_STRING.value,
                (byte) 0,

                // 121 : list<string> = {""}
                (byte) BondDataType.BT_LIST.value,
                (byte) 121, (byte) (121 >>> 8),
                (byte) BondDataType.BT_STRING.value,
                (byte) 1,
                (byte) 0,

                // 122 : list<string> = {"", "test"}
                (byte) BondDataType.BT_LIST.value,
                (byte) 122, (byte) (122 >>> 8),
                (byte) BondDataType.BT_STRING.value,
                (byte) 2,
                (byte) 0,
                (byte) 4,
                (byte) 't',
                (byte) 'e',
                (byte) 's',
                (byte) 't',

                // 130 : list<wstring> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 130, (byte) (130 >>> 8),
                (byte) BondDataType.BT_WSTRING.value,
                (byte) 0,

                // 131 : list<wstring> = {""}
                (byte) BondDataType.BT_LIST.value,
                (byte) 131, (byte) (131 >>> 8),
                (byte) BondDataType.BT_WSTRING.value,
                (byte) 1,
                (byte) 0,

                // 132 : list<wstring> = {"", "test"}
                (byte) BondDataType.BT_LIST.value,
                (byte) 132, (byte) (132 >>> 8),
                (byte) BondDataType.BT_WSTRING.value,
                (byte) 2,
                (byte) 0,
                (byte) 4,
                (byte) 't', (byte) 0,
                (byte) 'e', (byte) 0,
                (byte) 's', (byte) 0,
                (byte) 't', (byte) 0,

                // 1000 : list<struct> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 1000, (byte) (1000 >>> 8),
                (byte) BondDataType.BT_STRUCT.value,
                (byte) 0,

                // 1001 : list<struct> = {S1}
                //        S1: 0 : int8 = 0xFE
                (byte) BondDataType.BT_LIST.value,
                (byte) 1001, (byte) (1001 >>> 8),
                (byte) BondDataType.BT_STRUCT.value,
                (byte) 1,
                (byte) BondDataType.BT_INT8.value,
                (byte) 0, (byte) 0,
                (byte) 0xFE,
                (byte) BondDataType.BT_STOP.value,

                // 1002 : list<struct> = {S1, S2, S3}
                //        S1: 0 : int8 = 0xEF
                //        S2: 1 : int8 = 0xA0
                //            2 : int8 = 0xA1
                //        S2: 3 : list<int8> = { 0xFA, 0xFB, 0xFC }
                (byte) BondDataType.BT_LIST.value,
                (byte) 1002, (byte) (1002 >>> 8),
                (byte) BondDataType.BT_STRUCT.value,
                (byte) 3,
                (byte) BondDataType.BT_INT8.value,
                (byte) 0, (byte) 0,
                (byte) 0xEF,
                (byte) BondDataType.BT_STOP.value,
                (byte) BondDataType.BT_INT8.value,
                (byte) 1, (byte) 0,
                (byte) 0xA0,
                (byte) BondDataType.BT_INT8.value,
                (byte) 2, (byte) 0,
                (byte) 0xA1,
                (byte) BondDataType.BT_STOP.value,
                (byte) BondDataType.BT_LIST.value,
                (byte) 3, (byte) 0,
                (byte) BondDataType.BT_INT8.value,
                (byte) 3,
                (byte) 0xFA,
                (byte) 0xFB,
                (byte) 0xFC,
                (byte) BondDataType.BT_STOP.value,

                // 2000 : list<list<?>> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 2000, (byte) (2000 >>> 8),
                (byte) BondDataType.BT_LIST.value,
                (byte) 0,

                // 2001 : list<list<?>> = { L1 }
                //        L1: list<uint8> = { 0x0A, 0x0B, 0x0C }
                (byte) BondDataType.BT_LIST.value,
                (byte) 2001, (byte) (2001 >>> 8),
                (byte) BondDataType.BT_LIST.value,
                (byte) 1,
                (byte) BondDataType.BT_UINT8.value,
                (byte) 3,
                (byte) 0x0A,
                (byte) 0x0B,
                (byte) 0x0C,

                // 3000 : list<set<?>> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 3000, (byte) (3000 >>> 8),
                (byte) BondDataType.BT_SET.value,
                (byte) 0,

                // 3001 : list<set<?>> = { S1 }
                //        S1: set<uint8> = { 0x0A, 0x0B, 0x0C }
                (byte) BondDataType.BT_LIST.value,
                (byte) 3001, (byte) (3001 >>> 8),
                (byte) BondDataType.BT_SET.value,
                (byte) 1,
                (byte) BondDataType.BT_UINT8.value,
                (byte) 3,
                (byte) 0x0A,
                (byte) 0x0B,
                (byte) 0x0C,

                // 4000 : list<map<?, ?>> = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 4000, (byte) (4000 >>> 8),
                (byte) BondDataType.BT_MAP.value,
                (byte) 0,

                // 4001 : list<map<?, ?>> = { M1 }
                //        M1: map<bool, uint8> = { false -> 0xF0, true -> 0xF1 }
                (byte) BondDataType.BT_LIST.value,
                (byte) 4001, (byte) (4001 >>> 8),
                (byte) BondDataType.BT_MAP.value,
                (byte) 1,
                (byte) BondDataType.BT_BOOL.value,
                (byte) BondDataType.BT_UINT8.value,
                (byte) 2,
                (byte) 0x00,
                (byte) 0xF0,
                (byte) 0x01,
                (byte) 0xF1,

                // 4002 : list<map<?, ?>> = { M1, M2 }
                //        M1: map<bool, string> = { true -> "true", false -> "false" }
                //        M2: map<int8, struct> = { 0x00 -> S1 }
                //        S1: 0 : int16 = 0xABBA
                (byte) BondDataType.BT_LIST.value,
                (byte) 4002, (byte) (4002 >>> 8),
                (byte) BondDataType.BT_MAP.value,
                (byte) 2,
                (byte) BondDataType.BT_BOOL.value,
                (byte) BondDataType.BT_STRING.value,
                (byte) 2,
                (byte) 0x01,
                (byte) 4,
                (byte) 't',
                (byte) 'r',
                (byte) 'u',
                (byte) 'e',
                (byte) 0x00,
                (byte) 5,
                (byte) 'f',
                (byte) 'a',
                (byte) 'l',
                (byte) 's',
                (byte) 'e',
                (byte) BondDataType.BT_INT8.value,
                (byte) BondDataType.BT_STRUCT.value,
                (byte) 1,
                (byte) 0x00,
                (byte) BondDataType.BT_INT16.value,
                (byte) 0, (byte) 0,
                (byte) 0xBA, (byte) 0xAB,
                (byte) BondDataType.BT_STOP.value,

                // 10000 : blob (list<int8>) = {}
                (byte) BondDataType.BT_LIST.value,
                (byte) 10000, (byte) (10000 >>> 8),
                (byte) BondDataType.BT_UINT8.value,
                (byte) 0,

                // 10001 : blob (list<int8>) = {0x01}
                (byte) BondDataType.BT_LIST.value,
                (byte) 10001, (byte) (10001 >>> 8),
                (byte) BondDataType.BT_UINT8.value,
                (byte) 1,
                (byte) 0x01,

                // 10002 : blob (list<int8>) = {0x02, 0x03}
                (byte) BondDataType.BT_LIST.value,
                (byte) 10002, (byte) (10002 >>> 8),
                (byte) BondDataType.BT_UINT8.value,
                (byte) 2,
                (byte) 0x02,
                (byte) 0x03,

                (byte) BondDataType.BT_STOP.value
        };

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.MultiListFieldStruct, FACTORY);
    }

    @Test
    public void testLargeFieldStruct() throws IOException {
        // build payload
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        baos.write(new byte[]{(byte) BondDataType.BT_STRING.value, 1, 0});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeString1.length(), baos);
        baos.write(StringHelper.encodeString(TaggedBinaryTestHelper.LargeSequences.LargeString1));
        baos.write(new byte[]{(byte) BondDataType.BT_STRING.value, 2, 0});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeString2.length(), baos);
        baos.write(StringHelper.encodeString(TaggedBinaryTestHelper.LargeSequences.LargeString2));
        baos.write(new byte[]{(byte) BondDataType.BT_STRING.value, 3, 0});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeString3.length(), baos);
        baos.write(StringHelper.encodeString(TaggedBinaryTestHelper.LargeSequences.LargeString3));
        baos.write(new byte[]{(byte) BondDataType.BT_STRING.value, 4, 0});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeString4.length(), baos);
        baos.write(StringHelper.encodeString(TaggedBinaryTestHelper.LargeSequences.LargeString4));
        baos.write(new byte[]{(byte) BondDataType.BT_STRING.value, 5, 0});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeString5.length(), baos);
        baos.write(StringHelper.encodeString(TaggedBinaryTestHelper.LargeSequences.LargeString5));

        baos.write(new byte[]{(byte) BondDataType.BT_SET.value, 101, 0, (byte) BondDataType.BT_UINT8.value});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array1.length, baos);
        baos.write(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array1);
        baos.write(new byte[]{(byte) BondDataType.BT_SET.value, 102, 0, (byte) BondDataType.BT_UINT8.value});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array2.length, baos);
        baos.write(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array2);
        baos.write(new byte[]{(byte) BondDataType.BT_SET.value, 103, 0, (byte) BondDataType.BT_UINT8.value});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array3.length, baos);
        baos.write(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array3);
        baos.write(new byte[]{(byte) BondDataType.BT_SET.value, 104, 0, (byte) BondDataType.BT_UINT8.value});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array4.length, baos);
        baos.write(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array4);
        baos.write(new byte[]{(byte) BondDataType.BT_SET.value, 105, 0, (byte) BondDataType.BT_UINT8.value});
        VarUIntHelper.encodeVarUInt32(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array5.length, baos);
        baos.write(TaggedBinaryTestHelper.LargeSequences.LargeUInt8Array5);

        baos.write((byte) BondDataType.BT_STOP.value);

        byte[] payload = baos.toByteArray();

        TaggedBinaryTestHelper.testProtocol(
                payload, TaggedBinaryTestHelper.TestCases.LargeFieldStruct, FACTORY);
    }
}
