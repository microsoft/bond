// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

public class SimpleBinaryV2ProtocolTest {

    private static final int PROTOCOL_VERSION = 2;

    private static final UntaggedBinaryTestHelper.ProtocolImplementations IMPLS =
        new UntaggedBinaryTestHelper.ProtocolImplementations(
            new UntaggedBinaryTestHelper.CreateWriter() {
                @Override
                public ProtocolWriter newWriter(ByteArrayOutputStream outputStream) {
                    return new SimpleBinaryWriter(outputStream, PROTOCOL_VERSION);
                }
            },
            new UntaggedBinaryTestHelper.CreateReader() {
                @Override
                public UntaggedProtocolReader newReader(ByteArrayInputStream inputStream) {
                    return new SimpleBinaryReader(inputStream, PROTOCOL_VERSION);
                }
            });

    @Test
    public void testEmptyStruct() {
        byte[] payload = new byte[]{
        };

        UntaggedBinaryTestHelper.testProtocol(
            payload, UntaggedBinaryTestHelper.TestCases.EmptyStruct, IMPLS);
    }

    @Test
    public void testEmptyStructWithEmptyBase() {
        byte[] payload = new byte[]{
        };

        UntaggedBinaryTestHelper.testProtocol(
            payload, UntaggedBinaryTestHelper.TestCases.EmptyStructWithEmptyBase, IMPLS);
    }

    @Test
    public void testSingleFieldStruct() {
        byte[] payload = new byte[]{
            // 1 : int16 = 0x1234
            (byte) 0x34, (byte) 0x12,
        };

        UntaggedBinaryTestHelper.testProtocol(
            payload, UntaggedBinaryTestHelper.TestCases.SingleFieldStruct, IMPLS);
    }

    @Test
    public void testEmptyStructWithSingleFieldBase() {
        byte[] payload = new byte[]{
            // 1 : int16 = 0x1234
            (byte) 0x34, (byte) 0x12,
        };

        UntaggedBinaryTestHelper.testProtocol(
            payload, UntaggedBinaryTestHelper.TestCases.EmptyStructWithSingleFieldBase, IMPLS);
    }

    @Test
    public void testSingleFieldStructWithSingleFieldBase() {
        byte[] payload = new byte[]{
            // 1 : int16 = 0x1234
            (byte) 0x34, (byte) 0x12,

            // 2 : uint32 = 0xABCDEF98
            (byte) 0x98, (byte) 0xEF, (byte) 0xCD, (byte) 0xAB,
        };

        UntaggedBinaryTestHelper.testProtocol(
            payload, UntaggedBinaryTestHelper.TestCases.SingleFieldStructWithSingleFieldBase, IMPLS);
    }

    @Test
    public void testMultiFieldStruct() {
        byte[] payload = new byte[]{
            // 20 : int8 = 0x45
            (byte) 0x45,

            // 21 : int8 = 0xCA
            (byte) 0xCA,

            // 22 : int16 = 0x57AD
            (byte) 0xAD, (byte) 0x57,

            // 23 : int16 = 0xB63A
            (byte) 0x3A, (byte) 0xB6,

            // 24 : int32 = 0x5172A3D4
            (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x51,

            // 25 : int32 = 0xBA6B3CAD
            (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xBA,

            // 26 : int64 = 0x59187726A534D342
            (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5,
            (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x59,

            // 27 : int64 = 0xB0A162B334C5A6D7
            (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34,
            (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xB0,

            // 30 : uint8 = 0x55
            (byte) 0x55,

            // 31 : uint8 = 0xDA
            (byte) 0xDA,

            // 32 : uint16 = 0x67AD
            (byte) 0xAD, (byte) 0x67,

            // 33 : uint16 = 0xC63A
            (byte) 0x3A, (byte) 0xC6,

            // 34 : uint32 = 0x6172A3D4
            (byte) 0xD4, (byte) 0xA3, (byte) 0x72, (byte) 0x61,

            // 35 : uint32 = 0xCA6B3CAD
            (byte) 0xAD, (byte) 0x3C, (byte) 0x6B, (byte) 0xCA,

            // 36 : uint64 = 0x69187726A534D342
            (byte) 0x42, (byte) 0xD3, (byte) 0x34, (byte) 0xA5,
            (byte) 0x26, (byte) 0x77, (byte) 0x18, (byte) 0x69,

            // 37 : uint64 = 0xC0A162B334C5A6D7
            (byte) 0xD7, (byte) 0xA6, (byte) 0xC5, (byte) 0x34,
            (byte) 0xB3, (byte) 0x62, (byte) 0xA1, (byte) 0xC0,

            // 40 : bool = false
            (byte) 0x00,

            // 41 : bool = true
            (byte) 0x01,

            // 50 : float = 5.12 (bits: 40A3D70A)
            (byte) 0x0A, (byte) 0xD7, (byte) 0xA3, (byte) 0x40,

            // 51 : float = -1001 (bits: C47A4000)
            (byte) 0x00, (byte) 0x40, (byte) 0x7A, (byte) 0xC4,

            // 60 : double = 5.12 (bits: 40147AE147AE147B)
            (byte) 0x7B, (byte) 0x14, (byte) 0xAE, (byte) 0x47, (byte) 0xE1, (byte) 0x7A, (byte) 0x14, (byte) 0x40,

            // 61 : double = -1001 (bits: C08F480000000000)
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x48, (byte) 0x8F, (byte) 0xC0,

            // 70 : string = ""
            (byte) 0x00,

            // 71 : string = "Привет World!"
            (byte) 0x13,
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
            (byte) 0x00,

            // 81 : wstring = "Привет World!"
            (byte) 0x0D,
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
        };

        UntaggedBinaryTestHelper.testProtocol(
            payload, UntaggedBinaryTestHelper.TestCases.MultiFieldStruct, IMPLS);
    }

    @Test
    public void testMultiListFieldStruct() {
        byte[] payload = new byte[]{
            // 10 : list<int8> = {}
            (byte) 0x00,

            // 11 : list<int8> = {0x01}
            (byte) 0x01,
            (byte) 0x01,

            // 12 : list<int8> = {0x02, 0x03}
            (byte) 0x02,
            (byte) 0x02,
            (byte) 0x03,

            // 20 : list<int16> = {}
            (byte) 0x00,

            // 21 : list<int16> = {0x0001}
            (byte) 0x01,
            (byte) 0x01, (byte) 0x00,

            // 22 : list<int16> = {0x0002, 0x0003}
            (byte) 0x02,
            (byte) 0x02, (byte) 0x00,
            (byte) 0x03, (byte) 0x00,

            // 30 : list<int32> = {}
            (byte) 0x00,

            // 31 : list<int32> = {0x00000001}
            (byte) 0x01,
            (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 32 : list<int32> = {0x00000002, 0x00000003}
            (byte) 0x02,
            (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00,
            (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 40 : list<int64> = {}
            (byte) 0x00,

            // 41 : list<int64> = {0x0000000000000001}
            (byte) 0x01,
            (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 42 : list<int64> = {0x0000000000000002, 0x0000000000000003}
            (byte) 0x02,
            (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
            (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 50 : list<uint8> = {}
            (byte) 0x00,

            // 51 : list<uint8> = {0x01}
            (byte) 0x01,
            (byte) 0x01,

            // 52 : list<uint8> = {0x02, 0x03}
            (byte) 0x02,
            (byte) 0x02,
            (byte) 0x03,

            // 60 : list<uint16> = {}
            (byte) 0x00,

            // 61 : list<uint16> = {0x0001}
            (byte) 0x01,
            (byte) 0x01, (byte) 0x00,

            // 62 : list<uint16> = {0x0002, 0x0003}
            (byte) 0x02,
            (byte) 0x02, (byte) 0x00,
            (byte) 0x03, (byte) 0x00,

            // 70 : list<uint32> = {}
            (byte) 0x00,

            // 71 : list<uint32> = {0x00000001}
            (byte) 0x01,
            (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 72 : list<uint32> = {0x00000002, 0x00000003}
            (byte) 0x02,
            (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00,
            (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 80 : list<uint64> = {}
            (byte) 0x00,

            // 81 : list<uint64> = {0x0000000000000001}
            (byte) 0x01,
            (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 82 : list<uint64> = {0x0000000000000002, 0x0000000000000003}
            (byte) 0x02,
            (byte) 0x02, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
            (byte) 0x03, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 90 : list<bool> = {}
            (byte) 0x00,

            // 91 : list<bool> = {false}
            (byte) 0x01,
            (byte) 0x00,

            // 92 : list<bool> = {false, true}
            (byte) 0x02,
            (byte) 0x00,
            (byte) 0x01,

            // 100 : list<float> = {}
            (byte) 0x00,

            // 101 : list<float> = {0}
            (byte) 0x01,
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 102 : list<float> = {0, -0}
            (byte) 0x02,
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x80,

            // 110 : list<double> = {}
            (byte) 0x00,

            // 111 : list<double> = {0}
            (byte) 0x01,
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

            // 112 : list<double> = {0, -0}
            (byte) 0x02,
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
            (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x80,

            // 120 : list<string> = {}
            (byte) 0x00,

            // 121 : list<string> = {""}
            (byte) 0x01,
            (byte) 0x00,

            // 122 : list<string> = {"", "test"}
            (byte) 0x02,
            (byte) 0x00,
            (byte) 0x04,
            (byte) 't',
            (byte) 'e',
            (byte) 's',
            (byte) 't',

            // 130 : list<wstring> = {}
            (byte) 0x00,

            // 131 : list<wstring> = {""}
            (byte) 0x01,
            (byte) 0x00,

            // 132 : list<wstring> = {"", "test"}
            (byte) 0x02,
            (byte) 0x00,
            (byte) 0x04,
            (byte) 't', (byte) 0,
            (byte) 'e', (byte) 0,
            (byte) 's', (byte) 0,
            (byte) 't', (byte) 0,

            // 1000 : list<struct> = {}
            (byte) 0x00,

            // 1001 : list<struct> = {S1}
            //        S1: 0 : int8 = 0xFE
            (byte) 0x01,
            (byte) 0xFE,

            // 1002 : list<struct> = {S1, S2, S3}
            //        S1: 0 : int8 = 0xEF
            //        S2: 1 : int8 = 0xA0
            //            2 : int8 = 0xA1
            //        S2: 3 : list<int8> = { 0xFA, 0xFB, 0xFC }
            (byte) 0x03,
            (byte) 0xEF,
            (byte) 0xA0,
            (byte) 0xA1,
            (byte) 0x03,
            (byte) 0xFA,
            (byte) 0xFB,
            (byte) 0xFC,

            // 2000 : list<list<?>> = {}
            (byte) 0x00,

            // 2001 : list<list<?>> = { L1 }
            //        L1: list<uint8> = { 0x0A, 0x0B, 0x0C }
            (byte) 0x01,
            (byte) 0x03,
            (byte) 0x0A,
            (byte) 0x0B,
            (byte) 0x0C,

            // 3000 : list<set<?>> = {}
            (byte) 0x00,

            // 3001 : list<set<?>> = { S1 }
            //        S1: set<uint8> = { 0x0A, 0x0B, 0x0C }
            (byte) 0x01,
            (byte) 0x03,
            (byte) 0x0A,
            (byte) 0x0B,
            (byte) 0x0C,

            // 4000 : list<map<?, ?>> = {}
            (byte) 0x00,

            // 4001 : list<map<?, ?>> = { M1 }
            //        M1: map<bool, uint8> = { false -> 0xF0, true -> 0xF1 }
            (byte) 0x01,
            (byte) 0x02,
            (byte) 0x00,
            (byte) 0xF0,
            (byte) 0x01,
            (byte) 0xF1,

            // 4002 : list<map<?, ?>> = { M1, M2 }
            //        M1: map<bool, string> = { true -> "true", false -> "false" }
            //        M2: map<int8, struct> = { 0x00 -> S1 }
            //        S1: 0 : int16 = 0xABBA
            (byte) 0x02,
            (byte) 0x02,
            (byte) 0x01,
            (byte) 0x04,
            (byte) 't',
            (byte) 'r',
            (byte) 'u',
            (byte) 'e',
            (byte) 0x00,
            (byte) 0x05,
            (byte) 'f',
            (byte) 'a',
            (byte) 'l',
            (byte) 's',
            (byte) 'e',
            (byte) 0x01,
            (byte) 0x00,
            (byte) 0xBA, (byte) 0xAB,

            // 10000 : blob (list<int8>) = {}
            (byte) 0x00,

            // 10001 : blob (list<int8>) = {0x01}
            (byte) 0x01,
            (byte) 0x01,

            // 10002 : blob (list<int8>) = {0x02, 0x03}
            (byte) 0x02,
            (byte) 0x02,
            (byte) 0x03,
        };

        UntaggedBinaryTestHelper.testProtocol(
            payload, UntaggedBinaryTestHelper.TestCases.MultiListFieldStruct, IMPLS);
    }

    @Test
    public void testLargeFieldStruct() throws IOException {
        // build payload
        ByteArrayOutputStream baos = new ByteArrayOutputStream();

        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeString1.length(), baos);
        baos.write(StringHelper.encodeString(UntaggedBinaryTestHelper.LargeSequences.LargeString1));
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeString2.length(), baos);
        baos.write(StringHelper.encodeString(UntaggedBinaryTestHelper.LargeSequences.LargeString2));
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeString3.length(), baos);
        baos.write(StringHelper.encodeString(UntaggedBinaryTestHelper.LargeSequences.LargeString3));
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeString4.length(), baos);
        baos.write(StringHelper.encodeString(UntaggedBinaryTestHelper.LargeSequences.LargeString4));
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeString5.length(), baos);
        baos.write(StringHelper.encodeString(UntaggedBinaryTestHelper.LargeSequences.LargeString5));

        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array1.length, baos);
        baos.write(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array1);
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array2.length, baos);
        baos.write(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array2);
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array3.length, baos);
        baos.write(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array3);
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array4.length, baos);
        baos.write(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array4);
        VarUIntHelper.encodeVarUInt32(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array5.length, baos);
        baos.write(UntaggedBinaryTestHelper.LargeSequences.LargeUInt8Array5);

        UntaggedBinaryTestHelper.testProtocol(
            baos.toByteArray(), UntaggedBinaryTestHelper.TestCases.LargeFieldStruct, IMPLS);
    }
}
