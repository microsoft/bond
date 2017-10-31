// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Assert;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.Assert.*;

/**
 * Contains helpers for implementing unit tests for untagged binary protocol readers and writers.
 */
final class UntaggedBinaryTestHelper {
    /**
     * A delegate responsible for invoking user code for writing/serialization.
     */
    static abstract class InvokeWriter {
        /**
         * Serializes data.
         *
         * @param w the writer
         * @throws IOException
         */
        abstract void write(ProtocolWriter w) throws IOException;
    }

    /**
     * A delegate responsible for invoking user code for reading/deserialization.
     */
    static abstract class InvokeReader {
        /**
         * Deserializes data and verifies results as it reads.
         *
         * @param r the reader
         * @throws IOException
         */
        abstract void readAndVerify(UntaggedProtocolReader r) throws IOException;
    }

    /**
     * A delegate responsible for creating writers.
     */
    static abstract class CreateWriter {
        /**
         * Creates a new writer.
         *
         * @param outputStream the stream to write to
         * @return writer
         */
        abstract ProtocolWriter newWriter(ByteArrayOutputStream outputStream);
    }

    /**
     * A delegate responsible for creating readers.
     */
    static abstract class CreateReader {
        /**
         * Creates a new reader.
         *
         * @param inputStream the stream to read from
         * @return reader
         */
        abstract UntaggedProtocolReader newReader(ByteArrayInputStream inputStream);
    }

    /**
     * A tuple of {@see InvokeWriter} and {@see InvokeReader} that form a particular test case for binary protocols.
     * A specific protocol needs to provide the payload data and {@see ProtocolImplementations} factories,
     * and then call the method {@see testProtocol}.
     */
    static final class ProtocolTestCase {
        final InvokeWriter invokeWriterDelegate;
        final InvokeReader invokeReaderDelegate;

        ProtocolTestCase(InvokeWriter invokeWriterDelegate, InvokeReader invokeReaderDelegate) {
            this.invokeWriterDelegate = invokeWriterDelegate;
            this.invokeReaderDelegate = invokeReaderDelegate;
        }
    }

    /**
     * A tuple of {@see CreateWriter} and {@see CreateReader} that provide implementation of a protocol.
     */
    static final class ProtocolImplementations {
        final CreateWriter createWriterDelegate;
        final CreateReader createReaderDelegate;

        ProtocolImplementations(CreateWriter createWriterDelegate, CreateReader createReaderDelegate) {
            this.createWriterDelegate = createWriterDelegate;
            this.createReaderDelegate = createReaderDelegate;
        }
    }

    /**
     * Verifies a pair of a binary reader and a binary writer to ensure that they produce correct results.
     *
     * @param bytes                 the expected bytes, used as input to the reader and to verify the writer
     * @param testCase              defines the test case
     * @param implementationFactory defines the protocol implementation
     */
    static void testProtocol(
        byte[] bytes,
        ProtocolTestCase testCase,
        ProtocolImplementations implementationFactory) {

        // create writer and invoke first pass if needed
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ProtocolWriter writer = implementationFactory.createWriterDelegate.newWriter(baos);
        if (writer instanceof TwoPassProtocolWriter) {
            ProtocolWriter firstPassWriter = ((TwoPassProtocolWriter) writer).getFirstPassWriter();
            if (firstPassWriter != null) {
                try {
                    testCase.invokeWriterDelegate.write(firstPassWriter);
                } catch (IOException e) {
                    fail("IOException can't be thrown here: " + e);
                }
            }
        }

        try {
            testCase.invokeWriterDelegate.write(writer);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify writer's output (report the index of first mismatching byte)
        byte[] writtenBytes = baos.toByteArray();
        int minLength = Math.min(bytes.length, writtenBytes.length);
        for (int i = 0; i < minLength; ++i) {
            if (bytes[i] != writtenBytes[i]) {
                int x = 0;
            }
            assertEquals("Binary writer's output must match expected at position " + i, bytes[i], writtenBytes[i]);
        }

        // verify writer's output (checks length as well)
        assertArrayEquals("Binary writer's output must match expected everywhere", bytes, writtenBytes);

        // create reader
        ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
        UntaggedProtocolReader reader = implementationFactory.createReaderDelegate.newReader(bais);

        // invoke the reader to deserialize the data and verify results
        try {
            testCase.invokeReaderDelegate.readAndVerify(reader);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify that the reader has consumed the entire payload
        assertEquals("Binary reader must consume the entire payload", 0, bais.available());
    }

    /**
     * Defines some useful sequence constants.
     */
    static final class LargeSequences {

        private static byte[] generateAsciiByteArray(int size) {
            byte[] result = new byte[size];
            final byte minChar = (byte) 0x20;
            final byte maxChar = (byte) 0x7F;
            byte current = minChar;
            for (int i = 0; i < size; ++i) {
                result[i] = current;
                ++current;
                if (current == maxChar) {
                    current = minChar;
                }
            }

            return result;
        }

        static final byte[] LargeUInt8Array1 = generateAsciiByteArray(10);
        static final byte[] LargeUInt8Array2 = generateAsciiByteArray(100);
        static final byte[] LargeUInt8Array3 = generateAsciiByteArray(1000);
        static final byte[] LargeUInt8Array4 = generateAsciiByteArray(10000);
        static final byte[] LargeUInt8Array5 = generateAsciiByteArray(100000);

        static final String LargeString1 = new String(LargeUInt8Array1);
        static final String LargeString2 = new String(LargeUInt8Array2);
        static final String LargeString3 = new String(LargeUInt8Array3);
        static final String LargeString4 = new String(LargeUInt8Array4);
        static final String LargeString5 = new String(LargeUInt8Array5);
    }

    /**
     * Container for test cases.
     */
    static final class TestCases {

        static final ProtocolTestCase EmptyStruct = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {
                    w.writeStructBegin(null);
                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    try {
                        r.readInt8();
                    } catch (IOException ioe) {
                        return;
                    }
                    fail("untagged writer wrote bytes for an empty struct");
                }
            });

        static final ProtocolTestCase EmptyStructWithEmptyBase = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {
                    w.writeStructBegin(null);
                    w.writeBaseBegin(null);
                    w.writeBaseEnd();
                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    try {
                        r.readInt8();
                    } catch (IOException ioe) {
                        return;
                    }
                    fail("untagged writer wrote bytes for an empty struct");
                }
            });

        static final ProtocolTestCase SingleFieldStruct = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {
                    w.writeStructBegin(null);
                    w.writeInt16((short) 0x1234);
                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    readInt16AndVerify(r, (short) 0x1234);
                }
            });

        static final ProtocolTestCase EmptyStructWithSingleFieldBase = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {
                    w.writeStructBegin(null);
                    w.writeBaseBegin(null);
                    w.writeInt16((short) 0x1234);
                    w.writeBaseEnd();
                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    readInt16AndVerify(r, (short) 0x1234);
                }
            });

        static final ProtocolTestCase SingleFieldStructWithSingleFieldBase = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {
                    w.writeStructBegin(null);
                    w.writeBaseBegin(null);
                    w.writeInt16((short) 0x1234);
                    w.writeBaseEnd();
                    w.writeUInt32(0xABCDEF98);
                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    readInt16AndVerify(r, (short) 0x1234);
                    readUInt32AndVerify(r, 0xABCDEF98);
                }
            });

        static final ProtocolTestCase MultiFieldStruct = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {
                    w.writeStructBegin(null);
                    w.writeInt8((byte) 0x45);
                    w.writeInt8((byte) 0xCA);
                    w.writeInt16((short) 0x57AD);
                    w.writeInt16((short) 0xB63A);
                    w.writeInt32(0x5172A3D4);
                    w.writeInt32(0xBA6B3CAD);
                    w.writeInt64(0x59187726A534D342L);
                    w.writeInt64(0xB0A162B334C5A6D7L);
                    w.writeUInt8((byte) 0x55);
                    w.writeUInt8((byte) 0xDA);
                    w.writeUInt16((short) 0x67AD);
                    w.writeUInt16((short) 0xC63A);
                    w.writeUInt32(0x6172A3D4);
                    w.writeUInt32(0xCA6B3CAD);
                    w.writeUInt64(0x69187726A534D342L);
                    w.writeUInt64(0xC0A162B334C5A6D7L);
                    w.writeBool(false);
                    w.writeBool(true);
                    w.writeFloat(5.12F);
                    w.writeFloat(-1001F);
                    w.writeDouble(5.12);
                    w.writeDouble((double) -1001);
                    w.writeString("");
                    w.writeString("\u041f\u0440\u0438\u0432\u0435\u0442 World!");
                    w.writeWString("");
                    w.writeWString("\u041f\u0440\u0438\u0432\u0435\u0442 World!");
                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    readInt8AndVerify(r, (byte) 0x45);
                    readInt8AndVerify(r, (byte) 0xCA);
                    readInt16AndVerify(r, (short) 0x57AD);
                    readInt16AndVerify(r, (short) 0xB63A);
                    readInt32AndVerify(r, 0x5172A3D4);
                    readInt32AndVerify(r, 0xBA6B3CAD);
                    readInt64AndVerify(r, 0x59187726A534D342L);
                    readInt64AndVerify(r, 0xB0A162B334C5A6D7L);
                    readUInt8AndVerify(r, (byte) 0x55);
                    readUInt8AndVerify(r, (byte) 0xDA);
                    readUInt16AndVerify(r, (short) 0x67AD);
                    readUInt16AndVerify(r, (short) 0xC63A);
                    readUInt32AndVerify(r, 0x6172A3D4);
                    readUInt32AndVerify(r, 0xCA6B3CAD);
                    readUInt64AndVerify(r, 0x69187726A534D342L);
                    readUInt64AndVerify(r, 0xC0A162B334C5A6D7L);
                    readBoolAndVerify(r, false);
                    readBoolAndVerify(r, true);
                    readFloatAndVerify(r, 5.12F);
                    readFloatAndVerify(r, -1001F);
                    readDoubleAndVerify(r, 5.12);
                    readDoubleAndVerify(r, -1001);
                    readStringAndVerify(r, "");
                    readStringAndVerify(r, "\u041f\u0440\u0438\u0432\u0435\u0442 World!");
                    readWStringAndVerify(r, "");
                    readWStringAndVerify(r, "\u041f\u0440\u0438\u0432\u0435\u0442 World!");
                }
            });

        static final ProtocolTestCase MultiListFieldStruct = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {

                    w.writeStructBegin(null);

                    // int8
                    writeInt8List(w, new byte[]{});
                    writeInt8List(w, new byte[]{(byte) 0x01});
                    writeInt8List(w, new byte[]{(byte) 0x02, (byte) 0x03});

                    // int16
                    writeInt16List(w, new short[]{});
                    writeInt16List(w, new short[]{(short) 0x01});
                    writeInt16List(w, new short[]{(short) 0x02, (byte) 0x03});

                    // int32
                    writeInt32List(w, new int[]{});
                    writeInt32List(w, new int[]{0x01});
                    writeInt32List(w, new int[]{0x02, 0x03});

                    // int64
                    writeInt64List(w, new long[]{});
                    writeInt64List(w, new long[]{0x01});
                    writeInt64List(w, new long[]{0x02L, 0x03L});

                    // uint8
                    writeUInt8List(w, new byte[]{});
                    writeUInt8List(w, new byte[]{(byte) 0x01});
                    writeUInt8List(w, new byte[]{(byte) 0x02, (byte) 0x03});

                    // uint16
                    writeUInt16List(w, new short[]{});
                    writeUInt16List(w, new short[]{(short) 0x01});
                    writeUInt16List(w, new short[]{(short) 0x02, (byte) 0x03});

                    // uint32
                    writeUInt32List(w, new int[]{});
                    writeUInt32List(w, new int[]{0x01});
                    writeUInt32List(w, new int[]{0x02, 0x03});

                    // uint64
                    writeUInt64List(w, new long[]{});
                    writeUInt64List(w, new long[]{0x01});
                    writeUInt64List(w, new long[]{0x02L, 0x03L});

                    // bool
                    writeBoolList(w, new boolean[]{});
                    writeBoolList(w, new boolean[]{false});
                    writeBoolList(w, new boolean[]{false, true});

                    // float
                    writeFloatList(w, new float[]{});
                    writeFloatList(w, new float[]{0.0F});
                    writeFloatList(w, new float[]{0.0F, -0.0F});

                    // double
                    writeDoubleList(w, new double[]{});
                    writeDoubleList(w, new double[]{0});
                    writeDoubleList(w, new double[]{0.0, -0.0});

                    // string
                    writeStringList(w, new String[]{});
                    writeStringList(w, new String[]{""});
                    writeStringList(w, new String[]{"", "test"});

                    // wstring
                    writeWStringList(w, new String[]{});
                    writeWStringList(w, new String[]{""});
                    writeWStringList(w, new String[]{"", "test"});

                    // struct list: empty
                    w.writeContainerBegin(0, BondDataType.BT_STRUCT);
                    w.writeContainerEnd();

                    // struct list: 1 element
                    w.writeContainerBegin(1, BondDataType.BT_STRUCT);
                    w.writeStructBegin(null);
                    w.writeInt8((byte) 0xFE);
                    w.writeStructEnd();
                    w.writeContainerEnd();

                    // struct list: 3 elements
                    w.writeContainerBegin(3, BondDataType.BT_STRUCT);
                    w.writeStructBegin(null);
                    w.writeInt8((byte) 0xEF);
                    w.writeStructEnd();
                    w.writeStructBegin(null);
                    w.writeInt8((byte) 0xA0);
                    w.writeInt8((byte) 0xA1);
                    w.writeStructEnd();
                    w.writeStructBegin(null);
                    writeInt8List(w, new byte[]{(byte) 0xFA, (byte) 0xFB, (byte) 0xFC});
                    w.writeStructEnd();
                    w.writeContainerEnd();

                    // list list: empty
                    w.writeContainerBegin(0, BondDataType.BT_LIST);
                    w.writeContainerEnd();

                    // list list: 1 element
                    w.writeContainerBegin(1, BondDataType.BT_LIST);
                    w.writeContainerBegin(3, BondDataType.BT_UINT8);
                    w.writeBytes(new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C});
                    w.writeContainerEnd();
                    w.writeContainerEnd();

                    // set list: empty
                    w.writeContainerBegin(0, BondDataType.BT_SET);
                    w.writeContainerEnd();

                    // set list: 1 element
                    w.writeContainerBegin(1, BondDataType.BT_SET);
                    w.writeContainerBegin(3, BondDataType.BT_UINT8);
                    w.writeBytes(new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C});
                    w.writeContainerEnd();
                    w.writeContainerEnd();

                    // map list: empty
                    w.writeContainerBegin(0, BondDataType.BT_MAP);
                    w.writeContainerEnd();

                    // map list: 1 element
                    w.writeContainerBegin(1, BondDataType.BT_MAP);
                    w.writeContainerBegin(2, BondDataType.BT_BOOL, BondDataType.BT_UINT8);
                    w.writeBool(false);
                    w.writeUInt8((byte) 0xF0);
                    w.writeBool(true);
                    w.writeUInt8((byte) 0xF1);
                    w.writeContainerEnd();
                    w.writeContainerEnd();

                    // map list: 2 elements
                    w.writeContainerBegin(2, BondDataType.BT_MAP);
                    w.writeContainerBegin(2, BondDataType.BT_BOOL, BondDataType.BT_STRING);
                    w.writeBool(true);
                    w.writeString("true");
                    w.writeBool(false);
                    w.writeString("false");
                    w.writeContainerEnd();
                    w.writeContainerBegin(1, BondDataType.BT_INT8, BondDataType.BT_STRUCT);
                    w.writeInt8((byte) 0);
                    w.writeStructBegin(null);
                    w.writeInt16((short) 0xABBA);
                    w.writeStructEnd();
                    w.writeContainerEnd();
                    w.writeContainerEnd();

                    // blob (list of uint8)
                    writeBlob(w, new byte[]{});
                    writeBlob(w, new byte[]{(byte) 0x01});
                    writeBlob(w, new byte[]{(byte) 0x02, (byte) 0x03});

                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    int count;

                    // int8
                    readInt8ListAndVerify(r, new byte[]{});
                    readInt8ListAndVerify(r, new byte[]{(byte) 0x01});
                    readInt8ListAndVerify(r, new byte[]{(byte) 0x02, (byte) 0x03});

                    // int16
                    readInt16ListAndVerify(r, new short[]{});
                    readInt16ListAndVerify(r, new short[]{(short) 0x01});
                    readInt16ListAndVerify(r, new short[]{(short) 0x02, (short) 0x03});

                    // int32
                    readInt32ListAndVerify(r, new int[]{});
                    readInt32ListAndVerify(r, new int[]{0x01});
                    readInt32ListAndVerify(r, new int[]{0x02, 0x03});

                    // int64
                    readInt64ListAndVerify(r, new long[]{});
                    readInt64ListAndVerify(r, new long[]{0x01L});
                    readInt64ListAndVerify(r, new long[]{0x02L, 0x03L});

                    // uint8
                    readUInt8ListAndVerify(r, new byte[]{});
                    readUInt8ListAndVerify(r, new byte[]{(byte) 0x01});
                    readUInt8ListAndVerify(r, new byte[]{(byte) 0x02, (byte) 0x03});

                    // uint16
                    readUInt16ListAndVerify(r, new short[]{});
                    readUInt16ListAndVerify(r, new short[]{(short) 0x01});
                    readUInt16ListAndVerify(r, new short[]{(short) 0x02, (short) 0x03});

                    // uint32
                    readUInt32ListAndVerify(r, new int[]{});
                    readUInt32ListAndVerify(r, new int[]{0x01});
                    readUInt32ListAndVerify(r, new int[]{0x02, 0x03});

                    // uint64
                    readUInt64ListAndVerify(r, new long[]{});
                    readUInt64ListAndVerify(r, new long[]{0x01L});
                    readUInt64ListAndVerify(r, new long[]{0x02L, 0x03L});

                    // bool
                    readBoolListAndVerify(r, new boolean[]{});
                    readBoolListAndVerify(r, new boolean[]{false});
                    readBoolListAndVerify(r, new boolean[]{false, true});

                    // float
                    readFloatListAndVerify(r, new float[]{});
                    readFloatListAndVerify(r, new float[]{0.0F});
                    readFloatListAndVerify(r, new float[]{0.0F, -0.0F});

                    // double
                    readDoubleListAndVerify(r, new double[]{});
                    readDoubleListAndVerify(r, new double[]{0.0});
                    readDoubleListAndVerify(r, new double[]{0.0, -0.0});

                    // string
                    readStringListAndVerify(r, new String[]{});
                    readStringListAndVerify(r, new String[]{""});
                    readStringListAndVerify(r, new String[]{"", "test"});

                    // wstring
                    readWStringListAndVerify(r, new String[]{});
                    readWStringListAndVerify(r, new String[]{""});
                    readWStringListAndVerify(r, new String[]{"", "test"});

                    // struct list: empty
                    assertEquals("Count must match", 0, r.readContainerBegin());
                    r.readContainerEnd();

                    // struct list: 1 element
                    assertEquals("Count must match", 1, r.readContainerBegin());
                    r.readContainerEnd();
                    readInt8AndVerify(r, (byte) 0xFE);
                    r.readContainerEnd();

                    // struct list: 3 elements
                    assertEquals("Count must match", 3, r.readContainerBegin());
                    readInt8AndVerify(r, (byte) 0xEF);
                    readInt8AndVerify(r, (byte) 0xA0);
                    readInt8AndVerify(r, (byte) 0xA1);
                    readInt8ListAndVerify(r, new byte[]{(byte) 0xFA, (byte) 0xFB, (byte) 0xFC});
                    r.readContainerEnd();

                    // list list: empty
                    Assert.assertEquals(0, r.readContainerBegin());
                    r.readContainerEnd();

                    // list list: 1 element
                    assertEquals("Count must match", 1, r.readContainerBegin());
                    assertEquals("Count must match", 3, r.readContainerBegin());
                    assertArrayEquals("Value byte array must match",
                        new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C}, r.readBytes(3));
                    r.readContainerEnd();
                    r.readContainerEnd();

                    // set list: empty
                    Assert.assertEquals(0, r.readContainerBegin());
                    r.readContainerEnd();

                    // set list: 1 element
                    assertEquals("Count must match", 1, r.readContainerBegin());
                    assertEquals("Count must match", 3, r.readContainerBegin());
                    assertArrayEquals("Value byte array must match",
                        new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C}, r.readBytes(3));
                    r.readContainerEnd();
                    r.readContainerEnd();

                    // map list: empty
                    Assert.assertEquals(0, r.readContainerBegin());
                    r.readContainerEnd();

                    // map list: 1 element
                    assertEquals("Count must match", 1, r.readContainerBegin());
                    assertEquals("Count must match", 2, r.readContainerBegin());
                    assertEquals(false, r.readBool());
                    assertEquals((byte) 0xF0, r.readUInt8());
                    assertEquals(true, r.readBool());
                    assertEquals((byte) 0xF1, r.readUInt8());
                    r.readContainerEnd();
                    r.readContainerEnd();

                    // map list: 2 elements
                    assertEquals("Count must match", 2, r.readContainerBegin());
                    assertEquals("Count must match", 2, r.readContainerBegin());
                    assertEquals(true, r.readBool());
                    assertEquals("true", r.readString());
                    assertEquals(false, r.readBool());
                    assertEquals("false", r.readString());
                    r.readContainerEnd();
                    assertEquals("Count must match", 1, r.readContainerBegin());
                    assertEquals(0, r.readInt8());
                    readInt16AndVerify(r, (short) 0xABBA);
                    r.readContainerEnd();
                    r.readContainerEnd();

                    // blob (list of uint8)
                    readBlobAndVerify(r, new byte[]{});
                    readBlobAndVerify(r, new byte[]{(byte) 0x01});
                    readBlobAndVerify(r, new byte[]{(byte) 0x02, (byte) 0x03});
                }
            });

        static final ProtocolTestCase LargeFieldStruct = new ProtocolTestCase(
            new UntaggedBinaryTestHelper.InvokeWriter() {
                @Override
                void write(ProtocolWriter w) throws IOException {

                    w.writeStructBegin(null);

                    w.writeString(LargeSequences.LargeString1);
                    w.writeString(LargeSequences.LargeString2);
                    w.writeString(LargeSequences.LargeString3);
                    w.writeString(LargeSequences.LargeString4);
                    w.writeString(LargeSequences.LargeString5);

                    writeUInt8List(w, LargeSequences.LargeUInt8Array1);
                    writeUInt8List(w, LargeSequences.LargeUInt8Array2);
                    writeUInt8List(w, LargeSequences.LargeUInt8Array3);
                    writeUInt8List(w, LargeSequences.LargeUInt8Array4);
                    writeUInt8List(w, LargeSequences.LargeUInt8Array5);

                    w.writeStructEnd();
                }
            },
            new UntaggedBinaryTestHelper.InvokeReader() {
                @Override
                void readAndVerify(UntaggedProtocolReader r) throws IOException {
                    readStringAndVerify(r, LargeSequences.LargeString1);
                    readStringAndVerify(r, LargeSequences.LargeString2);
                    readStringAndVerify(r, LargeSequences.LargeString3);
                    readStringAndVerify(r, LargeSequences.LargeString4);
                    readStringAndVerify(r, LargeSequences.LargeString5);

                    readUInt8ListAndVerify(r, LargeSequences.LargeUInt8Array1);
                    readUInt8ListAndVerify(r, LargeSequences.LargeUInt8Array2);
                    readUInt8ListAndVerify(r, LargeSequences.LargeUInt8Array3);
                    readUInt8ListAndVerify(r, LargeSequences.LargeUInt8Array4);
                    readUInt8ListAndVerify(r, LargeSequences.LargeUInt8Array5);
                }
            });
    }

    private static void writeInt8List(ProtocolWriter w, byte[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_INT8);
        for (byte v : value) {
            w.writeInt8(v);
        }
        w.writeContainerEnd();
    }

    private static void writeInt16List(ProtocolWriter w, short[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_INT16);
        for (short v : value) {
            w.writeInt16(v);
        }
        w.writeContainerEnd();
    }

    private static void writeInt32List(ProtocolWriter w, int[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_INT32);
        for (int v : value) {
            w.writeInt32(v);
        }
        w.writeContainerEnd();
    }

    private static void writeInt64List(ProtocolWriter w, long[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_INT64);
        for (long v : value) {
            w.writeInt64(v);
        }
        w.writeContainerEnd();
    }

    private static void writeUInt8List(ProtocolWriter w, byte[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_UINT8);
        for (byte v : value) {
            w.writeUInt8(v);
        }
        w.writeContainerEnd();
    }

    private static void writeUInt16List(ProtocolWriter w, short[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_UINT16);
        for (short v : value) {
            w.writeUInt16(v);
        }
        w.writeContainerEnd();
    }

    private static void writeUInt32List(ProtocolWriter w, int[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_UINT32);
        for (int v : value) {
            w.writeUInt32(v);
        }
        w.writeContainerEnd();
    }

    private static void writeUInt64List(ProtocolWriter w, long[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_UINT64);
        for (long v : value) {
            w.writeUInt64(v);
        }
        w.writeContainerEnd();
    }

    private static void writeBoolList(ProtocolWriter w, boolean[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_BOOL);
        for (boolean v : value) {
            w.writeBool(v);
        }
        w.writeContainerEnd();
    }

    private static void writeFloatList(ProtocolWriter w, float[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_FLOAT);
        for (float v : value) {
            w.writeFloat(v);
        }
        w.writeContainerEnd();
    }

    private static void writeDoubleList(ProtocolWriter w, double[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_DOUBLE);
        for (double v : value) {
            w.writeDouble(v);
        }
        w.writeContainerEnd();
    }

    private static void writeStringList(ProtocolWriter w, String[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_STRING);
        for (String v : value) {
            w.writeString(v);
        }
        w.writeContainerEnd();
    }

    private static void writeWStringList(ProtocolWriter w, String[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_WSTRING);
        for (String v : value) {
            w.writeWString(v);
        }
        w.writeContainerEnd();
    }

    // A variant of writeUInt8ListField that write entire byte array rather than individual bytes
    private static void writeBlob(ProtocolWriter w, byte[] value) throws IOException {
        w.writeContainerBegin(value.length, BondDataType.BT_UINT8);
        w.writeBytes(value);
        w.writeContainerEnd();
    }

    private static void readInt8AndVerify(UntaggedProtocolReader r, byte value) throws IOException {
        assertEquals("Value must match", value, r.readInt8());
    }

    private static void readInt16AndVerify(UntaggedProtocolReader r, short value) throws IOException {
        assertEquals("Value must match", value, r.readInt16());
    }

    private static void readInt32AndVerify(UntaggedProtocolReader r, int value) throws IOException {
        assertEquals("Value must match", value, r.readInt32());
    }

    private static void readInt64AndVerify(UntaggedProtocolReader r, long value) throws IOException {
        assertEquals("Value must match", value, r.readInt64());
    }

    private static void readUInt8AndVerify(UntaggedProtocolReader r, byte value) throws IOException {
        assertEquals("Value must match", value, r.readUInt8());
    }

    private static void readUInt16AndVerify(UntaggedProtocolReader r, short value) throws IOException {
        assertEquals("Value must match", value, r.readUInt16());
    }

    private static void readUInt32AndVerify(UntaggedProtocolReader r, int value) throws IOException {
        assertEquals("Value must match", value, r.readUInt32());
    }

    private static void readUInt64AndVerify(
        UntaggedProtocolReader r, long value) throws IOException {
        assertEquals("Value must match", value, r.readUInt64());
    }

    private static void readBoolAndVerify(UntaggedProtocolReader r, boolean value) throws IOException {
        assertEquals("Value must match", value, r.readBool());
    }

    private static void readFloatAndVerify(UntaggedProtocolReader r, float value) throws IOException {
        float actual = r.readFloat();
        assertEquals("Value must match", value, actual, 0F);
        assertEquals("Raw bits must match", Float.floatToRawIntBits(value), Float.floatToRawIntBits(actual));
    }

    private static void readDoubleAndVerify(UntaggedProtocolReader r, double value) throws IOException {
        double actual = r.readDouble();
        assertEquals("Value must match", value, actual, 0F);
        assertEquals("Raw bits must match", Double.doubleToRawLongBits(value), Double.doubleToRawLongBits(actual));
    }

    private static void readStringAndVerify(UntaggedProtocolReader r, String value) throws IOException {
        assertEquals("Value must match", value, r.readString());
    }

    private static void readWStringAndVerify(UntaggedProtocolReader r, String value) throws IOException {
        assertEquals("Value must match", value, r.readWString());
    }

    private static void readInt8ListAndVerify(UntaggedProtocolReader r, byte[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt8());
        }
        r.readContainerEnd();
    }

    private static void readInt16ListAndVerify(UntaggedProtocolReader r, short[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt16());
        }
        r.readContainerEnd();
    }

    private static void readInt32ListAndVerify(UntaggedProtocolReader r, int[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt32());
        }
        r.readContainerEnd();
    }

    private static void readInt64ListAndVerify(UntaggedProtocolReader r, long[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt64());
        }
        r.readContainerEnd();
    }

    private static void readUInt8ListAndVerify(UntaggedProtocolReader r, byte[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt8());
        }
        r.readContainerEnd();
    }

    private static void readUInt16ListAndVerify(UntaggedProtocolReader r, short[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt16());
        }
        r.readContainerEnd();
    }

    private static void readUInt32ListAndVerify(UntaggedProtocolReader r, int[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt32());
        }
        r.readContainerEnd();
    }

    private static void readUInt64ListAndVerify(UntaggedProtocolReader r, long[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt64());
        }
        r.readContainerEnd();
    }

    private static void readBoolListAndVerify(UntaggedProtocolReader r, boolean[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readBool());
        }
        r.readContainerEnd();
    }

    private static void readFloatListAndVerify(UntaggedProtocolReader r, float[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            float element = r.readFloat();
            assertEquals("Element value must match", value[i], element, 0F);
            assertEquals("Element value bits must match",
                Float.floatToRawIntBits(value[i]), Float.floatToRawIntBits(element));
        }
        r.readContainerEnd();
    }

    private static void readDoubleListAndVerify(UntaggedProtocolReader r, double[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            double element = r.readDouble();
            assertEquals("Element value must match", value[i], element, 0);
            assertEquals("Element value bits must match",
                Double.doubleToRawLongBits(value[i]), Double.doubleToRawLongBits(element));
        }
        r.readContainerEnd();
    }

    private static void readStringListAndVerify(UntaggedProtocolReader r, String[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readString());
        }
        r.readContainerEnd();
    }

    private static void readWStringListAndVerify(UntaggedProtocolReader r, String[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readWString());
        }
        r.readContainerEnd();
    }

    // A variant of readUInt8ListAndVerify that reads entire byte array rather than individual bytes
    private static void readBlobAndVerify(UntaggedProtocolReader r, byte[] value) throws IOException {
        assertEquals(value.length, r.readContainerBegin());
        byte[] actualValue = r.readBytes(value.length);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], actualValue[i]);
        }
        r.readContainerEnd();
    }
}
