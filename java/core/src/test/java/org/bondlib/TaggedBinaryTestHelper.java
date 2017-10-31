// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.AbstractList;
import java.util.ArrayList;
import java.util.Stack;

import static org.junit.Assert.*;

/**
 * Contains helpers for implementing unit tests for tagged binary protocol readers and writers.
 */
public final class TaggedBinaryTestHelper {

    /**
     * Contains information to call the reader's skip method and verify the results.
     * Used for testing the method.
     */
    public static final class SkipInfo {
        public int offset;
        public BondDataType type;
        public int length;
    }

    /**
     * A list of {@see SkipInfo} that is passed to writer, which is expected to call
     * its methods to mark the start and the end of a value.
     */
    public static final class SkipInfoList extends AbstractList<SkipInfo> {
        private final ArrayList<SkipInfo> list;
        private final ByteArrayOutputStream outputStream;
        private final Stack<Integer> offsetStack;

        private SkipInfoList(ByteArrayOutputStream outputStream) {
            this.list = new ArrayList<SkipInfo>();
            this.outputStream = outputStream;
            this.offsetStack = new Stack<Integer>();
        }

        public void markValueStart() {
            this.offsetStack.push(this.outputStream.size());
        }

        public void recordValue(BondDataType type) {
            SkipInfo skipInfo = new SkipInfo();
            skipInfo.offset = this.offsetStack.pop();
            skipInfo.type = type;
            skipInfo.length = this.outputStream.size() - skipInfo.offset;
            this.list.add(skipInfo);
        }

        @Override
        public SkipInfo get(int index) {
            return this.list.get(index);
        }

        @Override
        public int size() {
            return this.list.size();
        }
    }

    /**
     * A delegate responsible for invoking user code for writing/serialization.
     */
    public static abstract class InvokeWriter {
        /**
         * Serializes data and determines the test set for skipping.
         *
         * @param w         the writer
         * @param skipTests collection of test cases for testing the skipping functionality, null if unused
         * @throws IOException
         */
        public abstract void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException;
    }

    /**
     * A delegate responsible for invoking user code for reading/deserialization.
     */
    public static abstract class InvokeReader {
        /**
         * Deserializes data and verifies results as it reads.
         *
         * @param r the reader
         * @throws IOException
         */
        public abstract void readAndVerify(TaggedProtocolReader r) throws IOException;
    }

    /**
     * A delegate responsible for creating writers.
     */
    public static abstract class CreateWriter {
        /**
         * Creates a new writer.
         *
         * @param outputStream the stream to write to
         * @return writer
         */
        public abstract ProtocolWriter newWriter(ByteArrayOutputStream outputStream);
    }

    /**
     * A delegate responsible for creating readers.
     */
    public static abstract class CreateReader {
        /**
         * Creates a new reader.
         *
         * @param inputStream the stream to read from
         * @return reader
         */
        public abstract TaggedProtocolReader newReader(ByteArrayInputStream inputStream);
    }

    /**
     * A tuple of {@see InvokeWriter} and {@see InvokeReader} that form a particular test case for binary protocols.
     * A specific protocol needs to provide the payload data and {@see ProtocolImplementations} factories,
     * and then call the method {@see testProtocol}.
     */
    public static final class ProtocolTestCase {
        public final InvokeWriter invokeWriterDelegate;
        public final InvokeReader invokeReaderDelegate;

        public ProtocolTestCase(InvokeWriter invokeWriterDelegate, InvokeReader invokeReaderDelegate) {
            this.invokeWriterDelegate = invokeWriterDelegate;
            this.invokeReaderDelegate = invokeReaderDelegate;
        }
    }

    /**
     * A tuple of {@see CreateWriter} and {@see CreateReader} that provide implementation of a protocol.
     */
    public static final class ProtocolImplementationFactory {
        public final CreateWriter createWriterDelegate;
        public final CreateReader createReaderDelegate;

        public ProtocolImplementationFactory(CreateWriter createWriterDelegate, CreateReader createReaderDelegate) {
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
    public static void testProtocol(
            byte[] bytes,
            ProtocolTestCase testCase,
            ProtocolImplementationFactory implementationFactory) {

        // create writer and invoke first pass if needed
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ProtocolWriter writer = implementationFactory.createWriterDelegate.newWriter(baos);
        if (writer instanceof TwoPassProtocolWriter) {
            ProtocolWriter firstPassWriter = ((TwoPassProtocolWriter) writer).getFirstPassWriter();
            if (firstPassWriter != null) {
                try {
                    testCase.invokeWriterDelegate.write(firstPassWriter, null);
                } catch (IOException e) {
                    fail("IOException can't be thrown here: " + e);
                }
            }
        }

        // invoke the writer to serialize the data
        SkipInfoList skipTests = new SkipInfoList(baos);
        try {
            testCase.invokeWriterDelegate.write(writer, skipTests);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify writer's output (report the index of first mismatching byte)
        byte[] writtenBytes = baos.toByteArray();
        int minLength = Math.min(bytes.length, writtenBytes.length);
        for (int i = 0; i < minLength; ++i) {
            assertEquals("Binary writer's output must match expected at position " + i, bytes[i], writtenBytes[i]);
        }

        // verify writer's output (checks length as well)
        assertArrayEquals("Binary writer's output must match expected everywhere", bytes, writtenBytes);

        // create reader
        ByteArrayInputStream bais = new ByteArrayInputStream(bytes);
        TaggedProtocolReader reader = implementationFactory.createReaderDelegate.newReader(bais);

        // invoke the reader to deserialize the data and verify results
        try {
            testCase.invokeReaderDelegate.readAndVerify(reader);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify that the reader has consumed the entire payload
        assertEquals("Binary reader must consume the entire payload", 0, bais.available());

        // verify skip functionality for every skippable value determined by the writer
        for (SkipInfo skipTest : skipTests) {
            bais = new ByteArrayInputStream(bytes);
            bais.skip(skipTest.offset);
            reader = implementationFactory.createReaderDelegate.newReader(bais);
            try {
                reader.skip(skipTest.type);
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }

            int skippedCount = bytes.length - bais.available() - skipTest.offset;
            assertEquals("Skip method must skip the correct number of bytes", skipTest.length, skippedCount);
        }
    }

    /**
     * Defines some useful sequence constants.
     */
    public static final class LargeSequences {

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

        public static final byte[] LargeUInt8Array1 = generateAsciiByteArray(10);
        public static final byte[] LargeUInt8Array2 = generateAsciiByteArray(100);
        public static final byte[] LargeUInt8Array3 = generateAsciiByteArray(1000);
        public static final byte[] LargeUInt8Array4 = generateAsciiByteArray(10000);
        public static final byte[] LargeUInt8Array5 = generateAsciiByteArray(100000);

        public static final String LargeString1 = new String(LargeUInt8Array1);
        public static final String LargeString2 = new String(LargeUInt8Array2);
        public static final String LargeString3 = new String(LargeUInt8Array3);
        public static final String LargeString4 = new String(LargeUInt8Array4);
        public static final String LargeString5 = new String(LargeUInt8Array5);
    }

    /**
     * Container for test cases.
     */
    public static final class TestCases {

        public static final ProtocolTestCase EmptyStruct = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase EmptyStructWithEmptyBase = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeBaseBegin(w, skipTests);
                        writeBaseEnd(w, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readBaseBeginAndVerify(r);
                        readFieldAndVerifyBaseEnd(r);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase SingleFieldStruct = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeInt16Field(w, (short) 0x1234, 1, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readInt16FieldAndVerify(r, (short) 0x1234, 1);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase EmptyStructWithSingleFieldBase = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeBaseBegin(w, skipTests);
                        writeInt16Field(w, (short) 0x1234, 1, skipTests);
                        writeBaseEnd(w, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readBaseBeginAndVerify(r);
                        readInt16FieldAndVerify(r, (short) 0x1234, 1);
                        readFieldAndVerifyBaseEnd(r);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase SingleFieldStructWithSingleFieldBase = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeBaseBegin(w, skipTests);
                        writeInt16Field(w, (short) 0x1234, 1, skipTests);
                        writeBaseEnd(w, skipTests);
                        writeUInt32Field(w, 0xABCDEF98, 2, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readBaseBeginAndVerify(r);
                        readInt16FieldAndVerify(r, (short) 0x1234, 1);
                        readFieldAndVerifyBaseEnd(r);
                        readUInt32FieldAndVerify(r, 0xABCDEF98, 2);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase MultiFieldStruct = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeInt8Field(w, (byte) 0x45, 20, skipTests);
                        writeInt8Field(w, (byte) 0xCA, 21, skipTests);
                        writeInt16Field(w, (short) 0x57AD, 22, skipTests);
                        writeInt16Field(w, (short) 0xB63A, 23, skipTests);
                        writeInt32Field(w, 0x5172A3D4, 24, skipTests);
                        writeInt32Field(w, 0xBA6B3CAD, 25, skipTests);
                        writeInt64Field(w, 0x59187726A534D342L, 26, skipTests);
                        writeInt64Field(w, 0xB0A162B334C5A6D7L, 27, skipTests);
                        writeUInt8Field(w, (byte) 0x55, 30, skipTests);
                        writeUInt8Field(w, (byte) 0xDA, 31, skipTests);
                        writeUInt16Field(w, (short) 0x67AD, 32, skipTests);
                        writeUInt16Field(w, (short) 0xC63A, 33, skipTests);
                        writeUInt32Field(w, 0x6172A3D4, 34, skipTests);
                        writeUInt32Field(w, 0xCA6B3CAD, 35, skipTests);
                        writeUInt64Field(w, 0x69187726A534D342L, 36, skipTests);
                        writeUInt64Field(w, 0xC0A162B334C5A6D7L, 37, skipTests);
                        writeBoolField(w, false, 40, skipTests);
                        writeBoolField(w, true, 41, skipTests);
                        writeFloatField(w, 5.12F, 50, skipTests);
                        writeFloatField(w, -1001F, 51, skipTests);
                        writeDoubleField(w, 5.12, 60, skipTests);
                        writeDoubleField(w, -1001, 61, skipTests);
                        writeStringField(w, "", 70, skipTests);
                        writeStringField(w, "\u041f\u0440\u0438\u0432\u0435\u0442 World!", 71, skipTests);
                        writeWStringField(w, "", 80, skipTests);
                        writeWStringField(w, "\u041f\u0440\u0438\u0432\u0435\u0442 World!", 81, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readInt8FieldAndVerify(r, (byte) 0x45, 20);
                        readInt8FieldAndVerify(r, (byte) 0xCA, 21);
                        readInt16FieldAndVerify(r, (short) 0x57AD, 22);
                        readInt16FieldAndVerify(r, (short) 0xB63A, 23);
                        readInt32FieldAndVerify(r, 0x5172A3D4, 24);
                        readInt32FieldAndVerify(r, 0xBA6B3CAD, 25);
                        readInt64FieldAndVerify(r, 0x59187726A534D342L, 26);
                        readInt64FieldAndVerify(r, 0xB0A162B334C5A6D7L, 27);
                        readUInt8FieldAndVerify(r, (byte) 0x55, 30);
                        readUInt8FieldAndVerify(r, (byte) 0xDA, 31);
                        readUInt16FieldAndVerify(r, (short) 0x67AD, 32);
                        readUInt16FieldAndVerify(r, (short) 0xC63A, 33);
                        readUInt32FieldAndVerify(r, 0x6172A3D4, 34);
                        readUInt32FieldAndVerify(r, 0xCA6B3CAD, 35);
                        readUInt64FieldAndVerify(r, 0x69187726A534D342L, 36);
                        readUInt64FieldAndVerify(r, 0xC0A162B334C5A6D7L, 37);
                        readBoolFieldAndVerify(r, false, 40);
                        readBoolFieldAndVerify(r, true, 41);
                        readFloatFieldAndVerify(r, 5.12F, 50);
                        readFloatFieldAndVerify(r, -1001F, 51);
                        readDoubleFieldAndVerify(r, 5.12, 60);
                        readDoubleFieldAndVerify(r, -1001, 61);
                        readStringFieldAndVerify(r, "", 70);
                        readStringFieldAndVerify(r, "\u041f\u0440\u0438\u0432\u0435\u0442 World!", 71);
                        readWStringFieldAndVerify(r, "", 80);
                        readWStringFieldAndVerify(r, "\u041f\u0440\u0438\u0432\u0435\u0442 World!", 81);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase MultiFieldStructWithSmallIds = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeInt8Field(w, (byte) 0x45, 0, skipTests);
                        writeInt8Field(w, (byte) 0xCA, 1, skipTests);
                        writeInt16Field(w, (short) 0x57AD, 2, skipTests);
                        writeInt16Field(w, (short) 0xB63A, 3, skipTests);
                        writeInt32Field(w, 0x5172A3D4, 4, skipTests);
                        writeInt32Field(w, 0xBA6B3CAD, 5, skipTests);
                        writeInt64Field(w, 0x59187726A534D342L, 6, skipTests);
                        writeInt64Field(w, 0xB0A162B334C5A6D7L, 7, skipTests);
                        writeUInt8Field(w, (byte) 0x55, 8, skipTests);
                        writeUInt8Field(w, (byte) 0xDA, 9, skipTests);
                        writeUInt16Field(w, (short) 0x67AD, 10, skipTests);
                        writeUInt16Field(w, (short) 0xC63A, 11, skipTests);
                        writeUInt32Field(w, 0x6172A3D4, 12, skipTests);
                        writeUInt32Field(w, 0xCA6B3CAD, 13, skipTests);
                        writeUInt64Field(w, 0x69187726A534D342L, 14, skipTests);
                        writeUInt64Field(w, 0xC0A162B334C5A6D7L, 15, skipTests);
                        writeBoolField(w, false, 16, skipTests);
                        writeBoolField(w, true, 17, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readInt8FieldAndVerify(r, (byte) 0x45, 0);
                        readInt8FieldAndVerify(r, (byte) 0xCA, 1);
                        readInt16FieldAndVerify(r, (short) 0x57AD, 2);
                        readInt16FieldAndVerify(r, (short) 0xB63A, 3);
                        readInt32FieldAndVerify(r, 0x5172A3D4, 4);
                        readInt32FieldAndVerify(r, 0xBA6B3CAD, 5);
                        readInt64FieldAndVerify(r, 0x59187726A534D342L, 6);
                        readInt64FieldAndVerify(r, 0xB0A162B334C5A6D7L, 7);
                        readUInt8FieldAndVerify(r, (byte) 0x55, 8);
                        readUInt8FieldAndVerify(r, (byte) 0xDA, 9);
                        readUInt16FieldAndVerify(r, (short) 0x67AD, 10);
                        readUInt16FieldAndVerify(r, (short) 0xC63A, 11);
                        readUInt32FieldAndVerify(r, 0x6172A3D4, 12);
                        readUInt32FieldAndVerify(r, 0xCA6B3CAD, 13);
                        readUInt64FieldAndVerify(r, 0x69187726A534D342L, 14);
                        readUInt64FieldAndVerify(r, 0xC0A162B334C5A6D7L, 15);
                        readBoolFieldAndVerify(r, false, 16);
                        readBoolFieldAndVerify(r, true, 17);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase MultiFieldStructWithLargeIds = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        writeStructBegin(w, skipTests);
                        writeInt8Field(w, (byte) 0x45, 200, skipTests);
                        writeInt8Field(w, (byte) 0xCA, 210, skipTests);
                        writeInt16Field(w, (short) 0x57AD, 220, skipTests);
                        writeInt16Field(w, (short) 0xB63A, 230, skipTests);
                        writeInt32Field(w, 0x5172A3D4, 240, skipTests);
                        writeInt32Field(w, 0xBA6B3CAD, 250, skipTests);
                        writeInt64Field(w, 0x59187726A534D342L, 260, skipTests);
                        writeInt64Field(w, 0xB0A162B334C5A6D7L, 270, skipTests);
                        writeUInt8Field(w, (byte) 0x55, 3000, skipTests);
                        writeUInt8Field(w, (byte) 0xDA, 3100, skipTests);
                        writeUInt16Field(w, (short) 0x67AD, 3200, skipTests);
                        writeUInt16Field(w, (short) 0xC63A, 3300, skipTests);
                        writeUInt32Field(w, 0x6172A3D4, 3400, skipTests);
                        writeUInt32Field(w, 0xCA6B3CAD, 3500, skipTests);
                        writeUInt64Field(w, 0x69187726A534D342L, 3600, skipTests);
                        writeUInt64Field(w, 0xC0A162B334C5A6D7L, 3700, skipTests);
                        writeBoolField(w, false, 40000, skipTests);
                        writeBoolField(w, true, 41000, skipTests);
                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);
                        readInt8FieldAndVerify(r, (byte) 0x45, 200);
                        readInt8FieldAndVerify(r, (byte) 0xCA, 210);
                        readInt16FieldAndVerify(r, (short) 0x57AD, 220);
                        readInt16FieldAndVerify(r, (short) 0xB63A, 230);
                        readInt32FieldAndVerify(r, 0x5172A3D4, 240);
                        readInt32FieldAndVerify(r, 0xBA6B3CAD, 250);
                        readInt64FieldAndVerify(r, 0x59187726A534D342L, 260);
                        readInt64FieldAndVerify(r, 0xB0A162B334C5A6D7L, 270);
                        readUInt8FieldAndVerify(r, (byte) 0x55, 3000);
                        readUInt8FieldAndVerify(r, (byte) 0xDA, 3100);
                        readUInt16FieldAndVerify(r, (short) 0x67AD, 3200);
                        readUInt16FieldAndVerify(r, (short) 0xC63A, 3300);
                        readUInt32FieldAndVerify(r, 0x6172A3D4, 3400);
                        readUInt32FieldAndVerify(r, 0xCA6B3CAD, 3500);
                        readUInt64FieldAndVerify(r, 0x69187726A534D342L, 3600);
                        readUInt64FieldAndVerify(r, 0xC0A162B334C5A6D7L, 3700);
                        readBoolFieldAndVerify(r, false, 40000);
                        readBoolFieldAndVerify(r, true, 41000);
                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase MultiListFieldStruct = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {
                        final boolean isSet = false;

                        writeStructBegin(w, skipTests);

                        // int8
                        writeInt8ListField(w, new byte[]{}, 10, skipTests, isSet);
                        writeInt8ListField(w, new byte[]{(byte) 0x01}, 11, skipTests, isSet);
                        writeInt8ListField(w, new byte[]{(byte) 0x02, (byte) 0x03}, 12, skipTests, isSet);

                        // int16
                        writeInt16ListField(w, new short[]{}, 20, skipTests, isSet);
                        writeInt16ListField(w, new short[]{(short) 0x01}, 21, skipTests, isSet);
                        writeInt16ListField(w, new short[]{(short) 0x02, (byte) 0x03}, 22, skipTests, isSet);

                        // int32
                        writeInt32ListField(w, new int[]{}, 30, skipTests, isSet);
                        writeInt32ListField(w, new int[]{0x01}, 31, skipTests, isSet);
                        writeInt32ListField(w, new int[]{0x02, 0x03}, 32, skipTests, isSet);

                        // int64
                        writeInt64ListField(w, new long[]{}, 40, skipTests, isSet);
                        writeInt64ListField(w, new long[]{0x01}, 41, skipTests, isSet);
                        writeInt64ListField(w, new long[]{0x02L, 0x03L}, 42, skipTests, isSet);

                        // uint8
                        writeUInt8ListField(w, new byte[]{}, 50, skipTests, isSet);
                        writeUInt8ListField(w, new byte[]{(byte) 0x01}, 51, skipTests, isSet);
                        writeUInt8ListField(w, new byte[]{(byte) 0x02, (byte) 0x03}, 52, skipTests, isSet);

                        // uint16
                        writeUInt16ListField(w, new short[]{}, 60, skipTests, isSet);
                        writeUInt16ListField(w, new short[]{(short) 0x01}, 61, skipTests, isSet);
                        writeUInt16ListField(w, new short[]{(short) 0x02, (byte) 0x03}, 62, skipTests, isSet);

                        // uint32
                        writeUInt32ListField(w, new int[]{}, 70, skipTests, isSet);
                        writeUInt32ListField(w, new int[]{0x01}, 71, skipTests, isSet);
                        writeUInt32ListField(w, new int[]{0x02, 0x03}, 72, skipTests, isSet);

                        // uint64
                        writeUInt64ListField(w, new long[]{}, 80, skipTests, isSet);
                        writeUInt64ListField(w, new long[]{0x01}, 81, skipTests, isSet);
                        writeUInt64ListField(w, new long[]{0x02L, 0x03L}, 82, skipTests, isSet);

                        // bool
                        writeBoolListField(w, new boolean[]{}, 90, skipTests, isSet);
                        writeBoolListField(w, new boolean[]{false}, 91, skipTests, isSet);
                        writeBoolListField(w, new boolean[]{false, true}, 92, skipTests, isSet);

                        // float
                        writeFloatListField(w, new float[]{}, 100, skipTests, isSet);
                        writeFloatListField(w, new float[]{0.0F}, 101, skipTests, isSet);
                        writeFloatListField(w, new float[]{0.0F, -0.0F}, 102, skipTests, isSet);

                        // double
                        writeDoubleListField(w, new double[]{}, 110, skipTests, isSet);
                        writeDoubleListField(w, new double[]{0}, 111, skipTests, isSet);
                        writeDoubleListField(w, new double[]{0.0, -0.0}, 112, skipTests, isSet);

                        // string
                        writeStringListField(w, new String[]{}, 120, skipTests, isSet);
                        writeStringListField(w, new String[]{""}, 121, skipTests, isSet);
                        writeStringListField(w, new String[]{"", "test"}, 122, skipTests, isSet);

                        // wstring
                        writeWStringListField(w, new String[]{}, 130, skipTests, isSet);
                        writeWStringListField(w, new String[]{""}, 131, skipTests, isSet);
                        writeWStringListField(w, new String[]{"", "test"}, 132, skipTests, isSet);

                        // struct list: empty
                        writeListFieldBegin(w, 1000, 0, BondDataType.BT_STRUCT, skipTests, isSet);
                        writeListFieldEnd(w, skipTests, isSet);

                        // struct list: 1 element
                        writeListFieldBegin(w, 1001, 1, BondDataType.BT_STRUCT, skipTests, isSet);
                        if (skipTests != null) {
                            skipTests.markValueStart();
                        }
                        writeStructBegin(w, skipTests);
                        writeInt8Field(w, (byte) 0xFE, 0, skipTests);
                        writeStructEnd(w, skipTests);
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_STRUCT);
                        }
                        writeListFieldEnd(w, skipTests, isSet);

                        // struct list: 3 elements
                        writeListFieldBegin(w, 1002, 3, BondDataType.BT_STRUCT, skipTests, isSet);
                        if (skipTests != null) {
                            skipTests.markValueStart();
                        }
                        writeStructBegin(w, skipTests);
                        writeInt8Field(w, (byte) 0xEF, 0, skipTests);
                        writeStructEnd(w, skipTests);
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_STRUCT);
                            skipTests.markValueStart();
                        }
                        writeStructBegin(w, skipTests);
                        writeInt8Field(w, (byte) 0xA0, 1, skipTests);
                        writeInt8Field(w, (byte) 0xA1, 2, skipTests);
                        writeStructEnd(w, skipTests);
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_STRUCT);
                            skipTests.markValueStart();
                        }
                        writeStructBegin(w, skipTests);
                        writeInt8ListField(w, new byte[]{(byte) 0xFA, (byte) 0xFB, (byte) 0xFC}, 3, skipTests, isSet);
                        writeStructEnd(w, skipTests);
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_STRUCT);
                        }
                        writeListFieldEnd(w, skipTests, isSet);

                        // list list: empty
                        writeListFieldBegin(w, 2000, 0, BondDataType.BT_LIST, skipTests, isSet);
                        writeListFieldEnd(w, skipTests, isSet);

                        // list list: 1 element
                        writeListFieldBegin(w, 2001, 1, BondDataType.BT_LIST, skipTests, isSet);
                        if (skipTests != null) {
                            skipTests.markValueStart();
                        }
                        w.writeContainerBegin(3, BondDataType.BT_UINT8);
                        w.writeBytes(new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C});
                        w.writeContainerEnd();
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_LIST);
                        }
                        writeListFieldEnd(w, skipTests, isSet);

                        // set list: empty
                        writeListFieldBegin(w, 3000, 0, BondDataType.BT_SET, skipTests, isSet);
                        writeListFieldEnd(w, skipTests, isSet);

                        // set list: 1 element
                        writeListFieldBegin(w, 3001, 1, BondDataType.BT_SET, skipTests, isSet);
                        if (skipTests != null) {
                            skipTests.markValueStart();
                        }
                        w.writeContainerBegin(3, BondDataType.BT_UINT8);
                        w.writeBytes(new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C});
                        w.writeContainerEnd();
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_SET);
                        }
                        writeListFieldEnd(w, skipTests, isSet);

                        // map list: empty
                        writeListFieldBegin(w, 4000, 0, BondDataType.BT_MAP, skipTests, isSet);
                        writeListFieldEnd(w, skipTests, isSet);

                        // map list: 1 element
                        writeListFieldBegin(w, 4001, 1, BondDataType.BT_MAP, skipTests, isSet);
                        if (skipTests != null) {
                            skipTests.markValueStart();
                        }
                        w.writeContainerBegin(2, BondDataType.BT_BOOL, BondDataType.BT_UINT8);
                        w.writeBool(false);
                        w.writeUInt8((byte) 0xF0);
                        w.writeBool(true);
                        w.writeUInt8((byte) 0xF1);
                        w.writeContainerEnd();
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_MAP);
                        }
                        writeListFieldEnd(w, skipTests, isSet);

                        // map list: 2 elements
                        writeListFieldBegin(w, 4002, 2, BondDataType.BT_MAP, skipTests, isSet);
                        if (skipTests != null) {
                            skipTests.markValueStart();
                        }
                        w.writeContainerBegin(2, BondDataType.BT_BOOL, BondDataType.BT_STRING);
                        w.writeBool(true);
                        w.writeString("true");
                        w.writeBool(false);
                        w.writeString("false");
                        w.writeContainerEnd();
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_MAP);
                            skipTests.markValueStart();
                        }
                        w.writeContainerBegin(1, BondDataType.BT_INT8, BondDataType.BT_STRUCT);
                        w.writeInt8((byte) 0);
                        writeStructBegin(w, skipTests);
                        writeInt16Field(w, (short) 0xABBA, 0, skipTests);
                        writeStructEnd(w, skipTests);
                        w.writeContainerEnd();
                        if (skipTests != null) {
                            skipTests.recordValue(BondDataType.BT_MAP);
                        }
                        writeListFieldEnd(w, skipTests, isSet);

                        // blob (list of uint8)
                        writeBlobField(w, new byte[]{}, 10000, skipTests, isSet);
                        writeBlobField(w, new byte[]{(byte) 0x01}, 10001, skipTests, isSet);
                        writeBlobField(w, new byte[]{(byte) 0x02, (byte) 0x03}, 10002, skipTests, isSet);

                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        final boolean isSet = false;
                        final TaggedProtocolReader.ReadContainerResult readContainerResult =
                                new TaggedProtocolReader.ReadContainerResult();

                        readStructBeginAndVerify(r);

                        // int8
                        readInt8ListFieldAndVerify(r, new byte[]{}, 10, isSet);
                        readInt8ListFieldAndVerify(r, new byte[]{(byte) 0x01}, 11, isSet);
                        readInt8ListFieldAndVerify(r, new byte[]{(byte) 0x02, (byte) 0x03}, 12, isSet);

                        // int16
                        readInt16ListFieldAndVerify(r, new short[]{}, 20, isSet);
                        readInt16ListFieldAndVerify(r, new short[]{(short) 0x01}, 21, isSet);
                        readInt16ListFieldAndVerify(r, new short[]{(short) 0x02, (short) 0x03}, 22, isSet);

                        // int32
                        readInt32ListFieldAndVerify(r, new int[]{}, 30, isSet);
                        readInt32ListFieldAndVerify(r, new int[]{0x01}, 31, isSet);
                        readInt32ListFieldAndVerify(r, new int[]{0x02, 0x03}, 32, isSet);

                        // int64
                        readInt64ListFieldAndVerify(r, new long[]{}, 40, isSet);
                        readInt64ListFieldAndVerify(r, new long[]{0x01L}, 41, isSet);
                        readInt64ListFieldAndVerify(r, new long[]{0x02L, 0x03L}, 42, isSet);

                        // uint8
                        readUInt8ListFieldAndVerify(r, new byte[]{}, 50, isSet);
                        readUInt8ListFieldAndVerify(r, new byte[]{(byte) 0x01}, 51, isSet);
                        readUInt8ListFieldAndVerify(r, new byte[]{(byte) 0x02, (byte) 0x03}, 52, isSet);

                        // uint16
                        readUInt16ListFieldAndVerify(r, new short[]{}, 60, isSet);
                        readUInt16ListFieldAndVerify(r, new short[]{(short) 0x01}, 61, isSet);
                        readUInt16ListFieldAndVerify(r, new short[]{(short) 0x02, (short) 0x03}, 62, isSet);

                        // uint32
                        readUInt32ListFieldAndVerify(r, new int[]{}, 70, isSet);
                        readUInt32ListFieldAndVerify(r, new int[]{0x01}, 71, isSet);
                        readUInt32ListFieldAndVerify(r, new int[]{0x02, 0x03}, 72, isSet);

                        // uint64
                        readUInt64ListFieldAndVerify(r, new long[]{}, 80, isSet);
                        readUInt64ListFieldAndVerify(r, new long[]{0x01L}, 81, isSet);
                        readUInt64ListFieldAndVerify(r, new long[]{0x02L, 0x03L}, 82, isSet);

                        // bool
                        readBoolListFieldAndVerify(r, new boolean[]{}, 90, isSet);
                        readBoolListFieldAndVerify(r, new boolean[]{false}, 91, isSet);
                        readBoolListFieldAndVerify(r, new boolean[]{false, true}, 92, isSet);

                        // float
                        readFloatListFieldAndVerify(r, new float[]{}, 100, isSet);
                        readFloatListFieldAndVerify(r, new float[]{0.0F}, 101, isSet);
                        readFloatListFieldAndVerify(r, new float[]{0.0F, -0.0F}, 102, isSet);

                        // double
                        readDoubleListFieldAndVerify(r, new double[]{}, 110, isSet);
                        readDoubleListFieldAndVerify(r, new double[]{0.0}, 111, isSet);
                        readDoubleListFieldAndVerify(r, new double[]{0.0, -0.0}, 112, isSet);

                        // string
                        readStringListFieldAndVerify(r, new String[]{}, 120, isSet);
                        readStringListFieldAndVerify(r, new String[]{""}, 121, isSet);
                        readStringListFieldAndVerify(r, new String[]{"", "test"}, 122, isSet);

                        // wstring
                        readWStringListFieldAndVerify(r, new String[]{}, 130, isSet);
                        readWStringListFieldAndVerify(r, new String[]{""}, 131, isSet);
                        readWStringListFieldAndVerify(r, new String[]{"", "test"}, 132, isSet);

                        // struct list: empty
                        readListFieldAndVerifyBegin(r, 1000, 0, BondDataType.BT_STRUCT, isSet);
                        readListFieldAndVerifyEnd(r);

                        // struct list: 1 element
                        readListFieldAndVerifyBegin(r, 1001, 1, BondDataType.BT_STRUCT, isSet);
                        readStructBeginAndVerify(r);
                        readInt8FieldAndVerify(r, (byte) 0xFE, 0);
                        readFieldAndVerifyStructEnd(r);
                        readListFieldAndVerifyEnd(r);

                        // struct list: 3 elements
                        readListFieldAndVerifyBegin(r, 1002, 3, BondDataType.BT_STRUCT, isSet);
                        readStructBeginAndVerify(r);
                        readInt8FieldAndVerify(r, (byte) 0xEF, 0);
                        readFieldAndVerifyStructEnd(r);
                        readStructBeginAndVerify(r);
                        readInt8FieldAndVerify(r, (byte) 0xA0, 1);
                        readInt8FieldAndVerify(r, (byte) 0xA1, 2);
                        readFieldAndVerifyStructEnd(r);
                        readStructBeginAndVerify(r);
                        readInt8ListFieldAndVerify(r, new byte[]{(byte) 0xFA, (byte) 0xFB, (byte) 0xFC}, 3, isSet);
                        readFieldAndVerifyStructEnd(r);
                        readListFieldAndVerifyEnd(r);

                        // list list: empty
                        readListFieldAndVerifyBegin(r, 2000, 0, BondDataType.BT_LIST, isSet);
                        readListFieldAndVerifyEnd(r);

                        // list list: 1 element
                        readListFieldAndVerifyBegin(r, 2001, 1, BondDataType.BT_LIST, isSet);
                        r.readListBegin(readContainerResult);
                        assertEquals("Count must match", 3, readContainerResult.count);
                        assertEquals("Element type must match", BondDataType.BT_UINT8, readContainerResult.elementType);
                        assertNull("Key type must be null", readContainerResult.keyType);
                        assertArrayEquals("Value byte array must match",
                                new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C}, r.readBytes(3));
                        r.readContainerEnd();
                        readListFieldAndVerifyEnd(r);

                        // set list: empty
                        readListFieldAndVerifyBegin(r, 3000, 0, BondDataType.BT_SET, isSet);
                        readListFieldAndVerifyEnd(r);

                        // set list: 1 element
                        readListFieldAndVerifyBegin(r, 3001, 1, BondDataType.BT_SET, isSet);
                        r.readListBegin(readContainerResult);
                        assertEquals("Count must match", 3, readContainerResult.count);
                        assertEquals("Element type must match", BondDataType.BT_UINT8, readContainerResult.elementType);
                        assertNull("Key type must be null", readContainerResult.keyType);
                        assertArrayEquals("Value byte array must match",
                                new byte[]{(byte) 0x0A, (byte) 0x0B, (byte) 0x0C}, r.readBytes(3));
                        r.readContainerEnd();
                        readListFieldAndVerifyEnd(r);

                        // map list: empty
                        readListFieldAndVerifyBegin(r, 4000, 0, BondDataType.BT_MAP, isSet);
                        readListFieldAndVerifyEnd(r);

                        // map list: 1 element
                        readListFieldAndVerifyBegin(r, 4001, 1, BondDataType.BT_MAP, isSet);
                        r.readMapBegin(readContainerResult);
                        assertEquals("Count must match", 2, readContainerResult.count);
                        assertEquals("Element type must match", BondDataType.BT_UINT8, readContainerResult.elementType);
                        assertEquals("Key type must match", BondDataType.BT_BOOL, readContainerResult.keyType);
                        assertEquals(false, r.readBool());
                        assertEquals((byte) 0xF0, r.readUInt8());
                        assertEquals(true, r.readBool());
                        assertEquals((byte) 0xF1, r.readUInt8());
                        r.readContainerEnd();
                        readListFieldAndVerifyEnd(r);

                        // map list: 2 elements
                        readListFieldAndVerifyBegin(r, 4002, 2, BondDataType.BT_MAP, isSet);
                        r.readMapBegin(readContainerResult);
                        assertEquals("Count must match", 2, readContainerResult.count);
                        assertEquals("Element type must match", BondDataType.BT_STRING, readContainerResult.elementType);
                        assertEquals("Key type must match", BondDataType.BT_BOOL, readContainerResult.keyType);
                        assertEquals(true, r.readBool());
                        assertEquals("true", r.readString());
                        assertEquals(false, r.readBool());
                        assertEquals("false", r.readString());
                        r.readContainerEnd();
                        r.readMapBegin(readContainerResult);
                        assertEquals("Count must match", 1, readContainerResult.count);
                        assertEquals("Element type must match", BondDataType.BT_STRUCT, readContainerResult.elementType);
                        assertEquals("Key type must match", BondDataType.BT_INT8, readContainerResult.keyType);
                        assertEquals(0, r.readInt8());
                        readStructBeginAndVerify(r);
                        readInt16FieldAndVerify(r, (short) 0xABBA, 0);
                        readFieldAndVerifyStructEnd(r);
                        r.readContainerEnd();
                        readListFieldAndVerifyEnd(r);

                        // blob (list of uint8)
                        readBlobFieldAndVerify(r, new byte[]{}, 10000, isSet);
                        readBlobFieldAndVerify(r, new byte[]{(byte) 0x01}, 10001, isSet);
                        readBlobFieldAndVerify(r, new byte[]{(byte) 0x02, (byte) 0x03}, 10002, isSet);

                        readFieldAndVerifyStructEnd(r);
                    }
                });

        public static final ProtocolTestCase LargeFieldStruct = new ProtocolTestCase(
                new TaggedBinaryTestHelper.InvokeWriter() {
                    @Override
                    public void write(ProtocolWriter w, SkipInfoList skipTests) throws IOException {

                        writeStructBegin(w, skipTests);

                        writeStringField(w, LargeSequences.LargeString1, 1, skipTests);
                        writeStringField(w, LargeSequences.LargeString2, 2, skipTests);
                        writeStringField(w, LargeSequences.LargeString3, 3, skipTests);
                        writeStringField(w, LargeSequences.LargeString4, 4, skipTests);
                        writeStringField(w, LargeSequences.LargeString5, 5, skipTests);

                        writeUInt8ListField(w, LargeSequences.LargeUInt8Array1, 101, skipTests, true);
                        writeUInt8ListField(w, LargeSequences.LargeUInt8Array2, 102, skipTests, true);
                        writeUInt8ListField(w, LargeSequences.LargeUInt8Array3, 103, skipTests, true);
                        writeUInt8ListField(w, LargeSequences.LargeUInt8Array4, 104, skipTests, true);
                        writeUInt8ListField(w, LargeSequences.LargeUInt8Array5, 105, skipTests, true);

                        writeStructEnd(w, skipTests);
                    }
                },
                new TaggedBinaryTestHelper.InvokeReader() {
                    @Override
                    public void readAndVerify(TaggedProtocolReader r) throws IOException {
                        readStructBeginAndVerify(r);

                        readStringFieldAndVerify(r, LargeSequences.LargeString1, 1);
                        readStringFieldAndVerify(r, LargeSequences.LargeString2, 2);
                        readStringFieldAndVerify(r, LargeSequences.LargeString3, 3);
                        readStringFieldAndVerify(r, LargeSequences.LargeString4, 4);
                        readStringFieldAndVerify(r, LargeSequences.LargeString5, 5);

                        readUInt8ListFieldAndVerify(r, LargeSequences.LargeUInt8Array1, 101, true);
                        readUInt8ListFieldAndVerify(r, LargeSequences.LargeUInt8Array2, 102, true);
                        readUInt8ListFieldAndVerify(r, LargeSequences.LargeUInt8Array3, 103, true);
                        readUInt8ListFieldAndVerify(r, LargeSequences.LargeUInt8Array4, 104, true);
                        readUInt8ListFieldAndVerify(r, LargeSequences.LargeUInt8Array5, 105, true);

                        readFieldAndVerifyStructEnd(r);
                    }
                });
    }

    private static void writeStructBegin(
            ProtocolWriter w, SkipInfoList skipTests) throws IOException {
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeStructBegin(null);
    }

    private static void writeStructEnd(
            ProtocolWriter w, SkipInfoList skipTests) throws IOException {
        w.writeStructEnd();
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_STRUCT);
        }
    }

    private static void writeBaseBegin(
            ProtocolWriter w, SkipInfoList skipTests) throws IOException {
        w.writeBaseBegin(null);
    }

    private static void writeBaseEnd(
            ProtocolWriter w, SkipInfoList skipTests) throws IOException {
        w.writeBaseEnd();
    }

    private static void writeInt8Field(
            ProtocolWriter w, byte value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_INT8, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeInt8(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_INT8);
        }
        w.writeFieldEnd();
    }

    private static void writeInt16Field(
            ProtocolWriter w, short value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_INT16, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeInt16(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_INT16);
        }
        w.writeFieldEnd();
    }

    private static void writeInt32Field(
            ProtocolWriter w, int value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_INT32, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeInt32(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_INT32);
        }
        w.writeFieldEnd();
    }

    private static void writeInt64Field(
            ProtocolWriter w, long value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_INT64, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeInt64(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_INT64);
        }
        w.writeFieldEnd();
    }

    private static void writeUInt8Field(
            ProtocolWriter w, byte value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_UINT8, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeUInt8(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_UINT8);
        }
        w.writeFieldEnd();
    }

    private static void writeUInt16Field(
            ProtocolWriter w, short value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_UINT16, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeUInt16(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_UINT16);
        }
        w.writeFieldEnd();
    }

    private static void writeUInt32Field(
            ProtocolWriter w, int value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_UINT32, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeUInt32(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_UINT32);
        }
        w.writeFieldEnd();
    }

    private static void writeUInt64Field(
            ProtocolWriter w, long value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_UINT64, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeUInt64(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_UINT64);
        }
        w.writeFieldEnd();
    }

    private static void writeBoolField(
            ProtocolWriter w, boolean value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_BOOL, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeBool(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_BOOL);
        }
        w.writeFieldEnd();
    }

    private static void writeFloatField(
            ProtocolWriter w, float value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_FLOAT, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeFloat(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_FLOAT);
        }
        w.writeFieldEnd();
    }

    private static void writeDoubleField(
            ProtocolWriter w, double value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_DOUBLE, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeDouble(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_DOUBLE);
        }
        w.writeFieldEnd();
    }

    private static void writeStringField(
            ProtocolWriter w, String value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_STRING, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeString(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_STRING);
        }
        w.writeFieldEnd();
    }

    private static void writeWStringField(
            ProtocolWriter w, String value, int id, SkipInfoList skipTests) throws IOException {
        w.writeFieldBegin(BondDataType.BT_WSTRING, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeWString(value);
        if (skipTests != null) {
            skipTests.recordValue(BondDataType.BT_WSTRING);
        }
        w.writeFieldEnd();
    }

    private static void writeInt8ListField(
            ProtocolWriter w, byte[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_INT8, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeInt8(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_INT8);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeInt16ListField(
            ProtocolWriter w, short[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_INT16, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeInt16(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_INT16);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeInt32ListField(
            ProtocolWriter w, int[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_INT32, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeInt32(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_INT32);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeInt64ListField(
            ProtocolWriter w, long[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_INT64, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeInt64(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_INT64);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeUInt8ListField(
            ProtocolWriter w, byte[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_UINT8, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeUInt8(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_UINT8);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeUInt16ListField(
            ProtocolWriter w, short[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_UINT16, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeUInt16(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_UINT16);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeUInt32ListField(
            ProtocolWriter w, int[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_UINT32, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeUInt32(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_UINT32);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeUInt64ListField(
            ProtocolWriter w, long[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_UINT64, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeUInt64(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_UINT64);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeBoolListField(
            ProtocolWriter w, boolean[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_BOOL, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeBool(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_BOOL);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeFloatListField(
            ProtocolWriter w, float[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_FLOAT, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeFloat(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_FLOAT);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeDoubleListField(
            ProtocolWriter w, double[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_DOUBLE, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeDouble(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_DOUBLE);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeStringListField(
            ProtocolWriter w, String[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_STRING, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeString(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_STRING);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeWStringListField(
            ProtocolWriter w, String[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_WSTRING, skipTests, isSet);
        for (int i = 0; i < value.length; ++i) {
            if (skipTests != null) {
                skipTests.markValueStart();
            }
            w.writeWString(value[i]);
            if (skipTests != null) {
                skipTests.recordValue(BondDataType.BT_WSTRING);
            }
        }
        writeListFieldEnd(w, skipTests, isSet);
    }

    // A variant of writeUInt8ListField that write entire byte array rather than individual bytes
    private static void writeBlobField(
            ProtocolWriter w, byte[] value, int id, SkipInfoList skipTests, boolean isSet) throws IOException {
        writeListFieldBegin(w, id, value.length, BondDataType.BT_UINT8, skipTests, isSet);
        w.writeBytes(value);
        writeListFieldEnd(w, skipTests, isSet);
    }

    private static void writeListFieldBegin(
            ProtocolWriter w, int id, int count, BondDataType elementType, SkipInfoList skipTests, boolean isSet)
            throws IOException {
        BondDataType listType = isSet ? BondDataType.BT_SET : BondDataType.BT_LIST;
        w.writeFieldBegin(listType, id, null);
        if (skipTests != null) {
            skipTests.markValueStart();
        }
        w.writeContainerBegin(count, elementType);
    }

    private static void writeListFieldEnd(
            ProtocolWriter w, SkipInfoList skipTests, boolean isSet) throws IOException {
        w.writeContainerEnd();
        BondDataType listType = isSet ? BondDataType.BT_SET : BondDataType.BT_LIST;
        if (skipTests != null) {
            skipTests.recordValue(listType);
        }
        w.writeFieldEnd();
    }

    private static void readStructBeginAndVerify(
            TaggedProtocolReader r) throws IOException {
        r.readStructBegin();
    }

    private static void readBaseBeginAndVerify(
            TaggedProtocolReader r) throws IOException {
        r.readBaseBegin();
    }

    private static void readFieldAndVerifyStructEnd(
            TaggedProtocolReader r) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must be BT_STOP", BondDataType.BT_STOP, readFieldResult.type);
        assertEquals("ID must be 0", 0, readFieldResult.id);
        r.readStructEnd();
    }

    private static void readFieldAndVerifyBaseEnd(
            TaggedProtocolReader r) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must be BT_STOP_BASE", BondDataType.BT_STOP_BASE, readFieldResult.type);
        assertEquals("ID must be 0", 0, readFieldResult.id);
        r.readBaseEnd();
    }

    private static void readInt8FieldAndVerify(
            TaggedProtocolReader r, byte value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_INT8, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readInt8());
        r.readFieldEnd();
    }

    private static void readInt16FieldAndVerify(
            TaggedProtocolReader r, short value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_INT16, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readInt16());
        r.readFieldEnd();
    }

    private static void readInt32FieldAndVerify(
            TaggedProtocolReader r, int value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_INT32, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readInt32());
        r.readFieldEnd();
    }

    private static void readInt64FieldAndVerify(
            TaggedProtocolReader r, long value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_INT64, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readInt64());
        r.readFieldEnd();
    }

    private static void readUInt8FieldAndVerify(
            TaggedProtocolReader r, byte value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_UINT8, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readUInt8());
        r.readFieldEnd();
    }

    private static void readUInt16FieldAndVerify(
            TaggedProtocolReader r, short value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_UINT16, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readUInt16());
        r.readFieldEnd();
    }

    private static void readUInt32FieldAndVerify(
            TaggedProtocolReader r, int value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_UINT32, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readUInt32());
        r.readFieldEnd();
    }

    private static void readUInt64FieldAndVerify(
            TaggedProtocolReader r, long value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_UINT64, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readUInt64());
        r.readFieldEnd();
    }

    private static void readBoolFieldAndVerify(
            TaggedProtocolReader r, boolean value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_BOOL, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readBool());
        r.readFieldEnd();
    }

    private static void readFloatFieldAndVerify(
            TaggedProtocolReader r, float value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_FLOAT, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        float actual = r.readFloat();
        assertEquals("Value must match", value, actual, 0F);
        assertEquals("Raw bits must match", Float.floatToRawIntBits(value), Float.floatToRawIntBits(actual));
        r.readFieldEnd();
    }

    private static void readDoubleFieldAndVerify(
            TaggedProtocolReader r, double value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_DOUBLE, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        double actual = r.readDouble();
        assertEquals("Value must match", value, actual, 0F);
        assertEquals("Raw bits must match", Double.doubleToRawLongBits(value), Double.doubleToRawLongBits(actual));
        r.readFieldEnd();
    }

    private static void readStringFieldAndVerify(
            TaggedProtocolReader r, String value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_STRING, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readString());
        r.readFieldEnd();
    }

    private static void readWStringFieldAndVerify(
            TaggedProtocolReader r, String value, int id) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        assertEquals("Type must match", BondDataType.BT_WSTRING, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        assertEquals("Value must match", value, r.readWString());
        r.readFieldEnd();
    }

    private static void readInt8ListFieldAndVerify(
            TaggedProtocolReader r, byte[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_INT8, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt8());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readInt16ListFieldAndVerify(
            TaggedProtocolReader r, short[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_INT16, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt16());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readInt32ListFieldAndVerify(
            TaggedProtocolReader r, int[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_INT32, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt32());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readInt64ListFieldAndVerify(
            TaggedProtocolReader r, long[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_INT64, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readInt64());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readUInt8ListFieldAndVerify(
            TaggedProtocolReader r, byte[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_UINT8, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt8());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readUInt16ListFieldAndVerify(
            TaggedProtocolReader r, short[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_UINT16, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt16());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readUInt32ListFieldAndVerify(
            TaggedProtocolReader r, int[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_UINT32, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt32());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readUInt64ListFieldAndVerify(
            TaggedProtocolReader r, long[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_UINT64, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readUInt64());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readBoolListFieldAndVerify(
            TaggedProtocolReader r, boolean[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_BOOL, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readBool());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readFloatListFieldAndVerify(
            TaggedProtocolReader r, float[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_FLOAT, isSet);
        for (int i = 0; i < value.length; ++i) {
            float element = r.readFloat();
            assertEquals("Element value must match", value[i], element, 0F);
            assertEquals("Element value bits must match",
                    Float.floatToRawIntBits(value[i]), Float.floatToRawIntBits(element));
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readDoubleListFieldAndVerify(
            TaggedProtocolReader r, double[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_DOUBLE, isSet);
        for (int i = 0; i < value.length; ++i) {
            double element = r.readDouble();
            assertEquals("Element value must match", value[i], element, 0);
            assertEquals("Element value bits must match",
                    Double.doubleToRawLongBits(value[i]), Double.doubleToRawLongBits(element));
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readStringListFieldAndVerify(
            TaggedProtocolReader r, String[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_STRING, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readString());
        }
        readListFieldAndVerifyEnd(r);
    }

    private static void readWStringListFieldAndVerify(
            TaggedProtocolReader r, String[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_WSTRING, isSet);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], r.readWString());
        }
        readListFieldAndVerifyEnd(r);
    }

    // A variant of readUInt8ListFieldAndVerify that reads entire byte array rather than individual bytes
    private static void readBlobFieldAndVerify(
            TaggedProtocolReader r, byte[] value, int id, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadContainerResult readContainerResult =
                readListFieldAndVerifyBegin(r, id, value.length, BondDataType.BT_UINT8, isSet);
        byte[] actualValue = r.readBytes(value.length);
        for (int i = 0; i < value.length; ++i) {
            assertEquals("Element value must match", value[i], actualValue[i]);
        }
        readListFieldAndVerifyEnd(r);
    }

    private static TaggedProtocolReader.ReadContainerResult readListFieldAndVerifyBegin(
            TaggedProtocolReader r, int id, int count, BondDataType elementType, boolean isSet) throws IOException {
        TaggedProtocolReader.ReadFieldResult readFieldResult = new TaggedProtocolReader.ReadFieldResult();
        r.readFieldBegin(readFieldResult);
        BondDataType expectedType = isSet ? BondDataType.BT_SET : BondDataType.BT_LIST;
        assertEquals("Type must match", expectedType, readFieldResult.type);
        assertEquals("ID must match", id, readFieldResult.id);
        TaggedProtocolReader.ReadContainerResult readContainerResult = new TaggedProtocolReader.ReadContainerResult();
        r.readListBegin(readContainerResult);
        assertEquals("Element count must match", count, readContainerResult.count);
        assertEquals("Element type must match", elementType, readContainerResult.elementType);
        assertNull("Key type must be null", readContainerResult.keyType);
        return readContainerResult;
    }

    private static void readListFieldAndVerifyEnd(
            TaggedProtocolReader r) throws IOException {
        r.readContainerEnd();
        r.readFieldEnd();
    }
}
