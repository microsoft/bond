// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
 * Contains helper methods for working with variable-length unsigned integer representation.
 * Since Java doesn't have unsigned integral types, they are represented by signed integral
 * types of the same width (e.g. short for unsigned 16-bit integer) where the sign bit is
 * interpreted as the most significant bit of the unsigned value. Please note that code
 * using unsigned integers in numeric expressions is responsible for implementing overflow
 * protection such as type promotion (e.g. use int to represent unsigned 16-bit value).
 */
final class VarUIntHelper {

    // prevent instantiation
    private VarUIntHelper() {
    }

    /**
     * Encodes an unsigned uint16 value (represented as a signed int16 value) into a stream.
     * The sign bit of the value is re-interpreted as the high order bit for encoding purposes.
     *
     * @param unsignedValue the value to encode, interpreted as unsigned
     * @param outputStream  the output stream
     * @return the number of bytes written to the stream (1-3)
     */
    static int encodeVarUInt16(short unsignedValue, OutputStream outputStream) throws IOException {
        int length = 1;

        // promote to non-negative int to avoid casting between short and int when using
        // bit shift operators, which implicitly perform type promotion from short to int
        int unsignedValueAsInt = UnsignedHelper.asUnsignedInt(unsignedValue);

        // byte 0
        if (unsignedValueAsInt >= 0x80) {
            outputStream.write((byte) (unsignedValueAsInt | 0x80));
            unsignedValueAsInt >>>= 7;
            length = 2;

            // byte 1
            if (unsignedValueAsInt >= 0x80) {
                outputStream.write((byte) (unsignedValueAsInt | 0x80));
                unsignedValueAsInt >>>= 7;
                length = 3;
            }
        }

        // last byte
        outputStream.write((byte) unsignedValueAsInt);

        return length;
    }

    /**
     * Encodes an unsigned uint32 value (represented as a signed int32 value) into a stream.
     * The sign bit of the value is re-interpreted as the high order bit for encoding purposes.
     *
     * @param unsignedValue the value to encode, interpreted as unsigned
     * @param outputStream  the output stream
     * @return the number of bytes written to the stream (1-5)
     */
    static int encodeVarUInt32(int unsignedValue, OutputStream outputStream) throws IOException {
        int length = 1;

        // byte 0 (needs a special case to test for negative)
        if (unsignedValue >= 0x80 || unsignedValue < 0) {
            outputStream.write((byte) (unsignedValue | 0x80));
            unsignedValue >>>= 7;
            length = 2;

            // byte 1
            if (unsignedValue >= 0x80) {
                outputStream.write((byte) (unsignedValue | 0x80));
                unsignedValue >>>= 7;
                length = 3;

                // byte 2
                if (unsignedValue >= 0x80) {
                    outputStream.write((byte) (unsignedValue | 0x80));
                    unsignedValue >>>= 7;
                    length = 4;

                    // byte 3
                    if (unsignedValue >= 0x80) {
                        outputStream.write((byte) (unsignedValue | 0x80));
                        unsignedValue >>>= 7;
                        length = 5;
                    }
                }
            }
        }

        // last byte
        outputStream.write((byte) unsignedValue);

        return length;
    }

    /**
     * Encodes an unsigned uint64 value (represented as signed int64 value) into a stream.
     * The sign bit of the value is re-interpreted as the high order bit for encoding purposes.
     *
     * @param unsignedValue the value to encode, interpreted as unsigned
     * @param outputStream  the output stream
     * @return the number of bytes written to the stream (1-10)
     */
    static int encodeVarUInt64(long unsignedValue, OutputStream outputStream) throws IOException {
        int length = 1;

        // byte 0 (needs a special case to test for negative)
        if (unsignedValue >= 0x80 || unsignedValue < 0) {
            outputStream.write((byte) (unsignedValue | 0x80));
            unsignedValue >>>= 7;
            length = 2;

            // byte 1
            if (unsignedValue >= 0x80) {
                outputStream.write((byte) (unsignedValue | 0x80));
                unsignedValue >>>= 7;
                length = 3;

                // byte 2
                if (unsignedValue >= 0x80) {
                    outputStream.write((byte) (unsignedValue | 0x80));
                    unsignedValue >>>= 7;
                    length = 4;

                    // byte 3
                    if (unsignedValue >= 0x80) {
                        outputStream.write((byte) (unsignedValue | 0x80));
                        unsignedValue >>>= 7;
                        length = 5;

                        // byte 4
                        if (unsignedValue >= 0x80) {
                            outputStream.write((byte) (unsignedValue | 0x80));
                            unsignedValue >>>= 7;
                            length = 6;

                            // byte 5
                            if (unsignedValue >= 0x80) {
                                outputStream.write((byte) (unsignedValue | 0x80));
                                unsignedValue >>>= 7;
                                length = 7;

                                // byte 6
                                if (unsignedValue >= 0x80) {
                                    outputStream.write((byte) (unsignedValue | 0x80));
                                    unsignedValue >>>= 7;
                                    length = 8;

                                    // byte 7
                                    if (unsignedValue >= 0x80) {
                                        outputStream.write((byte) (unsignedValue | 0x80));
                                        unsignedValue >>>= 7;
                                        length = 9;

                                        // byte 8
                                        if (unsignedValue >= 0x80) {
                                            outputStream.write((byte) (unsignedValue | 0x80));
                                            unsignedValue >>>= 7;
                                            length = 10;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        // last byte
        outputStream.write((byte) unsignedValue);

        return length;
    }

    /**
     * Decodes a variable-length unsigned uint16 value from a stream, using signed int16 type
     * to represent the result.
     *
     * @param inputStream the input stream
     * @return the result represented by signed short type
     * @throws IOException if I/O error occurred
     */
    static short decodeVarUInt16(InputStream inputStream) throws IOException {
        // byte 0 (bits 0-6)
        int unsignedResult = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));

        if (0x80 <= unsignedResult) {
            // byte 1 (bits 7-13)
            int raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
            unsignedResult = (unsignedResult & 0x7F) | ((raw & 0x7F) << 7);
            if (0x80 <= raw) {
                // byte 2 (bits 14-15)
                raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                unsignedResult |= raw << 14;
            }
        }

        return (short) unsignedResult;
    }

    /**
     * Decodes a variable-length unsigned uint32 value from a stream, using signed int32 type
     * to represent the result.
     *
     * @param inputStream the input stream
     * @return the result represented by signed int type
     * @throws IOException if I/O error occurred
     */
    static int decodeVarUInt32(InputStream inputStream) throws IOException {
        // byte 0 (bits 0-6)
        int unsignedResult = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));

        if (0x80 <= unsignedResult) {
            // byte 1 (bits 7-13)
            int raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
            unsignedResult = (unsignedResult & 0x7F) | ((raw & 0x7F) << 7);
            if (0x80 <= raw) {
                // byte 2 (bits 14-20)
                raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                unsignedResult |= (raw & 0x7F) << 14;
                if (0x80 <= raw) {
                    // byte 3 (bits 21-27)
                    raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                    unsignedResult |= (raw & 0x7F) << 21;
                    if (0x80 <= raw) {
                        // byte 4 (bits 28-31)
                        raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                        unsignedResult |= raw << 28;
                    }
                }
            }
        }

        return unsignedResult;
    }

    /**
     * Decodes a variable-length unsigned uint64 value from a stream, using signed int64 type
     * to represent the result.
     *
     * @param inputStream the input stream
     * @return the result represented by signed long type
     * @throws IOException if I/O error occurred
     */
    static long decodeVarUInt64(InputStream inputStream) throws IOException {
        // byte 0 (bits 0-6)
        long unsignedResult = UnsignedHelper.asUnsignedLong(StreamHelper.readByte(inputStream));

        if (0x80 <= unsignedResult) {
            // byte 1 (bits 7-13)
            long raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
            unsignedResult = (unsignedResult & 0x7F) | ((raw & 0x7F) << 7);
            if (0x80 <= raw) {
                // byte 2 (bits 14-20)
                raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                unsignedResult |= (raw & 0x7F) << 14;
                if (0x80 <= raw) {
                    // byte 3 (bits 21-27)
                    raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                    unsignedResult |= (raw & 0x7F) << 21;
                    if (0x80 <= raw) {
                        // byte 4 (bits 28-34)
                        raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                        unsignedResult |= (raw & 0x7F) << 28;
                        if (0x80 <= raw) {
                            // byte 5 (bits 35-41)
                            raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                            unsignedResult |= (raw & 0x7F) << 35;
                            if (0x80 <= raw) {
                                // byte 6 (bits 42-48)
                                raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                                unsignedResult |= (raw & 0x7F) << 42;
                                if (0x80 <= raw) {
                                    // byte 7 (bits 49-55)
                                    raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                                    unsignedResult |= (raw & 0x7F) << 49;
                                    if (0x80 <= raw) {
                                        // byte 8 (bits 56-62)
                                        raw = UnsignedHelper.asUnsignedInt(StreamHelper.readByte(inputStream));
                                        unsignedResult |= raw << 56;
                                        if (0x80 <= raw) {
                                            // byte 9 (ignored, per VarInt specification)
                                            StreamHelper.readByte(inputStream);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return unsignedResult;
    }

    /**
     * Computes the number of bytes needed to encode an unsigned uint16 value (represented as a signed int16 value).
     * The sign bit of the value is re-interpreted as the high order bit for encoding purposes.
     *
     * @param unsignedValue the value to encode, interpreted as unsigned
     * @return the number of bytes representing the encoded value (1-3)
     */
    static int getVarUInt16Length(short unsignedValue) {
        if (unsignedValue < 0) {
            // negative value means the highest bit of the unsigned value is set, so using maximum bytes
            return 3;
        } else if (unsignedValue < (1 << 7)) {
            // value fits into 7 bits
            return 1;
        } else if (unsignedValue < (1 << 14)) {
            // value fits into 14 bits
            return 2;
        } else {
            // maximum bytes
            return 3;
        }
    }

    /**
     * Computes the number of bytes needed to encode an unsigned uint32 value (represented as a signed int32 value).
     * The sign bit of the value is re-interpreted as the high order bit for encoding purposes.
     *
     * @param unsignedValue the value to encode, interpreted as unsigned
     * @return the number of bytes representing the encoded value (1-5)
     */
    static int getVarUInt32Length(int unsignedValue) {
        if (unsignedValue < 0) {
            // negative value means the highest bit of the unsigned value is set, so using maximum bytes
            return 5;
        } else if (unsignedValue < (1 << 7)) {
            // value fits into 7 bits
            return 1;
        } else if (unsignedValue < (1 << 14)) {
            // value fits into 14 bits
            return 2;
        } else if (unsignedValue < (1 << 21)) {
            // value fits into 21 bits
            return 3;
        } else if (unsignedValue < (1 << 28)) {
            // value fits into 28 bits
            return 4;
        } else {
            // maximum bytes
            return 5;
        }
    }

    /**
     * Computes the number of bytes needed to encode an unsigned uint64 value (represented as a signed int64 value).
     * The sign bit of the value is re-interpreted as the high order bit for encoding purposes.
     *
     * @param unsignedValue the value to encode, interpreted as unsigned
     * @return the number of bytes representing the encoded value (1-10)
     */
    static int getVarUInt64Length(long unsignedValue) {
        if (unsignedValue < 0L) {
            // negative value means the highest bit of the unsigned value is set, so using maximum bytes
            return 10;
        } else if (unsignedValue < (1L << 7)) {
            // value fits into 7 bits
            return 1;
        } else if (unsignedValue < (1L << 14)) {
            // value fits into 14 bits
            return 2;
        } else if (unsignedValue < (1L << 21)) {
            // value fits into 21 bits
            return 3;
        } else if (unsignedValue < (1L << 28)) {
            // value fits into 28 bits
            return 4;
        } else if (unsignedValue < (1L << 35)) {
            // value fits into 35 bits
            return 5;
        } else if (unsignedValue < (1L << 42)) {
            // value fits into 42 bits
            return 6;
        } else if (unsignedValue < (1L << 49)) {
            // value fits into 49 bits
            return 7;
        } else if (unsignedValue < (1L << 56)) {
            // value fits into 56 bits
            return 8;
        } else {
            // value fits into 63 bits (i.e. non-negative)
            return 9;
        }
    }
}
