// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.EOFException;
import java.io.IOException;

import static org.junit.Assert.*;

public class BinaryStreamReaderTest {

    @Test
    public void readInt8() {
        // setup
        byte[] inputBytes = new byte[]{
                (byte) 0x5A,
                (byte) 0xE3
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            byte v = r.readInt8();
            assertEquals((byte) 0x5A, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second
        try {
            byte v = r.readInt8();
            assertEquals((byte) 0xE3, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            byte v = r.readInt8();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readInt16() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0xE35A
                (byte) 0x5A, (byte) 0xE3,

                // 0xAAC0
                (byte) 0xC0, (byte) 0xAA
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            short v = r.readInt16();
            assertEquals((short) 0xE35A, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second
        try {
            short v = r.readInt16();
            assertEquals((short) 0xAAC0, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            short v = r.readInt16();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readInt32() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0x7F32E35A
                (byte) 0x5A, (byte) 0xE3, (byte) 0x32, (byte) 0x7F,

                // 0xBB00AAC0
                (byte) 0xC0, (byte) 0xAA, (byte) 0x00, (byte) 0xBB
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            int v = r.readInt32();
            assertEquals(0x7F32E35A, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second
        try {
            int v = r.readInt32();
            assertEquals(0xBB00AAC0, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            int v = r.readInt32();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readInt64() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0x325476987F32E35A
                (byte) 0x5A, (byte) 0xE3, (byte) 0x32, (byte) 0x7F,
                (byte) 0x98, (byte) 0x76, (byte) 0x54, (byte) 0x32,

                // 0xCCFF3113BB00AAC0
                (byte) 0xC0, (byte) 0xAA, (byte) 0x00, (byte) 0xBB,
                (byte) 0x13, (byte) 0x31, (byte) 0xFF, (byte) 0xCC
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            long v = r.readInt64();
            assertEquals(0x325476987F32E35AL, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second
        try {
            long v = r.readInt64();
            assertEquals(0xCCFF3113BB00AAC0L, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            long v = r.readInt64();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readFloat() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0, bit pattern:  0x00000000
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // PI (3.14159265358979323846), bit pattern: 0x40490FDB
                (byte) 0xDB, (byte) 0x0F, (byte) 0x49, (byte) 0x40,

                // -Infinity, bit pattern:  0xFF800000
                (byte) 0x00, (byte) 0x00, (byte) 0x80, (byte) 0xFF,

                // +Infinity, bit pattern:  0x7F800000
                (byte) 0x00, (byte) 0x00, (byte) 0x80, (byte) 0x7F,

                // A NaN (canonical for some mainstream JVM implementations), bit pattern:  0x7FC00000
                // (since "canonical Java NaN" is unspecified, this is treated as any other NaN representation)
                (byte) 0x00, (byte) 0x00, (byte) 0xC0, (byte) 0x7F,

                // A NaN (non-canonical), bit pattern:  0xFFC00000
                (byte) 0x00, (byte) 0x00, (byte) 0xC0, (byte) 0xFF,

                // A NaN (non-canonical), bit pattern:  0x7F800001
                (byte) 0x01, (byte) 0x00, (byte) 0x80, (byte) 0x7F
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first (0)
        try {
            float v = r.readFloat();
            assertEquals(0F, v, 0F);
            int vb = Float.floatToRawIntBits(v);
            assertEquals(0x00000000, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second (PI)
        try {
            float v = r.readFloat();
            assertEquals((float) Math.PI, v, 0F);
            int vb = Float.floatToRawIntBits(v);
            assertEquals(0x40490FDB, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read third (-Infinity)
        try {
            float v = r.readFloat();
            assertTrue(v == Float.NEGATIVE_INFINITY);
            int vb = Float.floatToRawIntBits(v);
            assertEquals(0xFF800000, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read fourth (+Infinity)
        try {
            float v = r.readFloat();
            assertTrue(v == Float.POSITIVE_INFINITY);
            int vb = Float.floatToRawIntBits(v);
            assertEquals(0x7F800000, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read fifth (NaN, canonical)
        try {
            float v = r.readFloat();
            assertTrue(Float.isNaN(v));
            int vb = Float.floatToRawIntBits(v);
            assertEquals(0x7FC00000, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read sixth (NaN, non-canonical)
        try {
            float v = r.readFloat();
            assertTrue(Float.isNaN(v));
            int vb = Float.floatToRawIntBits(v);
            assertEquals(0xFFC00000, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read seventh (NaN, non-canonical)
        try {
            float v = r.readFloat();
            assertTrue(Float.isNaN(v));
            int vb = Float.floatToRawIntBits(v);
            assertEquals(0x7F800001, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            float v = r.readFloat();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readDouble() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0, bit pattern:  0x0000000000000000
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,

                // PI (3.14159265358979323846), bit pattern: 0x400921FB54442D18
                (byte) 0x18, (byte) 0x2D, (byte) 0x44, (byte) 0x54,
                (byte) 0xFB, (byte) 0x21, (byte) 0x09, (byte) 0x40,

                // -Infinity, bit pattern:  0xFFF0000000000000
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0xF0, (byte) 0xFF,

                // +Infinity, bit pattern:  0x7FF0000000000000
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0xF0, (byte) 0x7F,

                // A NaN (canonical for some mainstream JVM implementations), bit pattern:  0x7FF8000000000000
                // (since "canonical Java NaN" is unspecified, this is treated as any other NaN representation)
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0xF8, (byte) 0x7F,

                // A NaN (non-canonical), bit pattern:  0xFFF8000000000000
                (byte) 0x00, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0xF8, (byte) 0xFF,

                // A NaN (non-canonical), bit pattern:  0x7FF0000000000001
                (byte) 0x01, (byte) 0x00, (byte) 0x00, (byte) 0x00,
                (byte) 0x00, (byte) 0x00, (byte) 0xF0, (byte) 0x7F
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first (0)
        try {
            double v = r.readDouble();
            assertEquals(0F, v, 0F);
            long vb = Double.doubleToRawLongBits(v);
            assertEquals(0x0000000000000000L, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second (PI)
        try {
            double v = r.readDouble();
            assertEquals(Math.PI, v, 0);
            long vb = Double.doubleToRawLongBits(v);
            assertEquals(0x400921FB54442D18L, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read third (-Infinity)
        try {
            double v = r.readDouble();
            assertTrue(v == Double.NEGATIVE_INFINITY);
            long vb = Double.doubleToRawLongBits(v);
            assertEquals(0xFFF0000000000000L, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read fourth (+Infinity)
        try {
            double v = r.readDouble();
            assertTrue(v == Double.POSITIVE_INFINITY);
            long vb = Double.doubleToRawLongBits(v);
            assertEquals(0x7FF0000000000000L, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read fifth (NaN, canonical)
        try {
            double v = r.readDouble();
            assertTrue(Double.isNaN(v));
            long vb = Double.doubleToRawLongBits(v);
            assertEquals(0x7FF8000000000000L, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read sixth (NaN, non-canonical)
        try {
            double v = r.readDouble();
            assertTrue(Double.isNaN(v));
            long vb = Double.doubleToRawLongBits(v);
            assertEquals(0xFFF8000000000000L, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read seventh (NaN, non-canonical)
        try {
            double v = r.readDouble();
            assertTrue(Double.isNaN(v));
            long vb = Double.doubleToRawLongBits(v);
            assertEquals(0x7FF0000000000001L, vb);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            double v = r.readDouble();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readBool() {
        // setup
        byte[] inputBytes = new byte[]{
                // false (anything else is true)
                (byte) 0x00,

                // true (LS bit set)
                (byte) 0x01,

                // true (MS bit set)
                (byte) 0x80,

                // true (all bits set)
                (byte) 0xFF,
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first (false)
        try {
            boolean v = r.readBool();
            assertFalse(v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second (true, LS bit set)
        try {
            boolean v = r.readBool();
            assertTrue(v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read third (true, MS bit set)
        try {
            boolean v = r.readBool();
            assertTrue(v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read fourth (true, all bits set)
        try {
            boolean v = r.readBool();
            assertTrue(v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            boolean v = r.readBool();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readBytes() {
        // setup
        byte[] inputBytes = new byte[]{
                // 7 bytes
                (byte) 0x01, (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x05, (byte) 0x08, (byte) 0x013,

                // 4 bytes
                (byte) 0x55, (byte) 0x00, (byte) 0xFF, (byte) 0xAA
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            int length = 7;
            int offset = 0;
            byte[] v = r.readBytes(length);
            for (int i = 0; i < length; ++i) {
                assertEquals(inputBytes[i + offset], v[i]);
            }
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second
        try {
            int length = 4;
            int offset = 7;
            byte[] v = r.readBytes(length);
            for (int i = 0; i < length; ++i) {
                assertEquals(inputBytes[i + offset], v[i]);
            }
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            int length = 1;
            r.readBytes(length);
            fail("Operation can't succeed: " + length);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readVarUInt16() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0x005A
                (byte) 0x5A,

                // 0x075A
                (byte) 0xDA, (byte) 0x0E
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            short v = r.readVarUInt16();
            assertEquals((short) 0x005A, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second
        try {
            short v = r.readVarUInt16();
            assertEquals((short) 0x075A, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            short v = r.readVarUInt16();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readVarUInt32() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0x0000005A
                (byte) 0x5A,

                // 0x8FFFFF5A
                (byte) 0xDA, (byte) 0xFE, (byte) 0xFF, (byte) 0xFF, (byte) 0x08
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            int v = r.readVarUInt32();
            assertEquals(0x005A, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second
        try {
            int v = r.readVarUInt32();
            assertEquals(0x8FFFFF5A, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            int v = r.readVarUInt32();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void readVarUInt64() {
        // setup
        byte[] inputBytes = new byte[]{
                // 0x000000000000005A
                (byte) 0x5A,

                // 0x8FFFFFFFFFFFFF5A (canonical, last byte is 0x01)
                // (the 10th byte in the VarUInt scheme for uint64 is irrelevant, set to 0x01 in canonical form)
                (byte) 0xDA, (byte) 0xFE, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x8F, (byte) 0x01,

                // 0x8FFFFFFFFFFFFF5A (non-canonical, last byte is 0x00)
                (byte) 0xDA, (byte) 0xFE, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x8F, (byte) 0x00,

                // 0x8FFFFFFFFFFFFF5A (non-canonical, last byte is anything)
                (byte) 0xDA, (byte) 0xFE, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x8F, (byte) 0xAA
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // read first
        try {
            long v = r.readVarUInt64();
            assertEquals(0x000000000000005AL, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read second (canonical representation of 0x8FFFFFFFFFFFFF5A)
        try {
            long v = r.readVarUInt64();
            assertEquals(0x8FFFFFFFFFFFFF5AL, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read third (non-canonical representation of 0x8FFFFFFFFFFFFF5A)
        try {
            long v = r.readVarUInt64();
            assertEquals(0x8FFFFFFFFFFFFF5AL, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // read fourth (non-canonical representation of 0x8FFFFFFFFFFFFF5A)
        try {
            long v = r.readVarUInt64();
            assertEquals(0x8FFFFFFFFFFFFF5AL, v);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            long v = r.readVarUInt64();
            fail("Operation can't succeed: " + v);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }

    @Test
    public void skipBytes() {
        // setup
        byte[] inputBytes = new byte[]{
                // 7 bytes
                (byte) 0x01, (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x05, (byte) 0x08, (byte) 0x013,

                // 4 bytes
                (byte) 0x55, (byte) 0x00, (byte) 0xFF, (byte) 0xAA
        };
        ByteArrayInputStream bais = new ByteArrayInputStream(inputBytes);
        BinaryStreamReader r = new BinaryStreamReader(bais);

        // skip first and read second
        try {
            int skippedLength = 7;
            int readLength = 4;
            r.skipBytes(skippedLength);
            byte[] v = r.readBytes(readLength);
            for (int i = 0; i < readLength; ++i) {
                assertEquals(inputBytes[i + skippedLength], v[i]);
            }
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // end of stream
        try {
            int skippedLength = 1;
            r.skipBytes(skippedLength);
            fail("Operation can't succeed: " + skippedLength);
        } catch (EOFException e) {
            // success
        } catch (IOException e) {
            fail("Other IOException can't be thrown here: " + e);
        }
    }
}
