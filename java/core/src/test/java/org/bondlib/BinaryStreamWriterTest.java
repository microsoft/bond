// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import static org.junit.Assert.*;

public class BinaryStreamWriterTest {
    @Test
    public void writeInt8() {
        // setup
        byte[] expectedBytes = new byte[]{
                (byte) 0x5A,
                (byte) 0xE3
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeInt8((byte) 0x5A);
            assertEquals(1, baos.size());

            w.writeInt8((byte) 0xE3);
            assertEquals(2, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeInt16() {
        // setup
        byte[] expectedBytes = new byte[]{
                // 0xE35A
                (byte) 0x5A, (byte) 0xE3,

                // 0xAAC0
                (byte) 0xC0, (byte) 0xAA
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeInt16((short) 0xE35A);
            assertEquals(2, baos.size());

            w.writeInt16((short) 0xAAC0);
            assertEquals(4, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeInt32() {
        // setup
        byte[] expectedBytes = new byte[]{
                // 0x7F32E35A
                (byte) 0x5A, (byte) 0xE3, (byte) 0x32, (byte) 0x7F,

                // 0xBB00AAC0
                (byte) 0xC0, (byte) 0xAA, (byte) 0x00, (byte) 0xBB
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeInt32(0x7F32E35A);
            assertEquals(4, baos.size());

            w.writeInt32(0xBB00AAC0);
            assertEquals(8, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeInt64() {
        // setup
        byte[] expectedBytes = new byte[]{
                // 0x325476987F32E35A
                (byte) 0x5A, (byte) 0xE3, (byte) 0x32, (byte) 0x7F,
                (byte) 0x98, (byte) 0x76, (byte) 0x54, (byte) 0x32,

                // 0xCCFF3113BB00AAC0
                (byte) 0xC0, (byte) 0xAA, (byte) 0x00, (byte) 0xBB,
                (byte) 0x13, (byte) 0x31, (byte) 0xFF, (byte) 0xCC
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeInt64(0x325476987F32E35AL);
            assertEquals(8, baos.size());

            w.writeInt64(0xCCFF3113BB00AAC0L);
            assertEquals(16, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeFloat() {
        // setup
        byte[] expectedBytes = new byte[]{
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
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeFloat(0F);
            assertEquals(4, baos.size());

            w.writeFloat((float) Math.PI);
            assertEquals(8, baos.size());

            w.writeFloat(Float.NEGATIVE_INFINITY);
            assertEquals(12, baos.size());

            w.writeFloat(Float.POSITIVE_INFINITY);
            assertEquals(16, baos.size());

            // NaN values need special handling:
            // * we're avoiding Float.NaN since it may not be the canonical representation on some JVM implementations
            // * we're also avoiding Float.intBitsToFloat since it may not preserve the same NaN bit pattern
            float nan0 = TestHelper.rawIntBitsToFloat(0x7FC00000);
            assertTrue(Float.isNaN(nan0));
            w.writeFloat(nan0);
            assertEquals(20, baos.size());

            float nan1 = TestHelper.rawIntBitsToFloat(0xFFC00000);
            assertTrue(Float.isNaN(nan1));
            w.writeFloat(nan1);
            assertEquals(24, baos.size());

            float nan2 = TestHelper.rawIntBitsToFloat(0x7F800001);
            assertTrue(Float.isNaN(nan2));
            w.writeFloat(nan2);
            assertEquals(28, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeDouble() {
        // setup
        byte[] expectedBytes = new byte[]{
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
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeDouble(0);
            assertEquals(8, baos.size());

            w.writeDouble(Math.PI);
            assertEquals(16, baos.size());

            w.writeDouble(Double.NEGATIVE_INFINITY);
            assertEquals(24, baos.size());

            w.writeDouble(Double.POSITIVE_INFINITY);
            assertEquals(32, baos.size());

            // NaN values need special handling:
            // * we're avoiding Double.NaN since it may not be the canonical representation on some JVM implementations
            // * we're also avoiding Double.longBitsToDouble since it may not preserve the same NaN bit pattern
            double nan0 = TestHelper.rawLongBitsToDouble(0x7FF8000000000000L);
            assertTrue(Double.isNaN(nan0));
            w.writeDouble(nan0);
            assertEquals(40, baos.size());

            double nan1 = TestHelper.rawLongBitsToDouble(0xFFF8000000000000L);
            assertTrue(Double.isNaN(nan1));
            w.writeDouble(nan1);
            assertEquals(48, baos.size());

            double nan2 = TestHelper.rawLongBitsToDouble(0x7FF0000000000001L);
            assertTrue(Double.isNaN(nan2));
            w.writeDouble(nan2);
            assertEquals(56, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeBool() {
        // setup
        byte[] expectedBytes = new byte[]{
                // false (anything else is true)
                (byte) 0x00,

                // true (the canonical form)
                // (the encoder represents true by 0x01)
                (byte) 0x01
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeBool(false);
            assertEquals(1, baos.size());

            w.writeBool(true);
            assertEquals(2, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeBytes() {
        // setup
        byte[] bytes1 = new byte[]{
                // 7 bytes
                (byte) 0x01, (byte) 0x01, (byte) 0x02, (byte) 0x03, (byte) 0x05, (byte) 0x08, (byte) 0x013
        };
        byte[] bytes2 = new byte[]{
                // 4 bytes
                (byte) 0x55, (byte) 0x00, (byte) 0xFF, (byte) 0xAA
        };
        byte[] expectedBytes = new byte[bytes1.length + bytes2.length];
        System.arraycopy(bytes1, 0, expectedBytes, 0, bytes1.length);
        System.arraycopy(bytes2, 0, expectedBytes, bytes1.length, bytes2.length);
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeBytes(bytes1);
            assertEquals(bytes1.length, baos.size());

            w.writeBytes(bytes2);
            assertEquals(bytes1.length + bytes2.length, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeVarUInt16() {
        // setup
        byte[] expectedBytes = new byte[]{
                // 0x005A
                (byte) 0x5A,

                // 0x075A
                (byte) 0xDA, (byte) 0x0E
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeVarUInt16((short) 0x005A);
            assertEquals(1, baos.size());

            w.writeVarUInt16((short) 0x075A);
            assertEquals(3, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeVarUInt32() {
        // setup
        byte[] expectedBytes = new byte[]{
                // 0x005A
                (byte) 0x5A,

                // 0x8FFFFF5A
                (byte) 0xDA, (byte) 0xFE, (byte) 0xFF, (byte) 0xFF, (byte) 0x08
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeVarUInt32(0x0000005A);
            assertEquals(1, baos.size());

            w.writeVarUInt32(0x8FFFFF5A);
            assertEquals(6, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }

    @Test
    public void writeVarUInt64() {
        // setup
        byte[] expectedBytes = new byte[]{
                // 0x000000000000005A
                (byte) 0x5A,

                // 0x8FFFFFFFFFFFFF5A (canonical, last byte is 0x01)
                // (the encoder sets the 10th byte to 0x01 since it's the proper canonical form)
                (byte) 0xDA, (byte) 0xFE, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF,
                (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x8F, (byte) 0x01,
        };
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        BinaryStreamWriter w = new BinaryStreamWriter(baos);

        // write
        try {
            w.writeVarUInt64(0x000000000000005AL);
            assertEquals(1, baos.size());

            w.writeVarUInt64(0x8FFFFFFFFFFFFF5AL);
            assertEquals(11, baos.size());
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify
        byte[] actualBytes = baos.toByteArray();
        assertArrayEquals(expectedBytes, actualBytes);
    }
}
