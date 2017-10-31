// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class StringHelperTest {

    @Test
    public void staticClass() {
        TestHelper.verifyStaticHelperClass(StringHelper.class);
    }

    @Test
    public void testStringEncoding() {
        // empty string
        testStringEncodingDecoding(
                "",
                new byte[]{});

        // various languages (producing UTF-8 of 1-3 units long)

        testStringEncodingDecoding(
                "unit test",
                new byte[]{
                        (byte) 0x75,
                        (byte) 0x6E,
                        (byte) 0x69,
                        (byte) 0x74,
                        (byte) 0x20,
                        (byte) 0x74,
                        (byte) 0x65,
                        (byte) 0x73,
                        (byte) 0x74
                });

        testStringEncodingDecoding(
                "\u55ae\u5143\u6e2c\u8a66",
                new byte[]{
                        (byte) 0xE5, (byte) 0x96, (byte) 0xAE,
                        (byte) 0xE5, (byte) 0x85, (byte) 0x83,
                        (byte) 0xE6, (byte) 0xB8, (byte) 0xAC,
                        (byte) 0xE8, (byte) 0xA9, (byte) 0xA6
                });

        testStringEncodingDecoding(
                "\u30e6\u30cb\u30c3\u30c8\u30c6\u30b9\u30c8",
                new byte[]{
                        (byte) 0xE3, (byte) 0x83, (byte) 0xA6,
                        (byte) 0xE3, (byte) 0x83, (byte) 0x8B,
                        (byte) 0xE3, (byte) 0x83, (byte) 0x83,
                        (byte) 0xE3, (byte) 0x83, (byte) 0x88,
                        (byte) 0xE3, (byte) 0x83, (byte) 0x86,
                        (byte) 0xE3, (byte) 0x82, (byte) 0xB9,
                        (byte) 0xE3, (byte) 0x83, (byte) 0x88
                });

        testStringEncodingDecoding(
                "\u0a2f\u0a42\u0a28\u0a3f\u0a1f \u0a1f\u0a48\u0a38\u0a1f",
                new byte[]{
                        (byte) 0xE0, (byte) 0xA8, (byte) 0xAF,
                        (byte) 0xE0, (byte) 0xA9, (byte) 0x82,
                        (byte) 0xE0, (byte) 0xA8, (byte) 0xA8,
                        (byte) 0xE0, (byte) 0xA8, (byte) 0xBF,
                        (byte) 0xE0, (byte) 0xA8, (byte) 0x9F,
                        (byte) 0x20,
                        (byte) 0xE0, (byte) 0xA8, (byte) 0x9F,
                        (byte) 0xE0, (byte) 0xA9, (byte) 0x88,
                        (byte) 0xE0, (byte) 0xA8, (byte) 0xB8,
                        (byte) 0xE0, (byte) 0xA8, (byte) 0x9F
                });

        testStringEncodingDecoding(
                "\u043c\u043e\u0434\u0443\u043b\u044c\u043d\u044b\u0439 \u0442\u0435\u0441\u0442",
                new byte[]{
                        (byte) 0xD0, (byte) 0xBC,
                        (byte) 0xD0, (byte) 0xBE,
                        (byte) 0xD0, (byte) 0xB4,
                        (byte) 0xD1, (byte) 0x83,
                        (byte) 0xD0, (byte) 0xBB,
                        (byte) 0xD1, (byte) 0x8C,
                        (byte) 0xD0, (byte) 0xBD,
                        (byte) 0xD1, (byte) 0x8B,
                        (byte) 0xD0, (byte) 0xB9,
                        (byte) 0x20,
                        (byte) 0xD1, (byte) 0x82,
                        (byte) 0xD0, (byte) 0xB5,
                        (byte) 0xD1, (byte) 0x81,
                        (byte) 0xD1, (byte) 0x82
                });

        testStringEncodingDecoding(
                "Ki\u1ec3m tra \u0111\u01a1n v\u1ecb",
                new byte[]{
                        (byte) 0x4B,
                        (byte) 0x69,
                        (byte) 0xE1, (byte) 0xBB, (byte) 0x83,
                        (byte) 0x6D,
                        (byte) 0x20,
                        (byte) 0x74,
                        (byte) 0x72,
                        (byte) 0x61,
                        (byte) 0x20,
                        (byte) 0xC4, (byte) 0x91,
                        (byte) 0xC6, (byte) 0xA1,
                        (byte) 0x6E,
                        (byte) 0x20,
                        (byte) 0x76,
                        (byte) 0xE1, (byte) 0xBB, (byte) 0x8B
                });

        testStringEncodingDecoding(
                "\u00fcnite testi",
                new byte[]{
                        (byte) 0xC3, (byte) 0xBC,
                        (byte) 0x6E,
                        (byte) 0x69,
                        (byte) 0x74,
                        (byte) 0x65,
                        (byte) 0x20,
                        (byte) 0x74,
                        (byte) 0x65,
                        (byte) 0x73,
                        (byte) 0x74,
                        (byte) 0x69
                });

        testStringEncodingDecoding(
                "\u10d4\u10e0\u10d7\u10d4\u10e3\u10da\u10d8 \u10d2\u10d0\u10db\u10dd\u10ea\u10d3\u10d0",
                new byte[]{
                        (byte) 0xE1, (byte) 0x83, (byte) 0x94,
                        (byte) 0xE1, (byte) 0x83, (byte) 0xA0,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x97,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x94,
                        (byte) 0xE1, (byte) 0x83, (byte) 0xA3,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x9A,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x98,
                        (byte) 0x20,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x92,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x90,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x9B,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x9D,
                        (byte) 0xE1, (byte) 0x83, (byte) 0xAA,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x93,
                        (byte) 0xE1, (byte) 0x83, (byte) 0x90
                });

        testStringEncodingDecoding(
                "\u05de\u05d1\u05d7\u05df \u05d9\u05d7\u05d9\u05d3\u05d4",
                new byte[]{
                        (byte) 0xD7, (byte) 0x9E,
                        (byte) 0xD7, (byte) 0x91,
                        (byte) 0xD7, (byte) 0x97,
                        (byte) 0xD7, (byte) 0x9F,
                        (byte) 0x20,
                        (byte) 0xD7, (byte) 0x99,
                        (byte) 0xD7, (byte) 0x97,
                        (byte) 0xD7, (byte) 0x99,
                        (byte) 0xD7, (byte) 0x93,
                        (byte) 0xD7, (byte) 0x94
                });

        testStringEncodingDecoding(
                "\u0627\u062e\u062a\u0628\u0627\u0631 \u0627\u0644\u0648\u062d\u062f\u0629",
                new byte[]{
                        (byte) 0xD8, (byte) 0xA7,
                        (byte) 0xD8, (byte) 0xAE,
                        (byte) 0xD8, (byte) 0xAA,
                        (byte) 0xD8, (byte) 0xA8,
                        (byte) 0xD8, (byte) 0xA7,
                        (byte) 0xD8, (byte) 0xB1,
                        (byte) 0x20,
                        (byte) 0xD8, (byte) 0xA7,
                        (byte) 0xD9, (byte) 0x84,
                        (byte) 0xD9, (byte) 0x88,
                        (byte) 0xD8, (byte) 0xAD,
                        (byte) 0xD8, (byte) 0xAF,
                        (byte) 0xD8, (byte) 0xA9
                });

        // 4 code units
        testStringEncodingDecoding(
                "\uD852\uDF62",
                new byte[]{
                        (byte) 0xF0, (byte) 0xA4, (byte) 0xAD, (byte) 0xA2,
                });
    }

    @Test
    public void testWStringEncoding() {
        // empty string
        testWStringEncodingDecoding(
                "",
                new byte[]{});

        // various languages (producing UTF-16LE of 1 unit long)

        testWStringEncodingDecoding(
                "unit test",
                new byte[]{
                        (byte) 0x75, (byte) 0x00,
                        (byte) 0x6E, (byte) 0x00,
                        (byte) 0x69, (byte) 0x00,
                        (byte) 0x74, (byte) 0x00,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x74, (byte) 0x00,
                        (byte) 0x65, (byte) 0x00,
                        (byte) 0x73, (byte) 0x00,
                        (byte) 0x74, (byte) 0x00
                });

        testWStringEncodingDecoding(
                "\u55ae\u5143\u6e2c\u8a66",
                new byte[]{
                        (byte) 0xAE, (byte) 0x55,
                        (byte) 0x43, (byte) 0x51,
                        (byte) 0x2C, (byte) 0x6E,
                        (byte) 0x66, (byte) 0x8A
                });

        testWStringEncodingDecoding(
                "\u30e6\u30cb\u30c3\u30c8\u30c6\u30b9\u30c8",
                new byte[]{
                        (byte) 0xE6, (byte) 0x30,
                        (byte) 0xCB, (byte) 0x30,
                        (byte) 0xC3, (byte) 0x30,
                        (byte) 0xC8, (byte) 0x30,
                        (byte) 0xC6, (byte) 0x30,
                        (byte) 0xB9, (byte) 0x30,
                        (byte) 0xC8, (byte) 0x30
                });

        testWStringEncodingDecoding(
                "\u0a2f\u0a42\u0a28\u0a3f\u0a1f \u0a1f\u0a48\u0a38\u0a1f",
                new byte[]{
                        (byte) 0x2F, (byte) 0x0A,
                        (byte) 0x42, (byte) 0x0A,
                        (byte) 0x28, (byte) 0x0A,
                        (byte) 0x3F, (byte) 0x0A,
                        (byte) 0x1F, (byte) 0x0A,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x1F, (byte) 0x0A,
                        (byte) 0x48, (byte) 0x0A,
                        (byte) 0x38, (byte) 0x0A,
                        (byte) 0x1F, (byte) 0x0A
                });

        testWStringEncodingDecoding(
                "\u043c\u043e\u0434\u0443\u043b\u044c\u043d\u044b\u0439 \u0442\u0435\u0441\u0442",
                new byte[]{
                        (byte) 0x3C, (byte) 0x04,
                        (byte) 0x3E, (byte) 0x04,
                        (byte) 0x34, (byte) 0x04,
                        (byte) 0x43, (byte) 0x04,
                        (byte) 0x3B, (byte) 0x04,
                        (byte) 0x4C, (byte) 0x04,
                        (byte) 0x3D, (byte) 0x04,
                        (byte) 0x4B, (byte) 0x04,
                        (byte) 0x39, (byte) 0x04,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x42, (byte) 0x04,
                        (byte) 0x35, (byte) 0x04,
                        (byte) 0x41, (byte) 0x04,
                        (byte) 0x42, (byte) 0x04
                });

        testWStringEncodingDecoding(
                "Ki\u1ec3m tra \u0111\u01a1n v\u1ecb",
                new byte[]{
                        (byte) 0x4B, (byte) 0x00,
                        (byte) 0x69, (byte) 0x00,
                        (byte) 0xC3, (byte) 0x1E,
                        (byte) 0x6D, (byte) 0x00,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x74, (byte) 0x00,
                        (byte) 0x72, (byte) 0x00,
                        (byte) 0x61, (byte) 0x00,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x11, (byte) 0x01,
                        (byte) 0xA1, (byte) 0x01,
                        (byte) 0x6E, (byte) 0x00,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x76, (byte) 0x00,
                        (byte) 0xCB, (byte) 0x1E
                });

        testWStringEncodingDecoding(
                "\u00fcnite testi",
                new byte[]{
                        (byte) 0xFC, (byte) 0x00,
                        (byte) 0x6E, (byte) 0x00,
                        (byte) 0x69, (byte) 0x00,
                        (byte) 0x74, (byte) 0x00,
                        (byte) 0x65, (byte) 0x00,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x74, (byte) 0x00,
                        (byte) 0x65, (byte) 0x00,
                        (byte) 0x73, (byte) 0x00,
                        (byte) 0x74, (byte) 0x00,
                        (byte) 0x69, (byte) 0x00
                });

        testWStringEncodingDecoding(
                "\u10d4\u10e0\u10d7\u10d4\u10e3\u10da\u10d8 \u10d2\u10d0\u10db\u10dd\u10ea\u10d3\u10d0",
                new byte[]{
                        (byte) 0xD4, (byte) 0x10,
                        (byte) 0xE0, (byte) 0x10,
                        (byte) 0xD7, (byte) 0x10,
                        (byte) 0xD4, (byte) 0x10,
                        (byte) 0xE3, (byte) 0x10,
                        (byte) 0xDA, (byte) 0x10,
                        (byte) 0xD8, (byte) 0x10,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0xD2, (byte) 0x10,
                        (byte) 0xD0, (byte) 0x10,
                        (byte) 0xDB, (byte) 0x10,
                        (byte) 0xDD, (byte) 0x10,
                        (byte) 0xEA, (byte) 0x10,
                        (byte) 0xD3, (byte) 0x10,
                        (byte) 0xD0, (byte) 0x10
                });

        testWStringEncodingDecoding(
                "\u05de\u05d1\u05d7\u05df \u05d9\u05d7\u05d9\u05d3\u05d4",
                new byte[]{
                        (byte) 0xDE, (byte) 0x05,
                        (byte) 0xD1, (byte) 0x05,
                        (byte) 0xD7, (byte) 0x05,
                        (byte) 0xDF, (byte) 0x05,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0xD9, (byte) 0x05,
                        (byte) 0xD7, (byte) 0x05,
                        (byte) 0xD9, (byte) 0x05,
                        (byte) 0xD3, (byte) 0x05,
                        (byte) 0xD4, (byte) 0x05
                });

        testWStringEncodingDecoding(
                "\u0627\u062e\u062a\u0628\u0627\u0631 \u0627\u0644\u0648\u062d\u062f\u0629",
                new byte[]{
                        (byte) 0x27, (byte) 0x06,
                        (byte) 0x2E, (byte) 0x06,
                        (byte) 0x2A, (byte) 0x06,
                        (byte) 0x28, (byte) 0x06,
                        (byte) 0x27, (byte) 0x06,
                        (byte) 0x31, (byte) 0x06,
                        (byte) 0x20, (byte) 0x00,
                        (byte) 0x27, (byte) 0x06,
                        (byte) 0x44, (byte) 0x06,
                        (byte) 0x48, (byte) 0x06,
                        (byte) 0x2D, (byte) 0x06,
                        (byte) 0x2F, (byte) 0x06,
                        (byte) 0x29, (byte) 0x06
                });

        // 2 code units
        testStringEncodingDecoding(
                "\uD852\uDF62",
                new byte[]{
                        (byte) 0xF0, (byte) 0xA4, (byte) 0xAD, (byte) 0xA2
                });
    }

    private static void testStringEncodingDecoding(String decoded, byte[] encoded) {

        // test encoding
        byte[] actualEncoded = StringHelper.encodeString(decoded);
        assertEquals(encoded.length, actualEncoded.length);
        assertArrayEquals(encoded, actualEncoded);

        // test decoding
        String actualDecoded = StringHelper.decodeString(encoded);
        assertEquals(decoded.length(), actualDecoded.length());
        assertEquals(decoded, actualDecoded);

        // test encoded length
        assertEquals(encoded.length, StringHelper.getEncodedStringLength(decoded));
    }

    private static void testWStringEncodingDecoding(String decoded, byte[] encoded) {

        // test encoding
        byte[] actualEncoded = StringHelper.encodeWString(decoded);
        assertEquals(encoded.length, actualEncoded.length);
        assertArrayEquals(encoded, actualEncoded);

        // test decoding
        String actualDecoded = StringHelper.decodeWString(encoded);
        assertEquals(decoded.length(), actualDecoded.length());
        assertEquals(decoded, actualDecoded);

        // test encoded length
        assertEquals(encoded.length, StringHelper.getEncodedWStringLength(decoded));
    }
}
