package com.microsoft.bond.protocol;

import org.junit.Test;
import sun.nio.cs.SingleByte;

import static org.junit.Assert.*;

public class StringHelperTest {

    @Test
    public void callImplicitDefaultConstructor() {
        // adds the implicit default constructor to unit test code coverage
        new StringHelper();
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
                "單元測試",
                new byte[]{
                        (byte) 0xE5, (byte) 0x96, (byte) 0xAE,
                        (byte) 0xE5, (byte) 0x85, (byte) 0x83,
                        (byte) 0xE6, (byte) 0xB8, (byte) 0xAC,
                        (byte) 0xE8, (byte) 0xA9, (byte) 0xA6
                });

        testStringEncodingDecoding(
                "ユニットテスト",
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
                "ਯੂਨਿਟ ਟੈਸਟ",
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
                "модульный тест",
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
                "Kiểm tra đơn vị",
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
                "ünite testi",
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
                "ერთეული გამოცდა",
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
                "מבחן יחידה",
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
                "اختبار الوحدة",
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
                new byte[] {
                        (byte)0xF0, (byte)0xA4, (byte)0xAD, (byte)0xA2,
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
                "單元測試",
                new byte[]{
                        (byte) 0xAE, (byte) 0x55,
                        (byte) 0x43, (byte) 0x51,
                        (byte) 0x2C, (byte) 0x6E,
                        (byte) 0x66, (byte) 0x8A
                });

        testWStringEncodingDecoding(
                "ユニットテスト",
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
                "ਯੂਨਿਟ ਟੈਸਟ",
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
                "модульный тест",
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
                "Kiểm tra đơn vị",
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
                "ünite testi",
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
                "ერთეული გამოცდა",
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
                "מבחן יחידה",
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
                "اختبار الوحدة",
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
                new byte[] {
                        (byte)0xF0, (byte)0xA4, (byte)0xAD, (byte)0xA2
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