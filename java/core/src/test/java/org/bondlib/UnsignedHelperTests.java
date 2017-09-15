// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.math.BigInteger;

import static org.junit.Assert.*;

public class UnsignedHelperTests {

    @Test
    public void staticClass() {
        TestHelper.verifyStaticHelperClass(UnsignedHelper.class);
    }

    @Test
    public void asUnsignedShort_Byte() {
        testWidenedResultShort((short) 0, (byte) 0x00);

        testWidenedResultShort((short) 1, (byte) 0x01);

        testWidenedResultShort((short) 2, (byte) 0x02);
        testWidenedResultShort((short) 3, (byte) 0x03);

        testWidenedResultShort((short) 4, (byte) 0x04);
        testWidenedResultShort((short) 5, (byte) 0x05);

        testWidenedResultShort((short) 7, (byte) 0x07);
        testWidenedResultShort((short) 8, (byte) 0x08);
        testWidenedResultShort((short) 9, (byte) 0x09);

        testWidenedResultShort((short) 15, (byte) 0x0F);
        testWidenedResultShort((short) 16, (byte) 0x10);
        testWidenedResultShort((short) 17, (byte) 0x11);

        testWidenedResultShort((short) 31, (byte) 0x1F);
        testWidenedResultShort((short) 32, (byte) 0x20);
        testWidenedResultShort((short) 33, (byte) 0x21);

        testWidenedResultShort((short) 63, (byte) 0x3F);
        testWidenedResultShort((short) 64, (byte) 0x40);
        testWidenedResultShort((short) 65, (byte) 0x41);

        testWidenedResultShort((short) 127, (byte) 0x7F);
        testWidenedResultShort((short) 128, (byte) 0x80);
        testWidenedResultShort((short) 129, (byte) 0x81);

        testWidenedResultShort((short) 255, (byte) 0xFF);
    }

    @Test
    public void asUnsignedInt_Byte() {
        testWidenedResultInt(0, (byte) 0x00);

        testWidenedResultInt(1, (byte) 0x01);

        testWidenedResultInt(2, (byte) 0x02);
        testWidenedResultInt(3, (byte) 0x03);

        testWidenedResultInt(4, (byte) 0x04);
        testWidenedResultInt(5, (byte) 0x05);

        testWidenedResultInt(7, (byte) 0x07);
        testWidenedResultInt(8, (byte) 0x08);
        testWidenedResultInt(9, (byte) 0x09);

        testWidenedResultInt(15, (byte) 0x0F);
        testWidenedResultInt(16, (byte) 0x10);
        testWidenedResultInt(17, (byte) 0x11);

        testWidenedResultInt(31, (byte) 0x1F);
        testWidenedResultInt(32, (byte) 0x20);
        testWidenedResultInt(33, (byte) 0x21);

        testWidenedResultInt(63, (byte) 0x3F);
        testWidenedResultInt(64, (byte) 0x40);
        testWidenedResultInt(65, (byte) 0x41);

        testWidenedResultInt(127, (byte) 0x7F);
        testWidenedResultInt(128, (byte) 0x80);
        testWidenedResultInt(129, (byte) 0x81);

        testWidenedResultInt(255, (byte) 0xFF);
    }

    @Test
    public void asUnsignedInt_Short() {
        testWidenedResultInt(0, (short) 0x0000);

        testWidenedResultInt(1, (short) 0x0001);

        testWidenedResultInt(2, (short) 0x0002);
        testWidenedResultInt(3, (short) 0x0003);

        testWidenedResultInt(4, (short) 0x0004);
        testWidenedResultInt(5, (short) 0x0005);

        testWidenedResultInt(7, (short) 0x0007);
        testWidenedResultInt(8, (short) 0x0008);
        testWidenedResultInt(9, (short) 0x0009);

        testWidenedResultInt(15, (short) 0x000F);
        testWidenedResultInt(16, (short) 0x0010);
        testWidenedResultInt(17, (short) 0x0011);

        testWidenedResultInt(31, (short) 0x001F);
        testWidenedResultInt(32, (short) 0x0020);
        testWidenedResultInt(33, (short) 0x0021);

        testWidenedResultInt(63, (short) 0x003F);
        testWidenedResultInt(64, (short) 0x0040);
        testWidenedResultInt(65, (short) 0x0041);

        testWidenedResultInt(127, (short) 0x007F);
        testWidenedResultInt(128, (short) 0x0080);
        testWidenedResultInt(129, (short) 0x0081);

        testWidenedResultInt(255, (short) 0x00FF);
        testWidenedResultInt(256, (short) 0x0100);
        testWidenedResultInt(257, (short) 0x0101);

        testWidenedResultInt(511, (short) 0x01FF);
        testWidenedResultInt(512, (short) 0x0200);
        testWidenedResultInt(513, (short) 0x0201);

        testWidenedResultInt(1023, (short) 0x03FF);
        testWidenedResultInt(1024, (short) 0x0400);
        testWidenedResultInt(1025, (short) 0x0401);

        testWidenedResultInt(2047, (short) 0x07FF);
        testWidenedResultInt(2048, (short) 0x0800);
        testWidenedResultInt(2049, (short) 0x0801);

        testWidenedResultInt(4095, (short) 0x0FFF);
        testWidenedResultInt(4096, (short) 0x1000);
        testWidenedResultInt(4097, (short) 0x1001);

        testWidenedResultInt(8191, (short) 0x1FFF);
        testWidenedResultInt(8192, (short) 0x2000);
        testWidenedResultInt(8193, (short) 0x2001);

        testWidenedResultInt(16383, (short) 0x3FFF);
        testWidenedResultInt(16384, (short) 0x4000);
        testWidenedResultInt(16385, (short) 0x4001);

        testWidenedResultInt(32767, (short) 0x7FFF);
        testWidenedResultInt(32768, (short) 0x8000);
        testWidenedResultInt(32769, (short) 0x8001);

        testWidenedResultInt(65535, (short) 0xFFFF);
    }

    @Test
    public void asUnsignedLong_Byte() {
        testWidenedResultLong(0L, (byte) 0x00);

        testWidenedResultLong(1L, (byte) 0x01);

        testWidenedResultLong(2L, (byte) 0x02);
        testWidenedResultLong(3L, (byte) 0x03);

        testWidenedResultLong(4L, (byte) 0x04);
        testWidenedResultLong(5L, (byte) 0x05);

        testWidenedResultLong(7L, (byte) 0x07);
        testWidenedResultLong(8L, (byte) 0x08);
        testWidenedResultLong(9L, (byte) 0x09);

        testWidenedResultLong(15L, (byte) 0x0F);
        testWidenedResultLong(16L, (byte) 0x10);
        testWidenedResultLong(17L, (byte) 0x11);

        testWidenedResultLong(31L, (byte) 0x1F);
        testWidenedResultLong(32L, (byte) 0x20);
        testWidenedResultLong(33L, (byte) 0x21);

        testWidenedResultLong(63L, (byte) 0x3F);
        testWidenedResultLong(64L, (byte) 0x40);
        testWidenedResultLong(65L, (byte) 0x41);

        testWidenedResultLong(127L, (byte) 0x7F);
        testWidenedResultLong(128L, (byte) 0x80);
        testWidenedResultLong(129L, (byte) 0x81);

        testWidenedResultLong(255L, (byte) 0xFF);
    }

    @Test
    public void asUnsignedLong_Short() {
        testWidenedResultLong(0L, (short) 0x0000);

        testWidenedResultLong(1L, (short) 0x0001);

        testWidenedResultLong(2L, (short) 0x0002);
        testWidenedResultLong(3L, (short) 0x0003);

        testWidenedResultLong(4L, (short) 0x0004);
        testWidenedResultLong(5L, (short) 0x0005);

        testWidenedResultLong(7L, (short) 0x0007);
        testWidenedResultLong(8L, (short) 0x0008);
        testWidenedResultLong(9L, (short) 0x0009);

        testWidenedResultLong(15L, (short) 0x000F);
        testWidenedResultLong(16L, (short) 0x0010);
        testWidenedResultLong(17L, (short) 0x0011);

        testWidenedResultLong(31L, (short) 0x001F);
        testWidenedResultLong(32L, (short) 0x0020);
        testWidenedResultLong(33L, (short) 0x0021);

        testWidenedResultLong(63L, (short) 0x003F);
        testWidenedResultLong(64L, (short) 0x0040);
        testWidenedResultLong(65L, (short) 0x0041);

        testWidenedResultLong(127L, (short) 0x007F);
        testWidenedResultLong(128L, (short) 0x0080);
        testWidenedResultLong(129L, (short) 0x0081);

        testWidenedResultLong(255L, (short) 0x00FF);
        testWidenedResultLong(256L, (short) 0x0100);
        testWidenedResultLong(257L, (short) 0x0101);

        testWidenedResultLong(511L, (short) 0x01FF);
        testWidenedResultLong(512L, (short) 0x0200);
        testWidenedResultLong(513L, (short) 0x0201);

        testWidenedResultLong(1023L, (short) 0x03FF);
        testWidenedResultLong(1024L, (short) 0x0400);
        testWidenedResultLong(1025L, (short) 0x0401);

        testWidenedResultLong(2047L, (short) 0x07FF);
        testWidenedResultLong(2048L, (short) 0x0800);
        testWidenedResultLong(2049L, (short) 0x0801);

        testWidenedResultLong(4095L, (short) 0x0FFF);
        testWidenedResultLong(4096L, (short) 0x1000);
        testWidenedResultLong(4097L, (short) 0x1001);

        testWidenedResultLong(8191L, (short) 0x1FFF);
        testWidenedResultLong(8192L, (short) 0x2000);
        testWidenedResultLong(8193L, (short) 0x2001);

        testWidenedResultLong(16383L, (short) 0x3FFF);
        testWidenedResultLong(16384L, (short) 0x4000);
        testWidenedResultLong(16385L, (short) 0x4001);

        testWidenedResultLong(32767L, (short) 0x7FFF);
        testWidenedResultLong(32768L, (short) 0x8000);
        testWidenedResultLong(32769L, (short) 0x8001);

        testWidenedResultLong(65535L, (short) 0xFFFF);
    }

    @Test
    public void asUnsignedLong_Int() {
        testWidenedResultLong(0L, 0x00000000);

        testWidenedResultLong(1L, 0x00000001);

        testWidenedResultLong(2L, 0x00000002);
        testWidenedResultLong(3L, 0x00000003);

        testWidenedResultLong(4L, 0x00000004);
        testWidenedResultLong(5L, 0x00000005);

        testWidenedResultLong(7L, 0x00000007);
        testWidenedResultLong(8L, 0x00000008);
        testWidenedResultLong(9L, 0x00000009);

        testWidenedResultLong(15L, 0x0000000F);
        testWidenedResultLong(16L, 0x00000010);
        testWidenedResultLong(17L, 0x00000011);

        testWidenedResultLong(31L, 0x0000001F);
        testWidenedResultLong(32L, 0x00000020);
        testWidenedResultLong(33L, 0x00000021);

        testWidenedResultLong(63L, 0x0000003F);
        testWidenedResultLong(64L, 0x00000040);
        testWidenedResultLong(65L, 0x00000041);

        testWidenedResultLong(127L, 0x0000007F);
        testWidenedResultLong(128L, 0x00000080);
        testWidenedResultLong(129L, 0x00000081);

        testWidenedResultLong(255L, 0x000000FF);
        testWidenedResultLong(256L, 0x00000100);
        testWidenedResultLong(257L, 0x00000101);

        testWidenedResultLong(511L, 0x000001FF);
        testWidenedResultLong(512L, 0x00000200);
        testWidenedResultLong(513L, 0x00000201);

        testWidenedResultLong(1023L, 0x000003FF);
        testWidenedResultLong(1024L, 0x00000400);
        testWidenedResultLong(1025L, 0x00000401);

        testWidenedResultLong(2047L, 0x000007FF);
        testWidenedResultLong(2048L, 0x00000800);
        testWidenedResultLong(2049L, 0x00000801);

        testWidenedResultLong(4095L, 0x00000FFF);
        testWidenedResultLong(4096L, 0x00001000);
        testWidenedResultLong(4097L, 0x00001001);

        testWidenedResultLong(8191L, 0x00001FFF);
        testWidenedResultLong(8192L, 0x00002000);
        testWidenedResultLong(8193L, 0x00002001);

        testWidenedResultLong(16383L, 0x00003FFF);
        testWidenedResultLong(16384L, 0x00004000);
        testWidenedResultLong(16385L, 0x00004001);

        testWidenedResultLong(32767L, 0x00007FFF);
        testWidenedResultLong(32768L, 0x00008000);
        testWidenedResultLong(32769L, 0x00008001);

        testWidenedResultLong(65535L, 0x0000FFFF);
        testWidenedResultLong(65536L, 0x00010000);
        testWidenedResultLong(65537L, 0x00010001);

        testWidenedResultLong(131071L, 0x0001FFFF);
        testWidenedResultLong(131072L, 0x00020000);
        testWidenedResultLong(131073L, 0x00020001);

        testWidenedResultLong(262143L, 0x0003FFFF);
        testWidenedResultLong(262144L, 0x00040000);
        testWidenedResultLong(262145L, 0x00040001);

        testWidenedResultLong(524287L, 0x0007FFFF);
        testWidenedResultLong(524288L, 0x00080000);
        testWidenedResultLong(524289L, 0x00080001);

        testWidenedResultLong(1048575L, 0x000FFFFF);
        testWidenedResultLong(1048576L, 0x00100000);
        testWidenedResultLong(1048577L, 0x00100001);

        testWidenedResultLong(2097151L, 0x001FFFFF);
        testWidenedResultLong(2097152L, 0x00200000);
        testWidenedResultLong(2097153L, 0x00200001);

        testWidenedResultLong(4194303L, 0x003FFFFF);
        testWidenedResultLong(4194304L, 0x00400000);
        testWidenedResultLong(4194305L, 0x00400001);

        testWidenedResultLong(8388607L, 0x007FFFFF);
        testWidenedResultLong(8388608L, 0x00800000);
        testWidenedResultLong(8388609L, 0x00800001);

        testWidenedResultLong(16777215L, 0x00FFFFFF);
        testWidenedResultLong(16777216L, 0x01000000);
        testWidenedResultLong(16777217L, 0x01000001);

        testWidenedResultLong(33554431L, 0x01FFFFFF);
        testWidenedResultLong(33554432L, 0x02000000);
        testWidenedResultLong(33554433L, 0x02000001);

        testWidenedResultLong(67108863L, 0x03FFFFFF);
        testWidenedResultLong(67108864L, 0x04000000);
        testWidenedResultLong(67108865L, 0x04000001);

        testWidenedResultLong(134217727L, 0x07FFFFFF);
        testWidenedResultLong(134217728L, 0x08000000);
        testWidenedResultLong(134217729L, 0x08000001);

        testWidenedResultLong(268435455L, 0x0FFFFFFF);
        testWidenedResultLong(268435456L, 0x10000000);
        testWidenedResultLong(268435457L, 0x10000001);

        testWidenedResultLong(536870911L, 0x1FFFFFFF);
        testWidenedResultLong(536870912L, 0x20000000);
        testWidenedResultLong(536870913L, 0x20000001);

        testWidenedResultLong(1073741823L, 0x3FFFFFFF);
        testWidenedResultLong(1073741824L, 0x40000000);
        testWidenedResultLong(1073741825L, 0x40000001);

        testWidenedResultLong(2147483647L, 0x7FFFFFFF);
        testWidenedResultLong(2147483648L, 0x80000000);
        testWidenedResultLong(2147483649L, 0x80000001);

        testWidenedResultLong(4294967295L, 0xFFFFFFFF);
    }

    @Test
    public void asUnsignedBigInt_Byte() {
        testWidenedResultBigInt(new BigInteger("0"), (byte) 0x00);

        testWidenedResultBigInt(new BigInteger("1"), (byte) 0x01);

        testWidenedResultBigInt(new BigInteger("2"), (byte) 0x02);
        testWidenedResultBigInt(new BigInteger("3"), (byte) 0x03);

        testWidenedResultBigInt(new BigInteger("4"), (byte) 0x04);
        testWidenedResultBigInt(new BigInteger("5"), (byte) 0x05);

        testWidenedResultBigInt(new BigInteger("7"), (byte) 0x07);
        testWidenedResultBigInt(new BigInteger("8"), (byte) 0x08);
        testWidenedResultBigInt(new BigInteger("9"), (byte) 0x09);

        testWidenedResultBigInt(new BigInteger("15"), (byte) 0x0F);
        testWidenedResultBigInt(new BigInteger("16"), (byte) 0x10);
        testWidenedResultBigInt(new BigInteger("17"), (byte) 0x11);

        testWidenedResultBigInt(new BigInteger("31"), (byte) 0x1F);
        testWidenedResultBigInt(new BigInteger("32"), (byte) 0x20);
        testWidenedResultBigInt(new BigInteger("33"), (byte) 0x21);

        testWidenedResultBigInt(new BigInteger("63"), (byte) 0x3F);
        testWidenedResultBigInt(new BigInteger("64"), (byte) 0x40);
        testWidenedResultBigInt(new BigInteger("65"), (byte) 0x41);

        testWidenedResultBigInt(new BigInteger("127"), (byte) 0x7F);
        testWidenedResultBigInt(new BigInteger("128"), (byte) 0x80);
        testWidenedResultBigInt(new BigInteger("129"), (byte) 0x81);

        testWidenedResultBigInt(new BigInteger("255"), (byte) 0xFF);
    }


    @Test
    public void asUnsignedBigInt_Short() {
        testWidenedResultBigInt(new BigInteger("0"), (short) 0x0000);

        testWidenedResultBigInt(new BigInteger("1"), (short) 0x0001);

        testWidenedResultBigInt(new BigInteger("2"), (short) 0x0002);
        testWidenedResultBigInt(new BigInteger("3"), (short) 0x0003);

        testWidenedResultBigInt(new BigInteger("4"), (short) 0x0004);
        testWidenedResultBigInt(new BigInteger("5"), (short) 0x0005);

        testWidenedResultBigInt(new BigInteger("7"), (short) 0x0007);
        testWidenedResultBigInt(new BigInteger("8"), (short) 0x0008);
        testWidenedResultBigInt(new BigInteger("9"), (short) 0x0009);

        testWidenedResultBigInt(new BigInteger("15"), (short) 0x000F);
        testWidenedResultBigInt(new BigInteger("16"), (short) 0x0010);
        testWidenedResultBigInt(new BigInteger("17"), (short) 0x0011);

        testWidenedResultBigInt(new BigInteger("31"), (short) 0x001F);
        testWidenedResultBigInt(new BigInteger("32"), (short) 0x0020);
        testWidenedResultBigInt(new BigInteger("33"), (short) 0x0021);

        testWidenedResultBigInt(new BigInteger("63"), (short) 0x003F);
        testWidenedResultBigInt(new BigInteger("64"), (short) 0x0040);
        testWidenedResultBigInt(new BigInteger("65"), (short) 0x0041);

        testWidenedResultBigInt(new BigInteger("127"), (short) 0x007F);
        testWidenedResultBigInt(new BigInteger("128"), (short) 0x0080);
        testWidenedResultBigInt(new BigInteger("129"), (short) 0x0081);

        testWidenedResultBigInt(new BigInteger("255"), (short) 0x00FF);
        testWidenedResultBigInt(new BigInteger("256"), (short) 0x0100);
        testWidenedResultBigInt(new BigInteger("257"), (short) 0x0101);

        testWidenedResultBigInt(new BigInteger("511"), (short) 0x01FF);
        testWidenedResultBigInt(new BigInteger("512"), (short) 0x0200);
        testWidenedResultBigInt(new BigInteger("513"), (short) 0x0201);

        testWidenedResultBigInt(new BigInteger("1023"), (short) 0x03FF);
        testWidenedResultBigInt(new BigInteger("1024"), (short) 0x0400);
        testWidenedResultBigInt(new BigInteger("1025"), (short) 0x0401);

        testWidenedResultBigInt(new BigInteger("2047"), (short) 0x07FF);
        testWidenedResultBigInt(new BigInteger("2048"), (short) 0x0800);
        testWidenedResultBigInt(new BigInteger("2049"), (short) 0x0801);

        testWidenedResultBigInt(new BigInteger("4095"), (short) 0x0FFF);
        testWidenedResultBigInt(new BigInteger("4096"), (short) 0x1000);
        testWidenedResultBigInt(new BigInteger("4097"), (short) 0x1001);

        testWidenedResultBigInt(new BigInteger("8191"), (short) 0x1FFF);
        testWidenedResultBigInt(new BigInteger("8192"), (short) 0x2000);
        testWidenedResultBigInt(new BigInteger("8193"), (short) 0x2001);

        testWidenedResultBigInt(new BigInteger("16383"), (short) 0x3FFF);
        testWidenedResultBigInt(new BigInteger("16384"), (short) 0x4000);
        testWidenedResultBigInt(new BigInteger("16385"), (short) 0x4001);

        testWidenedResultBigInt(new BigInteger("32767"), (short) 0x7FFF);
        testWidenedResultBigInt(new BigInteger("32768"), (short) 0x8000);
        testWidenedResultBigInt(new BigInteger("32769"), (short) 0x8001);

        testWidenedResultBigInt(new BigInteger("65535"), (short) 0xFFFF);
    }

    @Test
    public void asUnsignedBigInt_Int() {
        testWidenedResultBigInt(new BigInteger("0"), 0x00000000);

        testWidenedResultBigInt(new BigInteger("1"), 0x00000001);

        testWidenedResultBigInt(new BigInteger("2"), 0x00000002);
        testWidenedResultBigInt(new BigInteger("3"), 0x00000003);

        testWidenedResultBigInt(new BigInteger("4"), 0x00000004);
        testWidenedResultBigInt(new BigInteger("5"), 0x00000005);

        testWidenedResultBigInt(new BigInteger("7"), 0x00000007);
        testWidenedResultBigInt(new BigInteger("8"), 0x00000008);
        testWidenedResultBigInt(new BigInteger("9"), 0x00000009);

        testWidenedResultBigInt(new BigInteger("15"), 0x0000000F);
        testWidenedResultBigInt(new BigInteger("16"), 0x00000010);
        testWidenedResultBigInt(new BigInteger("17"), 0x00000011);

        testWidenedResultBigInt(new BigInteger("31"), 0x0000001F);
        testWidenedResultBigInt(new BigInteger("32"), 0x00000020);
        testWidenedResultBigInt(new BigInteger("33"), 0x00000021);

        testWidenedResultBigInt(new BigInteger("63"), 0x0000003F);
        testWidenedResultBigInt(new BigInteger("64"), 0x00000040);
        testWidenedResultBigInt(new BigInteger("65"), 0x00000041);

        testWidenedResultBigInt(new BigInteger("127"), 0x0000007F);
        testWidenedResultBigInt(new BigInteger("128"), 0x00000080);
        testWidenedResultBigInt(new BigInteger("129"), 0x00000081);

        testWidenedResultBigInt(new BigInteger("255"), 0x000000FF);
        testWidenedResultBigInt(new BigInteger("256"), 0x00000100);
        testWidenedResultBigInt(new BigInteger("257"), 0x00000101);

        testWidenedResultBigInt(new BigInteger("511"), 0x000001FF);
        testWidenedResultBigInt(new BigInteger("512"), 0x00000200);
        testWidenedResultBigInt(new BigInteger("513"), 0x00000201);

        testWidenedResultBigInt(new BigInteger("1023"), 0x000003FF);
        testWidenedResultBigInt(new BigInteger("1024"), 0x00000400);
        testWidenedResultBigInt(new BigInteger("1025"), 0x00000401);

        testWidenedResultBigInt(new BigInteger("2047"), 0x000007FF);
        testWidenedResultBigInt(new BigInteger("2048"), 0x00000800);
        testWidenedResultBigInt(new BigInteger("2049"), 0x00000801);

        testWidenedResultBigInt(new BigInteger("4095"), 0x00000FFF);
        testWidenedResultBigInt(new BigInteger("4096"), 0x00001000);
        testWidenedResultBigInt(new BigInteger("4097"), 0x00001001);

        testWidenedResultBigInt(new BigInteger("8191"), 0x00001FFF);
        testWidenedResultBigInt(new BigInteger("8192"), 0x00002000);
        testWidenedResultBigInt(new BigInteger("8193"), 0x00002001);

        testWidenedResultBigInt(new BigInteger("16383"), 0x00003FFF);
        testWidenedResultBigInt(new BigInteger("16384"), 0x00004000);
        testWidenedResultBigInt(new BigInteger("16385"), 0x00004001);

        testWidenedResultBigInt(new BigInteger("32767"), 0x00007FFF);
        testWidenedResultBigInt(new BigInteger("32768"), 0x00008000);
        testWidenedResultBigInt(new BigInteger("32769"), 0x00008001);

        testWidenedResultBigInt(new BigInteger("65535"), 0x0000FFFF);
        testWidenedResultBigInt(new BigInteger("65536"), 0x00010000);
        testWidenedResultBigInt(new BigInteger("65537"), 0x00010001);

        testWidenedResultBigInt(new BigInteger("131071"), 0x0001FFFF);
        testWidenedResultBigInt(new BigInteger("131072"), 0x00020000);
        testWidenedResultBigInt(new BigInteger("131073"), 0x00020001);

        testWidenedResultBigInt(new BigInteger("262143"), 0x0003FFFF);
        testWidenedResultBigInt(new BigInteger("262144"), 0x00040000);
        testWidenedResultBigInt(new BigInteger("262145"), 0x00040001);

        testWidenedResultBigInt(new BigInteger("524287"), 0x0007FFFF);
        testWidenedResultBigInt(new BigInteger("524288"), 0x00080000);
        testWidenedResultBigInt(new BigInteger("524289"), 0x00080001);

        testWidenedResultBigInt(new BigInteger("1048575"), 0x000FFFFF);
        testWidenedResultBigInt(new BigInteger("1048576"), 0x00100000);
        testWidenedResultBigInt(new BigInteger("1048577"), 0x00100001);

        testWidenedResultBigInt(new BigInteger("2097151"), 0x001FFFFF);
        testWidenedResultBigInt(new BigInteger("2097152"), 0x00200000);
        testWidenedResultBigInt(new BigInteger("2097153"), 0x00200001);

        testWidenedResultBigInt(new BigInteger("4194303"), 0x003FFFFF);
        testWidenedResultBigInt(new BigInteger("4194304"), 0x00400000);
        testWidenedResultBigInt(new BigInteger("4194305"), 0x00400001);

        testWidenedResultBigInt(new BigInteger("8388607"), 0x007FFFFF);
        testWidenedResultBigInt(new BigInteger("8388608"), 0x00800000);
        testWidenedResultBigInt(new BigInteger("8388609"), 0x00800001);

        testWidenedResultBigInt(new BigInteger("16777215"), 0x00FFFFFF);
        testWidenedResultBigInt(new BigInteger("16777216"), 0x01000000);
        testWidenedResultBigInt(new BigInteger("16777217"), 0x01000001);

        testWidenedResultBigInt(new BigInteger("33554431"), 0x01FFFFFF);
        testWidenedResultBigInt(new BigInteger("33554432"), 0x02000000);
        testWidenedResultBigInt(new BigInteger("33554433"), 0x02000001);

        testWidenedResultBigInt(new BigInteger("67108863"), 0x03FFFFFF);
        testWidenedResultBigInt(new BigInteger("67108864"), 0x04000000);
        testWidenedResultBigInt(new BigInteger("67108865"), 0x04000001);

        testWidenedResultBigInt(new BigInteger("134217727"), 0x07FFFFFF);
        testWidenedResultBigInt(new BigInteger("134217728"), 0x08000000);
        testWidenedResultBigInt(new BigInteger("134217729"), 0x08000001);

        testWidenedResultBigInt(new BigInteger("268435455"), 0x0FFFFFFF);
        testWidenedResultBigInt(new BigInteger("268435456"), 0x10000000);
        testWidenedResultBigInt(new BigInteger("268435457"), 0x10000001);

        testWidenedResultBigInt(new BigInteger("536870911"), 0x1FFFFFFF);
        testWidenedResultBigInt(new BigInteger("536870912"), 0x20000000);
        testWidenedResultBigInt(new BigInteger("536870913"), 0x20000001);

        testWidenedResultBigInt(new BigInteger("1073741823"), 0x3FFFFFFF);
        testWidenedResultBigInt(new BigInteger("1073741824"), 0x40000000);
        testWidenedResultBigInt(new BigInteger("1073741825"), 0x40000001);

        testWidenedResultBigInt(new BigInteger("2147483647"), 0x7FFFFFFF);
        testWidenedResultBigInt(new BigInteger("2147483648"), 0x80000000);
        testWidenedResultBigInt(new BigInteger("2147483649"), 0x80000001);

        testWidenedResultBigInt(new BigInteger("4294967295"), 0xFFFFFFFF);
    }

    @Test
    public void asUnsignedBigInt_Long() {
        testWidenedResultBigInt(new BigInteger("0"), 0x0000000000000000L);

        testWidenedResultBigInt(new BigInteger("1"), 0x0000000000000001L);

        testWidenedResultBigInt(new BigInteger("2"), 0x0000000000000002L);
        testWidenedResultBigInt(new BigInteger("3"), 0x0000000000000003L);

        testWidenedResultBigInt(new BigInteger("4"), 0x0000000000000004L);
        testWidenedResultBigInt(new BigInteger("5"), 0x0000000000000005L);

        testWidenedResultBigInt(new BigInteger("7"), 0x0000000000000007L);
        testWidenedResultBigInt(new BigInteger("8"), 0x0000000000000008L);
        testWidenedResultBigInt(new BigInteger("9"), 0x0000000000000009L);

        testWidenedResultBigInt(new BigInteger("15"), 0x000000000000000FL);
        testWidenedResultBigInt(new BigInteger("16"), 0x0000000000000010L);
        testWidenedResultBigInt(new BigInteger("17"), 0x0000000000000011L);

        testWidenedResultBigInt(new BigInteger("31"), 0x000000000000001FL);
        testWidenedResultBigInt(new BigInteger("32"), 0x0000000000000020L);
        testWidenedResultBigInt(new BigInteger("33"), 0x0000000000000021L);

        testWidenedResultBigInt(new BigInteger("63"), 0x000000000000003FL);
        testWidenedResultBigInt(new BigInteger("64"), 0x0000000000000040L);
        testWidenedResultBigInt(new BigInteger("65"), 0x0000000000000041L);

        testWidenedResultBigInt(new BigInteger("127"), 0x000000000000007FL);
        testWidenedResultBigInt(new BigInteger("128"), 0x0000000000000080L);
        testWidenedResultBigInt(new BigInteger("129"), 0x0000000000000081L);

        testWidenedResultBigInt(new BigInteger("255"), 0x00000000000000FFL);
        testWidenedResultBigInt(new BigInteger("256"), 0x0000000000000100L);
        testWidenedResultBigInt(new BigInteger("257"), 0x0000000000000101L);

        testWidenedResultBigInt(new BigInteger("511"), 0x00000000000001FFL);
        testWidenedResultBigInt(new BigInteger("512"), 0x0000000000000200L);
        testWidenedResultBigInt(new BigInteger("513"), 0x0000000000000201L);

        testWidenedResultBigInt(new BigInteger("1023"), 0x00000000000003FFL);
        testWidenedResultBigInt(new BigInteger("1024"), 0x0000000000000400L);
        testWidenedResultBigInt(new BigInteger("1025"), 0x0000000000000401L);

        testWidenedResultBigInt(new BigInteger("2047"), 0x00000000000007FFL);
        testWidenedResultBigInt(new BigInteger("2048"), 0x0000000000000800L);
        testWidenedResultBigInt(new BigInteger("2049"), 0x0000000000000801L);

        testWidenedResultBigInt(new BigInteger("4095"), 0x0000000000000FFFL);
        testWidenedResultBigInt(new BigInteger("4096"), 0x0000000000001000L);
        testWidenedResultBigInt(new BigInteger("4097"), 0x0000000000001001L);

        testWidenedResultBigInt(new BigInteger("8191"), 0x0000000000001FFFL);
        testWidenedResultBigInt(new BigInteger("8192"), 0x0000000000002000L);
        testWidenedResultBigInt(new BigInteger("8193"), 0x0000000000002001L);

        testWidenedResultBigInt(new BigInteger("16383"), 0x0000000000003FFFL);
        testWidenedResultBigInt(new BigInteger("16384"), 0x0000000000004000L);
        testWidenedResultBigInt(new BigInteger("16385"), 0x0000000000004001L);

        testWidenedResultBigInt(new BigInteger("32767"), 0x0000000000007FFFL);
        testWidenedResultBigInt(new BigInteger("32768"), 0x0000000000008000L);
        testWidenedResultBigInt(new BigInteger("32769"), 0x0000000000008001L);

        testWidenedResultBigInt(new BigInteger("65535"), 0x000000000000FFFFL);
        testWidenedResultBigInt(new BigInteger("65536"), 0x0000000000010000L);
        testWidenedResultBigInt(new BigInteger("65537"), 0x0000000000010001L);

        testWidenedResultBigInt(new BigInteger("131071"), 0x000000000001FFFFL);
        testWidenedResultBigInt(new BigInteger("131072"), 0x0000000000020000L);
        testWidenedResultBigInt(new BigInteger("131073"), 0x0000000000020001L);

        testWidenedResultBigInt(new BigInteger("262143"), 0x000000000003FFFFL);
        testWidenedResultBigInt(new BigInteger("262144"), 0x0000000000040000L);
        testWidenedResultBigInt(new BigInteger("262145"), 0x0000000000040001L);

        testWidenedResultBigInt(new BigInteger("524287"), 0x000000000007FFFFL);
        testWidenedResultBigInt(new BigInteger("524288"), 0x0000000000080000L);
        testWidenedResultBigInt(new BigInteger("524289"), 0x0000000000080001L);

        testWidenedResultBigInt(new BigInteger("1048575"), 0x00000000000FFFFFL);
        testWidenedResultBigInt(new BigInteger("1048576"), 0x0000000000100000L);
        testWidenedResultBigInt(new BigInteger("1048577"), 0x0000000000100001L);

        testWidenedResultBigInt(new BigInteger("2097151"), 0x00000000001FFFFFL);
        testWidenedResultBigInt(new BigInteger("2097152"), 0x0000000000200000L);
        testWidenedResultBigInt(new BigInteger("2097153"), 0x0000000000200001L);

        testWidenedResultBigInt(new BigInteger("4194303"), 0x00000000003FFFFFL);
        testWidenedResultBigInt(new BigInteger("4194304"), 0x0000000000400000L);
        testWidenedResultBigInt(new BigInteger("4194305"), 0x0000000000400001L);

        testWidenedResultBigInt(new BigInteger("8388607"), 0x00000000007FFFFFL);
        testWidenedResultBigInt(new BigInteger("8388608"), 0x0000000000800000L);
        testWidenedResultBigInt(new BigInteger("8388609"), 0x0000000000800001L);

        testWidenedResultBigInt(new BigInteger("16777215"), 0x0000000000FFFFFFL);
        testWidenedResultBigInt(new BigInteger("16777216"), 0x0000000001000000L);
        testWidenedResultBigInt(new BigInteger("16777217"), 0x0000000001000001L);

        testWidenedResultBigInt(new BigInteger("33554431"), 0x0000000001FFFFFFL);
        testWidenedResultBigInt(new BigInteger("33554432"), 0x0000000002000000L);
        testWidenedResultBigInt(new BigInteger("33554433"), 0x0000000002000001L);

        testWidenedResultBigInt(new BigInteger("67108863"), 0x0000000003FFFFFFL);
        testWidenedResultBigInt(new BigInteger("67108864"), 0x0000000004000000L);
        testWidenedResultBigInt(new BigInteger("67108865"), 0x0000000004000001L);

        testWidenedResultBigInt(new BigInteger("134217727"), 0x0000000007FFFFFFL);
        testWidenedResultBigInt(new BigInteger("134217728"), 0x0000000008000000L);
        testWidenedResultBigInt(new BigInteger("134217729"), 0x0000000008000001L);

        testWidenedResultBigInt(new BigInteger("268435455"), 0x000000000FFFFFFFL);
        testWidenedResultBigInt(new BigInteger("268435456"), 0x0000000010000000L);
        testWidenedResultBigInt(new BigInteger("268435457"), 0x0000000010000001L);

        testWidenedResultBigInt(new BigInteger("536870911"), 0x000000001FFFFFFFL);
        testWidenedResultBigInt(new BigInteger("536870912"), 0x0000000020000000L);
        testWidenedResultBigInt(new BigInteger("536870913"), 0x0000000020000001L);

        testWidenedResultBigInt(new BigInteger("1073741823"), 0x000000003FFFFFFFL);
        testWidenedResultBigInt(new BigInteger("1073741824"), 0x0000000040000000L);
        testWidenedResultBigInt(new BigInteger("1073741825"), 0x0000000040000001L);

        testWidenedResultBigInt(new BigInteger("2147483647"), 0x000000007FFFFFFFL);
        testWidenedResultBigInt(new BigInteger("2147483648"), 0x0000000080000000L);
        testWidenedResultBigInt(new BigInteger("2147483649"), 0x0000000080000001L);

        testWidenedResultBigInt(new BigInteger("4294967295"), 0x00000000FFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("4294967296"), 0x0000000100000000L);
        testWidenedResultBigInt(new BigInteger("4294967297"), 0x0000000100000001L);

        testWidenedResultBigInt(new BigInteger("8589934591"), 0x00000001FFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("8589934592"), 0x0000000200000000L);
        testWidenedResultBigInt(new BigInteger("8589934593"), 0x0000000200000001L);

        testWidenedResultBigInt(new BigInteger("17179869183"), 0x00000003FFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("17179869184"), 0x0000000400000000L);
        testWidenedResultBigInt(new BigInteger("17179869185"), 0x0000000400000001L);

        testWidenedResultBigInt(new BigInteger("34359738367"), 0x00000007FFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("34359738368"), 0x0000000800000000L);
        testWidenedResultBigInt(new BigInteger("34359738369"), 0x0000000800000001L);

        testWidenedResultBigInt(new BigInteger("68719476735"), 0x0000000FFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("68719476736"), 0x0000001000000000L);
        testWidenedResultBigInt(new BigInteger("68719476737"), 0x0000001000000001L);

        testWidenedResultBigInt(new BigInteger("137438953471"), 0x0000001FFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("137438953472"), 0x0000002000000000L);
        testWidenedResultBigInt(new BigInteger("137438953473"), 0x0000002000000001L);

        testWidenedResultBigInt(new BigInteger("274877906943"), 0x0000003FFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("274877906944"), 0x0000004000000000L);
        testWidenedResultBigInt(new BigInteger("274877906945"), 0x0000004000000001L);

        testWidenedResultBigInt(new BigInteger("549755813887"), 0x0000007FFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("549755813888"), 0x0000008000000000L);
        testWidenedResultBigInt(new BigInteger("549755813889"), 0x0000008000000001L);

        testWidenedResultBigInt(new BigInteger("1099511627775"), 0x000000FFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("1099511627776"), 0x0000010000000000L);
        testWidenedResultBigInt(new BigInteger("1099511627777"), 0x0000010000000001L);

        testWidenedResultBigInt(new BigInteger("2199023255551"), 0x000001FFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("2199023255552"), 0x0000020000000000L);
        testWidenedResultBigInt(new BigInteger("2199023255553"), 0x0000020000000001L);

        testWidenedResultBigInt(new BigInteger("4398046511103"), 0x000003FFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("4398046511104"), 0x0000040000000000L);
        testWidenedResultBigInt(new BigInteger("4398046511105"), 0x0000040000000001L);

        testWidenedResultBigInt(new BigInteger("8796093022207"), 0x000007FFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("8796093022208"), 0x0000080000000000L);
        testWidenedResultBigInt(new BigInteger("8796093022209"), 0x0000080000000001L);

        testWidenedResultBigInt(new BigInteger("17592186044415"), 0x00000FFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("17592186044416"), 0x0000100000000000L);
        testWidenedResultBigInt(new BigInteger("17592186044417"), 0x0000100000000001L);

        testWidenedResultBigInt(new BigInteger("35184372088831"), 0x00001FFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("35184372088832"), 0x0000200000000000L);
        testWidenedResultBigInt(new BigInteger("35184372088833"), 0x0000200000000001L);

        testWidenedResultBigInt(new BigInteger("70368744177663"), 0x00003FFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("70368744177664"), 0x0000400000000000L);
        testWidenedResultBigInt(new BigInteger("70368744177665"), 0x0000400000000001L);

        testWidenedResultBigInt(new BigInteger("140737488355327"), 0x00007FFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("140737488355328"), 0x0000800000000000L);
        testWidenedResultBigInt(new BigInteger("140737488355329"), 0x0000800000000001L);

        testWidenedResultBigInt(new BigInteger("281474976710655"), 0x0000FFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("281474976710656"), 0x0001000000000000L);
        testWidenedResultBigInt(new BigInteger("281474976710657"), 0x0001000000000001L);

        testWidenedResultBigInt(new BigInteger("562949953421311"), 0x0001FFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("562949953421312"), 0x0002000000000000L);
        testWidenedResultBigInt(new BigInteger("562949953421313"), 0x0002000000000001L);

        testWidenedResultBigInt(new BigInteger("1125899906842623"), 0x0003FFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("1125899906842624"), 0x0004000000000000L);
        testWidenedResultBigInt(new BigInteger("1125899906842625"), 0x0004000000000001L);

        testWidenedResultBigInt(new BigInteger("2251799813685247"), 0x0007FFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("2251799813685248"), 0x0008000000000000L);
        testWidenedResultBigInt(new BigInteger("2251799813685249"), 0x0008000000000001L);

        testWidenedResultBigInt(new BigInteger("4503599627370495"), 0x000FFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("4503599627370496"), 0x0010000000000000L);
        testWidenedResultBigInt(new BigInteger("4503599627370497"), 0x0010000000000001L);

        testWidenedResultBigInt(new BigInteger("9007199254740991"), 0x001FFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("9007199254740992"), 0x0020000000000000L);
        testWidenedResultBigInt(new BigInteger("9007199254740993"), 0x0020000000000001L);

        testWidenedResultBigInt(new BigInteger("18014398509481983"), 0x003FFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("18014398509481984"), 0x0040000000000000L);
        testWidenedResultBigInt(new BigInteger("18014398509481985"), 0x0040000000000001L);

        testWidenedResultBigInt(new BigInteger("36028797018963967"), 0x007FFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("36028797018963968"), 0x0080000000000000L);
        testWidenedResultBigInt(new BigInteger("36028797018963969"), 0x0080000000000001L);

        testWidenedResultBigInt(new BigInteger("72057594037927935"), 0x00FFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("72057594037927936"), 0x0100000000000000L);
        testWidenedResultBigInt(new BigInteger("72057594037927937"), 0x0100000000000001L);

        testWidenedResultBigInt(new BigInteger("144115188075855871"), 0x01FFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("144115188075855872"), 0x0200000000000000L);
        testWidenedResultBigInt(new BigInteger("144115188075855873"), 0x0200000000000001L);

        testWidenedResultBigInt(new BigInteger("288230376151711743"), 0x03FFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("288230376151711744"), 0x0400000000000000L);
        testWidenedResultBigInt(new BigInteger("288230376151711745"), 0x0400000000000001L);

        testWidenedResultBigInt(new BigInteger("576460752303423487"), 0x07FFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("576460752303423488"), 0x0800000000000000L);
        testWidenedResultBigInt(new BigInteger("576460752303423489"), 0x0800000000000001L);

        testWidenedResultBigInt(new BigInteger("1152921504606846975"), 0x0FFFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("1152921504606846976"), 0x1000000000000000L);
        testWidenedResultBigInt(new BigInteger("1152921504606846977"), 0x1000000000000001L);

        testWidenedResultBigInt(new BigInteger("2305843009213693951"), 0x1FFFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("2305843009213693952"), 0x2000000000000000L);
        testWidenedResultBigInt(new BigInteger("2305843009213693953"), 0x2000000000000001L);

        testWidenedResultBigInt(new BigInteger("4611686018427387903"), 0x3FFFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("4611686018427387904"), 0x4000000000000000L);
        testWidenedResultBigInt(new BigInteger("4611686018427387905"), 0x4000000000000001L);

        testWidenedResultBigInt(new BigInteger("9223372036854775807"), 0x7FFFFFFFFFFFFFFFL);
        testWidenedResultBigInt(new BigInteger("9223372036854775808"), 0x8000000000000000L);
        testWidenedResultBigInt(new BigInteger("9223372036854775809"), 0x8000000000000001L);

        testWidenedResultBigInt(new BigInteger("18446744073709551615"), 0xFFFFFFFFFFFFFFFFL);
    }

    private static void testWidenedResultShort(short expectedWidenedValue, byte unsignedValue) {
        short actualWidenedValue = UnsignedHelper.asUnsignedShort(unsignedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue >= 0);
    }

    private static void testWidenedResultInt(int expectedWidenedValue, byte unsignedValue) {
        int actualWidenedValue = UnsignedHelper.asUnsignedInt(unsignedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue >= 0);
    }

    private static void testWidenedResultInt(int expectedWidenedValue, short unsignedValue) {
        int actualWidenedValue = UnsignedHelper.asUnsignedInt(unsignedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue >= 0);
    }

    private static void testWidenedResultLong(long expectedWidenedValue, byte unsignedValue) {
        long actualWidenedValue = UnsignedHelper.asUnsignedLong(unsignedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue >= 0);
    }

    private static void testWidenedResultLong(long expectedWidenedValue, short unsignedValue) {
        long actualWidenedValue = UnsignedHelper.asUnsignedLong(unsignedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue >= 0);
    }

    private static void testWidenedResultLong(long expectedWidenedValue, int unsignedValue) {
        long actualWidenedValue = UnsignedHelper.asUnsignedLong(unsignedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue >= 0);
    }

    private static void testWidenedResultBigInt(BigInteger expectedWidenedValue, byte unsignedValue) {
        BigInteger actualWidenedValue = UnsignedHelper.asUnsignedBigInt(unsignedValue);
        assertNotNull(actualWidenedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue.signum() >= 0);
    }

    private static void testWidenedResultBigInt(BigInteger expectedWidenedValue, short unsignedValue) {
        BigInteger actualWidenedValue = UnsignedHelper.asUnsignedBigInt(unsignedValue);
        assertNotNull(actualWidenedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue.signum() >= 0);
    }

    private static void testWidenedResultBigInt(BigInteger expectedWidenedValue, int unsignedValue) {
        BigInteger actualWidenedValue = UnsignedHelper.asUnsignedBigInt(unsignedValue);
        assertNotNull(actualWidenedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue.signum() >= 0);
    }

    private static void testWidenedResultBigInt(BigInteger expectedWidenedValue, long unsignedValue) {
        BigInteger actualWidenedValue = UnsignedHelper.asUnsignedBigInt(unsignedValue);
        assertNotNull(actualWidenedValue);
        assertEquals(expectedWidenedValue, actualWidenedValue);
        assertTrue("Result must be non-negative.", actualWidenedValue.signum() >= 0);
    }
}
