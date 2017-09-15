// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import static org.junit.Assert.*;

public class ZigzagHelperTest {

    @Test
    public void staticClass() {
        TestHelper.verifyStaticHelperClass(ZigzagHelper.class);
    }

    @Test
    public void testZigzag16Encoding() {
        testEncodedDecodedTuple16((short) 0, (short) 0x0000);

        testEncodedDecodedTuple16((short) -1, (short) 0x0001);
        testEncodedDecodedTuple16((short) 1, (short) 0x0002);

        testEncodedDecodedTuple16((short) -2, (short) 0x0003);
        testEncodedDecodedTuple16((short) 2, (short) 0x0004);
        testEncodedDecodedTuple16((short) -3, (short) 0x0005);
        testEncodedDecodedTuple16((short) 3, (short) 0x0006);

        testEncodedDecodedTuple16((short) -4, (short) 0x0007);
        testEncodedDecodedTuple16((short) 4, (short) 0x0008);
        testEncodedDecodedTuple16((short) -5, (short) 0x0009);
        testEncodedDecodedTuple16((short) 5, (short) 0x000A);

        testEncodedDecodedTuple16((short) -7, (short) 0x000D);
        testEncodedDecodedTuple16((short) 7, (short) 0x000E);
        testEncodedDecodedTuple16((short) -8, (short) 0x000F);
        testEncodedDecodedTuple16((short) 8, (short) 0x0010);
        testEncodedDecodedTuple16((short) -9, (short) 0x0011);
        testEncodedDecodedTuple16((short) 9, (short) 0x0012);

        testEncodedDecodedTuple16((short) -15, (short) 0x001D);
        testEncodedDecodedTuple16((short) 15, (short) 0x001E);
        testEncodedDecodedTuple16((short) -16, (short) 0x001F);
        testEncodedDecodedTuple16((short) 16, (short) 0x0020);
        testEncodedDecodedTuple16((short) -17, (short) 0x0021);
        testEncodedDecodedTuple16((short) 17, (short) 0x0022);

        testEncodedDecodedTuple16((short) -31, (short) 0x003D);
        testEncodedDecodedTuple16((short) 31, (short) 0x003E);
        testEncodedDecodedTuple16((short) -32, (short) 0x003F);
        testEncodedDecodedTuple16((short) 32, (short) 0x0040);
        testEncodedDecodedTuple16((short) -33, (short) 0x0041);
        testEncodedDecodedTuple16((short) 33, (short) 0x0042);

        testEncodedDecodedTuple16((short) -63, (short) 0x007D);
        testEncodedDecodedTuple16((short) 63, (short) 0x007E);
        testEncodedDecodedTuple16((short) -64, (short) 0x007F);
        testEncodedDecodedTuple16((short) 64, (short) 0x0080);
        testEncodedDecodedTuple16((short) -65, (short) 0x0081);
        testEncodedDecodedTuple16((short) 65, (short) 0x0082);

        testEncodedDecodedTuple16((short) -127, (short) 0x00FD);
        testEncodedDecodedTuple16((short) 127, (short) 0x00FE);
        testEncodedDecodedTuple16((short) -128, (short) 0x00FF);
        testEncodedDecodedTuple16((short) 128, (short) 0x0100);
        testEncodedDecodedTuple16((short) -129, (short) 0x0101);
        testEncodedDecodedTuple16((short) 129, (short) 0x0102);

        testEncodedDecodedTuple16((short) -255, (short) 0x01FD);
        testEncodedDecodedTuple16((short) 255, (short) 0x01FE);
        testEncodedDecodedTuple16((short) -256, (short) 0x01FF);
        testEncodedDecodedTuple16((short) 256, (short) 0x0200);
        testEncodedDecodedTuple16((short) -257, (short) 0x0201);
        testEncodedDecodedTuple16((short) 257, (short) 0x0202);

        testEncodedDecodedTuple16((short) -511, (short) 0x03FD);
        testEncodedDecodedTuple16((short) 511, (short) 0x03FE);
        testEncodedDecodedTuple16((short) -512, (short) 0x03FF);
        testEncodedDecodedTuple16((short) 512, (short) 0x0400);
        testEncodedDecodedTuple16((short) -513, (short) 0x0401);
        testEncodedDecodedTuple16((short) 513, (short) 0x0402);

        testEncodedDecodedTuple16((short) -1023, (short) 0x07FD);
        testEncodedDecodedTuple16((short) 1023, (short) 0x07FE);
        testEncodedDecodedTuple16((short) -1024, (short) 0x07FF);
        testEncodedDecodedTuple16((short) 1024, (short) 0x0800);
        testEncodedDecodedTuple16((short) -1025, (short) 0x0801);
        testEncodedDecodedTuple16((short) 1025, (short) 0x0802);

        testEncodedDecodedTuple16((short) -2047, (short) 0x0FFD);
        testEncodedDecodedTuple16((short) 2047, (short) 0x0FFE);
        testEncodedDecodedTuple16((short) -2048, (short) 0x0FFF);
        testEncodedDecodedTuple16((short) 2048, (short) 0x1000);
        testEncodedDecodedTuple16((short) -2049, (short) 0x1001);
        testEncodedDecodedTuple16((short) 2049, (short) 0x1002);

        testEncodedDecodedTuple16((short) -4095, (short) 0x1FFD);
        testEncodedDecodedTuple16((short) 4095, (short) 0x1FFE);
        testEncodedDecodedTuple16((short) -4096, (short) 0x1FFF);
        testEncodedDecodedTuple16((short) 4096, (short) 0x2000);
        testEncodedDecodedTuple16((short) -4097, (short) 0x2001);
        testEncodedDecodedTuple16((short) 4097, (short) 0x2002);

        testEncodedDecodedTuple16((short) -8191, (short) 0x3FFD);
        testEncodedDecodedTuple16((short) 8191, (short) 0x3FFE);
        testEncodedDecodedTuple16((short) -8192, (short) 0x3FFF);
        testEncodedDecodedTuple16((short) 8192, (short) 0x4000);
        testEncodedDecodedTuple16((short) -8193, (short) 0x4001);
        testEncodedDecodedTuple16((short) 8193, (short) 0x4002);

        testEncodedDecodedTuple16((short) -16383, (short) 0x7FFD);
        testEncodedDecodedTuple16((short) 16383, (short) 0x7FFE);
        testEncodedDecodedTuple16((short) -16384, (short) 0x7FFF);
        testEncodedDecodedTuple16((short) 16384, (short) 0x8000);
        testEncodedDecodedTuple16((short) -16385, (short) 0x8001);
        testEncodedDecodedTuple16((short) 16385, (short) 0x8002);

        testEncodedDecodedTuple16((short) -32767, (short) 0xFFFD);
        testEncodedDecodedTuple16((short) 32767, (short) 0xFFFE);
        testEncodedDecodedTuple16((short) -32768, (short) 0xFFFF);
    }

    @Test
    public void testZigzag32Encoding() {
        testEncodedDecodedTuple32(0, 0x00000000);

        testEncodedDecodedTuple32(-1, 0x00000001);
        testEncodedDecodedTuple32(1, 0x00000002);

        testEncodedDecodedTuple32(-2, 0x00000003);
        testEncodedDecodedTuple32(2, 0x00000004);
        testEncodedDecodedTuple32(-3, 0x00000005);
        testEncodedDecodedTuple32(3, 0x00000006);

        testEncodedDecodedTuple32(-4, 0x00000007);
        testEncodedDecodedTuple32(4, 0x00000008);
        testEncodedDecodedTuple32(-5, 0x00000009);
        testEncodedDecodedTuple32(5, 0x0000000A);

        testEncodedDecodedTuple32(-7, 0x0000000D);
        testEncodedDecodedTuple32(7, 0x0000000E);
        testEncodedDecodedTuple32(-8, 0x0000000F);
        testEncodedDecodedTuple32(8, 0x00000010);
        testEncodedDecodedTuple32(-9, 0x00000011);
        testEncodedDecodedTuple32(9, 0x00000012);

        testEncodedDecodedTuple32(-15, 0x0000001D);
        testEncodedDecodedTuple32(15, 0x0000001E);
        testEncodedDecodedTuple32(-16, 0x0000001F);
        testEncodedDecodedTuple32(16, 0x00000020);
        testEncodedDecodedTuple32(-17, 0x00000021);
        testEncodedDecodedTuple32(17, 0x00000022);

        testEncodedDecodedTuple32(-31, 0x0000003D);
        testEncodedDecodedTuple32(31, 0x0000003E);
        testEncodedDecodedTuple32(-32, 0x0000003F);
        testEncodedDecodedTuple32(32, 0x00000040);
        testEncodedDecodedTuple32(-33, 0x00000041);
        testEncodedDecodedTuple32(33, 0x00000042);

        testEncodedDecodedTuple32(-63, 0x0000007D);
        testEncodedDecodedTuple32(63, 0x0000007E);
        testEncodedDecodedTuple32(-64, 0x0000007F);
        testEncodedDecodedTuple32(64, 0x00000080);
        testEncodedDecodedTuple32(-65, 0x00000081);
        testEncodedDecodedTuple32(65, 0x00000082);

        testEncodedDecodedTuple32(-127, 0x000000FD);
        testEncodedDecodedTuple32(127, 0x000000FE);
        testEncodedDecodedTuple32(-128, 0x000000FF);
        testEncodedDecodedTuple32(128, 0x00000100);
        testEncodedDecodedTuple32(-129, 0x00000101);
        testEncodedDecodedTuple32(129, 0x00000102);

        testEncodedDecodedTuple32(-255, 0x000001FD);
        testEncodedDecodedTuple32(255, 0x000001FE);
        testEncodedDecodedTuple32(-256, 0x000001FF);
        testEncodedDecodedTuple32(256, 0x00000200);
        testEncodedDecodedTuple32(-257, 0x00000201);
        testEncodedDecodedTuple32(257, 0x00000202);

        testEncodedDecodedTuple32(-511, 0x000003FD);
        testEncodedDecodedTuple32(511, 0x000003FE);
        testEncodedDecodedTuple32(-512, 0x000003FF);
        testEncodedDecodedTuple32(512, 0x00000400);
        testEncodedDecodedTuple32(-513, 0x00000401);
        testEncodedDecodedTuple32(513, 0x00000402);

        testEncodedDecodedTuple32(-1023, 0x000007FD);
        testEncodedDecodedTuple32(1023, 0x000007FE);
        testEncodedDecodedTuple32(-1024, 0x000007FF);
        testEncodedDecodedTuple32(1024, 0x00000800);
        testEncodedDecodedTuple32(-1025, 0x00000801);
        testEncodedDecodedTuple32(1025, 0x00000802);

        testEncodedDecodedTuple32(-2047, 0x00000FFD);
        testEncodedDecodedTuple32(2047, 0x00000FFE);
        testEncodedDecodedTuple32(-2048, 0x00000FFF);
        testEncodedDecodedTuple32(2048, 0x00001000);
        testEncodedDecodedTuple32(-2049, 0x00001001);
        testEncodedDecodedTuple32(2049, 0x00001002);

        testEncodedDecodedTuple32(-4095, 0x00001FFD);
        testEncodedDecodedTuple32(4095, 0x00001FFE);
        testEncodedDecodedTuple32(-4096, 0x00001FFF);
        testEncodedDecodedTuple32(4096, 0x00002000);
        testEncodedDecodedTuple32(-4097, 0x00002001);
        testEncodedDecodedTuple32(4097, 0x00002002);

        testEncodedDecodedTuple32(-8191, 0x00003FFD);
        testEncodedDecodedTuple32(8191, 0x00003FFE);
        testEncodedDecodedTuple32(-8192, 0x00003FFF);
        testEncodedDecodedTuple32(8192, 0x00004000);
        testEncodedDecodedTuple32(-8193, 0x00004001);
        testEncodedDecodedTuple32(8193, 0x00004002);

        testEncodedDecodedTuple32(-16383, 0x00007FFD);
        testEncodedDecodedTuple32(16383, 0x00007FFE);
        testEncodedDecodedTuple32(-16384, 0x00007FFF);
        testEncodedDecodedTuple32(16384, 0x00008000);
        testEncodedDecodedTuple32(-16385, 0x00008001);
        testEncodedDecodedTuple32(16385, 0x00008002);

        testEncodedDecodedTuple32(-32767, 0x0000FFFD);
        testEncodedDecodedTuple32(32767, 0x0000FFFE);
        testEncodedDecodedTuple32(-32768, 0x0000FFFF);
        testEncodedDecodedTuple32(32768, 0x00010000);
        testEncodedDecodedTuple32(-32769, 0x00010001);
        testEncodedDecodedTuple32(32769, 0x00010002);

        testEncodedDecodedTuple32(-65535, 0x0001FFFD);
        testEncodedDecodedTuple32(65535, 0x0001FFFE);
        testEncodedDecodedTuple32(-65536, 0x0001FFFF);
        testEncodedDecodedTuple32(65536, 0x00020000);
        testEncodedDecodedTuple32(-65537, 0x00020001);
        testEncodedDecodedTuple32(65537, 0x00020002);

        testEncodedDecodedTuple32(-131071, 0x0003FFFD);
        testEncodedDecodedTuple32(131071, 0x0003FFFE);
        testEncodedDecodedTuple32(-131072, 0x0003FFFF);
        testEncodedDecodedTuple32(131072, 0x00040000);
        testEncodedDecodedTuple32(-131073, 0x00040001);
        testEncodedDecodedTuple32(131073, 0x00040002);

        testEncodedDecodedTuple32(-262143, 0x0007FFFD);
        testEncodedDecodedTuple32(262143, 0x0007FFFE);
        testEncodedDecodedTuple32(-262144, 0x0007FFFF);
        testEncodedDecodedTuple32(262144, 0x00080000);
        testEncodedDecodedTuple32(-262145, 0x00080001);
        testEncodedDecodedTuple32(262145, 0x00080002);

        testEncodedDecodedTuple32(-524287, 0x000FFFFD);
        testEncodedDecodedTuple32(524287, 0x000FFFFE);
        testEncodedDecodedTuple32(-524288, 0x000FFFFF);
        testEncodedDecodedTuple32(524288, 0x00100000);
        testEncodedDecodedTuple32(-524289, 0x00100001);
        testEncodedDecodedTuple32(524289, 0x00100002);

        testEncodedDecodedTuple32(-1048575, 0x001FFFFD);
        testEncodedDecodedTuple32(1048575, 0x001FFFFE);
        testEncodedDecodedTuple32(-1048576, 0x001FFFFF);
        testEncodedDecodedTuple32(1048576, 0x00200000);
        testEncodedDecodedTuple32(-1048577, 0x00200001);
        testEncodedDecodedTuple32(1048577, 0x00200002);

        testEncodedDecodedTuple32(-2097151, 0x003FFFFD);
        testEncodedDecodedTuple32(2097151, 0x003FFFFE);
        testEncodedDecodedTuple32(-2097152, 0x003FFFFF);
        testEncodedDecodedTuple32(2097152, 0x00400000);
        testEncodedDecodedTuple32(-2097153, 0x00400001);
        testEncodedDecodedTuple32(2097153, 0x00400002);

        testEncodedDecodedTuple32(-4194303, 0x007FFFFD);
        testEncodedDecodedTuple32(4194303, 0x007FFFFE);
        testEncodedDecodedTuple32(-4194304, 0x007FFFFF);
        testEncodedDecodedTuple32(4194304, 0x00800000);
        testEncodedDecodedTuple32(-4194305, 0x00800001);
        testEncodedDecodedTuple32(4194305, 0x00800002);

        testEncodedDecodedTuple32(-8388607, 0x00FFFFFD);
        testEncodedDecodedTuple32(8388607, 0x00FFFFFE);
        testEncodedDecodedTuple32(-8388608, 0x00FFFFFF);
        testEncodedDecodedTuple32(8388608, 0x01000000);
        testEncodedDecodedTuple32(-8388609, 0x01000001);
        testEncodedDecodedTuple32(8388609, 0x01000002);

        testEncodedDecodedTuple32(-16777215, 0x01FFFFFD);
        testEncodedDecodedTuple32(16777215, 0x01FFFFFE);
        testEncodedDecodedTuple32(-16777216, 0x01FFFFFF);
        testEncodedDecodedTuple32(16777216, 0x02000000);
        testEncodedDecodedTuple32(-16777217, 0x02000001);
        testEncodedDecodedTuple32(16777217, 0x02000002);

        testEncodedDecodedTuple32(-33554431, 0x03FFFFFD);
        testEncodedDecodedTuple32(33554431, 0x03FFFFFE);
        testEncodedDecodedTuple32(-33554432, 0x03FFFFFF);
        testEncodedDecodedTuple32(33554432, 0x04000000);
        testEncodedDecodedTuple32(-33554433, 0x04000001);
        testEncodedDecodedTuple32(33554433, 0x04000002);

        testEncodedDecodedTuple32(-67108863, 0x07FFFFFD);
        testEncodedDecodedTuple32(67108863, 0x07FFFFFE);
        testEncodedDecodedTuple32(-67108864, 0x07FFFFFF);
        testEncodedDecodedTuple32(67108864, 0x08000000);
        testEncodedDecodedTuple32(-67108865, 0x08000001);
        testEncodedDecodedTuple32(67108865, 0x08000002);

        testEncodedDecodedTuple32(-134217727, 0x0FFFFFFD);
        testEncodedDecodedTuple32(134217727, 0x0FFFFFFE);
        testEncodedDecodedTuple32(-134217728, 0x0FFFFFFF);
        testEncodedDecodedTuple32(134217728, 0x10000000);
        testEncodedDecodedTuple32(-134217729, 0x10000001);
        testEncodedDecodedTuple32(134217729, 0x10000002);

        testEncodedDecodedTuple32(-268435455, 0x1FFFFFFD);
        testEncodedDecodedTuple32(268435455, 0x1FFFFFFE);
        testEncodedDecodedTuple32(-268435456, 0x1FFFFFFF);
        testEncodedDecodedTuple32(268435456, 0x20000000);
        testEncodedDecodedTuple32(-268435457, 0x20000001);
        testEncodedDecodedTuple32(268435457, 0x20000002);

        testEncodedDecodedTuple32(-536870911, 0x3FFFFFFD);
        testEncodedDecodedTuple32(536870911, 0x3FFFFFFE);
        testEncodedDecodedTuple32(-536870912, 0x3FFFFFFF);
        testEncodedDecodedTuple32(536870912, 0x40000000);
        testEncodedDecodedTuple32(-536870913, 0x40000001);
        testEncodedDecodedTuple32(536870913, 0x40000002);

        testEncodedDecodedTuple32(-1073741823, 0x7FFFFFFD);
        testEncodedDecodedTuple32(1073741823, 0x7FFFFFFE);
        testEncodedDecodedTuple32(-1073741824, 0x7FFFFFFF);
        testEncodedDecodedTuple32(1073741824, 0x80000000);
        testEncodedDecodedTuple32(-1073741825, 0x80000001);
        testEncodedDecodedTuple32(1073741825, 0x80000002);

        testEncodedDecodedTuple32(-2147483647, 0xFFFFFFFD);
        testEncodedDecodedTuple32(2147483647, 0xFFFFFFFE);
        testEncodedDecodedTuple32(-2147483648, 0xFFFFFFFF);
    }

    @Test
    public void testZigzag64Encoding() {
        testEncodedDecodedTuple64(0L, 0x0000000000000000L);

        testEncodedDecodedTuple64(-1, 0x0000000000000001L);
        testEncodedDecodedTuple64(1, 0x0000000000000002L);

        testEncodedDecodedTuple64(-2L, 0x0000000000000003L);
        testEncodedDecodedTuple64(2L, 0x0000000000000004L);
        testEncodedDecodedTuple64(-3L, 0x0000000000000005L);
        testEncodedDecodedTuple64(3L, 0x0000000000000006L);

        testEncodedDecodedTuple64(-4L, 0x0000000000000007L);
        testEncodedDecodedTuple64(4L, 0x0000000000000008L);
        testEncodedDecodedTuple64(-5L, 0x0000000000000009L);
        testEncodedDecodedTuple64(5L, 0x000000000000000AL);

        testEncodedDecodedTuple64(-7L, 0x0000000DL);
        testEncodedDecodedTuple64(7L, 0x0000000EL);
        testEncodedDecodedTuple64(-8L, 0x0000000FL);
        testEncodedDecodedTuple64(8L, 0x00000010L);
        testEncodedDecodedTuple64(-9L, 0x00000011L);
        testEncodedDecodedTuple64(9L, 0x00000012L);

        testEncodedDecodedTuple64(-15L, 0x0000001DL);
        testEncodedDecodedTuple64(15L, 0x0000001EL);
        testEncodedDecodedTuple64(-16L, 0x0000001FL);
        testEncodedDecodedTuple64(16L, 0x00000020L);
        testEncodedDecodedTuple64(-17L, 0x00000021L);
        testEncodedDecodedTuple64(17L, 0x00000022L);

        testEncodedDecodedTuple64(-31L, 0x0000003DL);
        testEncodedDecodedTuple64(31L, 0x0000003EL);
        testEncodedDecodedTuple64(-32L, 0x0000003FL);
        testEncodedDecodedTuple64(32L, 0x00000040L);
        testEncodedDecodedTuple64(-33L, 0x00000041L);
        testEncodedDecodedTuple64(33L, 0x00000042L);

        testEncodedDecodedTuple64(-63L, 0x0000007DL);
        testEncodedDecodedTuple64(63L, 0x0000007EL);
        testEncodedDecodedTuple64(-64L, 0x0000007FL);
        testEncodedDecodedTuple64(64L, 0x00000080L);
        testEncodedDecodedTuple64(-65L, 0x00000081L);
        testEncodedDecodedTuple64(65L, 0x00000082L);

        testEncodedDecodedTuple64(-127L, 0x000000FDL);
        testEncodedDecodedTuple64(127L, 0x000000FEL);
        testEncodedDecodedTuple64(-128L, 0x000000FFL);
        testEncodedDecodedTuple64(128L, 0x00000100L);
        testEncodedDecodedTuple64(-129L, 0x00000101L);
        testEncodedDecodedTuple64(129L, 0x00000102L);

        testEncodedDecodedTuple64(-255L, 0x000001FDL);
        testEncodedDecodedTuple64(255L, 0x000001FEL);
        testEncodedDecodedTuple64(-256L, 0x000001FFL);
        testEncodedDecodedTuple64(256L, 0x00000200L);
        testEncodedDecodedTuple64(-257L, 0x00000201L);
        testEncodedDecodedTuple64(257L, 0x00000202L);

        testEncodedDecodedTuple64(-511L, 0x000003FDL);
        testEncodedDecodedTuple64(511L, 0x000003FEL);
        testEncodedDecodedTuple64(-512L, 0x000003FFL);
        testEncodedDecodedTuple64(512L, 0x00000400L);
        testEncodedDecodedTuple64(-513L, 0x00000401L);
        testEncodedDecodedTuple64(513L, 0x00000402L);

        testEncodedDecodedTuple64(-1023L, 0x000007FDL);
        testEncodedDecodedTuple64(1023L, 0x000007FEL);
        testEncodedDecodedTuple64(-1024L, 0x000007FFL);
        testEncodedDecodedTuple64(1024L, 0x00000800L);
        testEncodedDecodedTuple64(-1025L, 0x00000801L);
        testEncodedDecodedTuple64(1025L, 0x00000802L);

        testEncodedDecodedTuple64(-2047L, 0x00000FFDL);
        testEncodedDecodedTuple64(2047L, 0x00000FFEL);
        testEncodedDecodedTuple64(-2048L, 0x00000FFFL);
        testEncodedDecodedTuple64(2048L, 0x00001000L);
        testEncodedDecodedTuple64(-2049L, 0x00001001L);
        testEncodedDecodedTuple64(2049L, 0x00001002L);

        testEncodedDecodedTuple64(-4095L, 0x00001FFDL);
        testEncodedDecodedTuple64(4095L, 0x00001FFEL);
        testEncodedDecodedTuple64(-4096L, 0x00001FFFL);
        testEncodedDecodedTuple64(4096L, 0x00002000L);
        testEncodedDecodedTuple64(-4097L, 0x00002001L);
        testEncodedDecodedTuple64(4097L, 0x00002002L);

        testEncodedDecodedTuple64(-8191L, 0x00003FFDL);
        testEncodedDecodedTuple64(8191L, 0x00003FFEL);
        testEncodedDecodedTuple64(-8192L, 0x00003FFFL);
        testEncodedDecodedTuple64(8192L, 0x00004000L);
        testEncodedDecodedTuple64(-8193L, 0x00004001L);
        testEncodedDecodedTuple64(8193L, 0x00004002L);

        testEncodedDecodedTuple64(-16383L, 0x00007FFDL);
        testEncodedDecodedTuple64(16383L, 0x00007FFEL);
        testEncodedDecodedTuple64(-16384L, 0x00007FFFL);
        testEncodedDecodedTuple64(16384L, 0x00008000L);
        testEncodedDecodedTuple64(-16385L, 0x00008001L);
        testEncodedDecodedTuple64(16385L, 0x00008002L);

        testEncodedDecodedTuple64(-32767L, 0x0000FFFDL);
        testEncodedDecodedTuple64(32767L, 0x0000FFFEL);
        testEncodedDecodedTuple64(-32768L, 0x0000FFFFL);
        testEncodedDecodedTuple64(32768L, 0x00010000L);
        testEncodedDecodedTuple64(-32769L, 0x00010001L);
        testEncodedDecodedTuple64(32769L, 0x00010002L);

        testEncodedDecodedTuple64(-65535L, 0x0001FFFDL);
        testEncodedDecodedTuple64(65535L, 0x0001FFFEL);
        testEncodedDecodedTuple64(-65536L, 0x0001FFFFL);
        testEncodedDecodedTuple64(65536L, 0x00020000L);
        testEncodedDecodedTuple64(-65537L, 0x00020001L);
        testEncodedDecodedTuple64(65537L, 0x00020002L);

        testEncodedDecodedTuple64(-131071L, 0x0003FFFDL);
        testEncodedDecodedTuple64(131071L, 0x0003FFFEL);
        testEncodedDecodedTuple64(-131072L, 0x0003FFFFL);
        testEncodedDecodedTuple64(131072L, 0x00040000L);
        testEncodedDecodedTuple64(-131073L, 0x00040001L);
        testEncodedDecodedTuple64(131073L, 0x00040002L);

        testEncodedDecodedTuple64(-262143L, 0x0007FFFDL);
        testEncodedDecodedTuple64(262143L, 0x0007FFFEL);
        testEncodedDecodedTuple64(-262144L, 0x0007FFFFL);
        testEncodedDecodedTuple64(262144L, 0x00080000L);
        testEncodedDecodedTuple64(-262145L, 0x00080001L);
        testEncodedDecodedTuple64(262145L, 0x00080002L);

        testEncodedDecodedTuple64(-524287L, 0x000FFFFDL);
        testEncodedDecodedTuple64(524287L, 0x000FFFFEL);
        testEncodedDecodedTuple64(-524288L, 0x000FFFFFL);
        testEncodedDecodedTuple64(524288L, 0x00100000L);
        testEncodedDecodedTuple64(-524289L, 0x00100001L);
        testEncodedDecodedTuple64(524289L, 0x00100002L);

        testEncodedDecodedTuple64(-1048575L, 0x001FFFFDL);
        testEncodedDecodedTuple64(1048575L, 0x001FFFFEL);
        testEncodedDecodedTuple64(-1048576L, 0x001FFFFFL);
        testEncodedDecodedTuple64(1048576L, 0x00200000L);
        testEncodedDecodedTuple64(-1048577L, 0x00200001L);
        testEncodedDecodedTuple64(1048577L, 0x00200002L);

        testEncodedDecodedTuple64(-2097151L, 0x003FFFFDL);
        testEncodedDecodedTuple64(2097151L, 0x003FFFFEL);
        testEncodedDecodedTuple64(-2097152L, 0x003FFFFFL);
        testEncodedDecodedTuple64(2097152L, 0x00400000L);
        testEncodedDecodedTuple64(-2097153L, 0x00400001L);
        testEncodedDecodedTuple64(2097153L, 0x00400002L);

        testEncodedDecodedTuple64(-4194303L, 0x007FFFFDL);
        testEncodedDecodedTuple64(4194303L, 0x007FFFFEL);
        testEncodedDecodedTuple64(-4194304L, 0x007FFFFFL);
        testEncodedDecodedTuple64(4194304L, 0x00800000L);
        testEncodedDecodedTuple64(-4194305L, 0x00800001L);
        testEncodedDecodedTuple64(4194305L, 0x00800002L);

        testEncodedDecodedTuple64(-8388607L, 0x00FFFFFDL);
        testEncodedDecodedTuple64(8388607L, 0x00FFFFFEL);
        testEncodedDecodedTuple64(-8388608L, 0x00FFFFFFL);
        testEncodedDecodedTuple64(8388608L, 0x01000000L);
        testEncodedDecodedTuple64(-8388609L, 0x01000001L);
        testEncodedDecodedTuple64(8388609L, 0x01000002L);

        testEncodedDecodedTuple64(-16777215L, 0x01FFFFFDL);
        testEncodedDecodedTuple64(16777215L, 0x01FFFFFEL);
        testEncodedDecodedTuple64(-16777216L, 0x01FFFFFFL);
        testEncodedDecodedTuple64(16777216L, 0x02000000L);
        testEncodedDecodedTuple64(-16777217L, 0x02000001L);
        testEncodedDecodedTuple64(16777217L, 0x02000002L);

        testEncodedDecodedTuple64(-33554431L, 0x03FFFFFDL);
        testEncodedDecodedTuple64(33554431L, 0x03FFFFFEL);
        testEncodedDecodedTuple64(-33554432L, 0x03FFFFFFL);
        testEncodedDecodedTuple64(33554432L, 0x04000000L);
        testEncodedDecodedTuple64(-33554433L, 0x04000001L);
        testEncodedDecodedTuple64(33554433L, 0x04000002L);

        testEncodedDecodedTuple64(-67108863L, 0x07FFFFFDL);
        testEncodedDecodedTuple64(67108863L, 0x07FFFFFEL);
        testEncodedDecodedTuple64(-67108864L, 0x07FFFFFFL);
        testEncodedDecodedTuple64(67108864L, 0x08000000L);
        testEncodedDecodedTuple64(-67108865L, 0x08000001L);
        testEncodedDecodedTuple64(67108865L, 0x08000002L);

        testEncodedDecodedTuple64(-134217727L, 0x0FFFFFFDL);
        testEncodedDecodedTuple64(134217727L, 0x0FFFFFFEL);
        testEncodedDecodedTuple64(-134217728L, 0x0FFFFFFFL);
        testEncodedDecodedTuple64(134217728L, 0x10000000L);
        testEncodedDecodedTuple64(-134217729L, 0x10000001L);
        testEncodedDecodedTuple64(134217729L, 0x10000002L);

        testEncodedDecodedTuple64(-268435455L, 0x1FFFFFFDL);
        testEncodedDecodedTuple64(268435455L, 0x1FFFFFFEL);
        testEncodedDecodedTuple64(-268435456L, 0x1FFFFFFFL);
        testEncodedDecodedTuple64(268435456L, 0x20000000L);
        testEncodedDecodedTuple64(-268435457L, 0x20000001L);
        testEncodedDecodedTuple64(268435457L, 0x20000002L);

        testEncodedDecodedTuple64(-536870911L, 0x3FFFFFFDL);
        testEncodedDecodedTuple64(536870911L, 0x3FFFFFFEL);
        testEncodedDecodedTuple64(-536870912L, 0x3FFFFFFFL);
        testEncodedDecodedTuple64(536870912L, 0x40000000L);
        testEncodedDecodedTuple64(-536870913L, 0x40000001L);
        testEncodedDecodedTuple64(536870913L, 0x40000002L);

        testEncodedDecodedTuple64(-1073741823L, 0x7FFFFFFDL);
        testEncodedDecodedTuple64(1073741823L, 0x7FFFFFFEL);
        testEncodedDecodedTuple64(-1073741824L, 0x7FFFFFFFL);
        testEncodedDecodedTuple64(1073741824L, 0x80000000L);
        testEncodedDecodedTuple64(-1073741825L, 0x80000001L);
        testEncodedDecodedTuple64(1073741825L, 0x80000002L);

        testEncodedDecodedTuple64(-2147483647L, 0xFFFFFFFDL);
        testEncodedDecodedTuple64(2147483647L, 0xFFFFFFFEL);
        testEncodedDecodedTuple64(-2147483648L, 0xFFFFFFFFL);
        testEncodedDecodedTuple64(2147483648L, 0x100000000L);
        testEncodedDecodedTuple64(-2147483649L, 0x100000001L);
        testEncodedDecodedTuple64(2147483649L, 0x100000002L);

        testEncodedDecodedTuple64(-4294967295L, 0x1FFFFFFFDL);
        testEncodedDecodedTuple64(4294967295L, 0x1FFFFFFFEL);
        testEncodedDecodedTuple64(-4294967296L, 0x1FFFFFFFFL);
        testEncodedDecodedTuple64(4294967296L, 0x200000000L);
        testEncodedDecodedTuple64(-4294967297L, 0x200000001L);
        testEncodedDecodedTuple64(4294967297L, 0x200000002L);

        testEncodedDecodedTuple64(-8589934591L, 0x3FFFFFFFDL);
        testEncodedDecodedTuple64(8589934591L, 0x3FFFFFFFEL);
        testEncodedDecodedTuple64(-8589934592L, 0x3FFFFFFFFL);
        testEncodedDecodedTuple64(8589934592L, 0x400000000L);
        testEncodedDecodedTuple64(-8589934593L, 0x400000001L);
        testEncodedDecodedTuple64(8589934593L, 0x400000002L);

        testEncodedDecodedTuple64(-17179869183L, 0x7FFFFFFFDL);
        testEncodedDecodedTuple64(17179869183L, 0x7FFFFFFFEL);
        testEncodedDecodedTuple64(-17179869184L, 0x7FFFFFFFFL);
        testEncodedDecodedTuple64(17179869184L, 0x800000000L);
        testEncodedDecodedTuple64(-17179869185L, 0x800000001L);
        testEncodedDecodedTuple64(17179869185L, 0x800000002L);

        testEncodedDecodedTuple64(-34359738367L, 0xFFFFFFFFDL);
        testEncodedDecodedTuple64(34359738367L, 0xFFFFFFFFEL);
        testEncodedDecodedTuple64(-34359738368L, 0xFFFFFFFFFL);
        testEncodedDecodedTuple64(34359738368L, 0x1000000000L);
        testEncodedDecodedTuple64(-34359738369L, 0x1000000001L);
        testEncodedDecodedTuple64(34359738369L, 0x1000000002L);

        testEncodedDecodedTuple64(-68719476735L, 0x1FFFFFFFFDL);
        testEncodedDecodedTuple64(68719476735L, 0x1FFFFFFFFEL);
        testEncodedDecodedTuple64(-68719476736L, 0x1FFFFFFFFFL);
        testEncodedDecodedTuple64(68719476736L, 0x2000000000L);
        testEncodedDecodedTuple64(-68719476737L, 0x2000000001L);
        testEncodedDecodedTuple64(68719476737L, 0x2000000002L);

        testEncodedDecodedTuple64(-137438953471L, 0x3FFFFFFFFDL);
        testEncodedDecodedTuple64(137438953471L, 0x3FFFFFFFFEL);
        testEncodedDecodedTuple64(-137438953472L, 0x3FFFFFFFFFL);
        testEncodedDecodedTuple64(137438953472L, 0x4000000000L);
        testEncodedDecodedTuple64(-137438953473L, 0x4000000001L);
        testEncodedDecodedTuple64(137438953473L, 0x4000000002L);

        testEncodedDecodedTuple64(-274877906943L, 0x7FFFFFFFFDL);
        testEncodedDecodedTuple64(274877906943L, 0x7FFFFFFFFEL);
        testEncodedDecodedTuple64(-274877906944L, 0x7FFFFFFFFFL);
        testEncodedDecodedTuple64(274877906944L, 0x8000000000L);
        testEncodedDecodedTuple64(-274877906945L, 0x8000000001L);
        testEncodedDecodedTuple64(274877906945L, 0x8000000002L);

        testEncodedDecodedTuple64(-549755813887L, 0xFFFFFFFFFDL);
        testEncodedDecodedTuple64(549755813887L, 0xFFFFFFFFFEL);
        testEncodedDecodedTuple64(-549755813888L, 0xFFFFFFFFFFL);
        testEncodedDecodedTuple64(549755813888L, 0x10000000000L);
        testEncodedDecodedTuple64(-549755813889L, 0x10000000001L);
        testEncodedDecodedTuple64(549755813889L, 0x10000000002L);

        testEncodedDecodedTuple64(-1099511627775L, 0x1FFFFFFFFFDL);
        testEncodedDecodedTuple64(1099511627775L, 0x1FFFFFFFFFEL);
        testEncodedDecodedTuple64(-1099511627776L, 0x1FFFFFFFFFFL);
        testEncodedDecodedTuple64(1099511627776L, 0x20000000000L);
        testEncodedDecodedTuple64(-1099511627777L, 0x20000000001L);
        testEncodedDecodedTuple64(1099511627777L, 0x20000000002L);

        testEncodedDecodedTuple64(-2199023255551L, 0x3FFFFFFFFFDL);
        testEncodedDecodedTuple64(2199023255551L, 0x3FFFFFFFFFEL);
        testEncodedDecodedTuple64(-2199023255552L, 0x3FFFFFFFFFFL);
        testEncodedDecodedTuple64(2199023255552L, 0x40000000000L);
        testEncodedDecodedTuple64(-2199023255553L, 0x40000000001L);
        testEncodedDecodedTuple64(2199023255553L, 0x40000000002L);

        testEncodedDecodedTuple64(-4398046511103L, 0x7FFFFFFFFFDL);
        testEncodedDecodedTuple64(4398046511103L, 0x7FFFFFFFFFEL);
        testEncodedDecodedTuple64(-4398046511104L, 0x7FFFFFFFFFFL);
        testEncodedDecodedTuple64(4398046511104L, 0x80000000000L);
        testEncodedDecodedTuple64(-4398046511105L, 0x80000000001L);
        testEncodedDecodedTuple64(4398046511105L, 0x80000000002L);

        testEncodedDecodedTuple64(-8796093022207L, 0xFFFFFFFFFFDL);
        testEncodedDecodedTuple64(8796093022207L, 0xFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-8796093022208L, 0xFFFFFFFFFFFL);
        testEncodedDecodedTuple64(8796093022208L, 0x100000000000L);
        testEncodedDecodedTuple64(-8796093022209L, 0x100000000001L);
        testEncodedDecodedTuple64(8796093022209L, 0x100000000002L);

        testEncodedDecodedTuple64(-17592186044415L, 0x1FFFFFFFFFFDL);
        testEncodedDecodedTuple64(17592186044415L, 0x1FFFFFFFFFFEL);
        testEncodedDecodedTuple64(-17592186044416L, 0x1FFFFFFFFFFFL);
        testEncodedDecodedTuple64(17592186044416L, 0x200000000000L);
        testEncodedDecodedTuple64(-17592186044417L, 0x200000000001L);
        testEncodedDecodedTuple64(17592186044417L, 0x200000000002L);

        testEncodedDecodedTuple64(-35184372088831L, 0x3FFFFFFFFFFDL);
        testEncodedDecodedTuple64(35184372088831L, 0x3FFFFFFFFFFEL);
        testEncodedDecodedTuple64(-35184372088832L, 0x3FFFFFFFFFFFL);
        testEncodedDecodedTuple64(35184372088832L, 0x400000000000L);
        testEncodedDecodedTuple64(-35184372088833L, 0x400000000001L);
        testEncodedDecodedTuple64(35184372088833L, 0x400000000002L);

        testEncodedDecodedTuple64(-70368744177663L, 0x7FFFFFFFFFFDL);
        testEncodedDecodedTuple64(70368744177663L, 0x7FFFFFFFFFFEL);
        testEncodedDecodedTuple64(-70368744177664L, 0x7FFFFFFFFFFFL);
        testEncodedDecodedTuple64(70368744177664L, 0x800000000000L);
        testEncodedDecodedTuple64(-70368744177665L, 0x800000000001L);
        testEncodedDecodedTuple64(70368744177665L, 0x800000000002L);

        testEncodedDecodedTuple64(-140737488355327L, 0xFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(140737488355327L, 0xFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-140737488355328L, 0xFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(140737488355328L, 0x1000000000000L);
        testEncodedDecodedTuple64(-140737488355329L, 0x1000000000001L);
        testEncodedDecodedTuple64(140737488355329L, 0x1000000000002L);

        testEncodedDecodedTuple64(-281474976710655L, 0x1FFFFFFFFFFFDL);
        testEncodedDecodedTuple64(281474976710655L, 0x1FFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-281474976710656L, 0x1FFFFFFFFFFFFL);
        testEncodedDecodedTuple64(281474976710656L, 0x2000000000000L);
        testEncodedDecodedTuple64(-281474976710657L, 0x2000000000001L);
        testEncodedDecodedTuple64(281474976710657L, 0x2000000000002L);

        testEncodedDecodedTuple64(-562949953421311L, 0x3FFFFFFFFFFFDL);
        testEncodedDecodedTuple64(562949953421311L, 0x3FFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-562949953421312L, 0x3FFFFFFFFFFFFL);
        testEncodedDecodedTuple64(562949953421312L, 0x4000000000000L);
        testEncodedDecodedTuple64(-562949953421313L, 0x4000000000001L);
        testEncodedDecodedTuple64(562949953421313L, 0x4000000000002L);

        testEncodedDecodedTuple64(-1125899906842623L, 0x7FFFFFFFFFFFDL);
        testEncodedDecodedTuple64(1125899906842623L, 0x7FFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-1125899906842624L, 0x7FFFFFFFFFFFFL);
        testEncodedDecodedTuple64(1125899906842624L, 0x8000000000000L);
        testEncodedDecodedTuple64(-1125899906842625L, 0x8000000000001L);
        testEncodedDecodedTuple64(1125899906842625L, 0x8000000000002L);

        testEncodedDecodedTuple64(-2251799813685247L, 0xFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(2251799813685247L, 0xFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-2251799813685248L, 0xFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(2251799813685248L, 0x10000000000000L);
        testEncodedDecodedTuple64(-2251799813685249L, 0x10000000000001L);
        testEncodedDecodedTuple64(2251799813685249L, 0x10000000000002L);

        testEncodedDecodedTuple64(-4503599627370495L, 0x1FFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(4503599627370495L, 0x1FFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-4503599627370496L, 0x1FFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(4503599627370496L, 0x20000000000000L);
        testEncodedDecodedTuple64(-4503599627370497L, 0x20000000000001L);
        testEncodedDecodedTuple64(4503599627370497L, 0x20000000000002L);

        testEncodedDecodedTuple64(-9007199254740991L, 0x3FFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(9007199254740991L, 0x3FFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-9007199254740992L, 0x3FFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(9007199254740992L, 0x40000000000000L);
        testEncodedDecodedTuple64(-9007199254740993L, 0x40000000000001L);
        testEncodedDecodedTuple64(9007199254740993L, 0x40000000000002L);

        testEncodedDecodedTuple64(-18014398509481983L, 0x7FFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(18014398509481983L, 0x7FFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-18014398509481984L, 0x7FFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(18014398509481984L, 0x80000000000000L);
        testEncodedDecodedTuple64(-18014398509481985L, 0x80000000000001L);
        testEncodedDecodedTuple64(18014398509481985L, 0x80000000000002L);

        testEncodedDecodedTuple64(-36028797018963967L, 0xFFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(36028797018963967L, 0xFFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-36028797018963968L, 0xFFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(36028797018963968L, 0x100000000000000L);
        testEncodedDecodedTuple64(-36028797018963969L, 0x100000000000001L);
        testEncodedDecodedTuple64(36028797018963969L, 0x100000000000002L);

        testEncodedDecodedTuple64(-72057594037927935L, 0x1FFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(72057594037927935L, 0x1FFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-72057594037927936L, 0x1FFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(72057594037927936L, 0x200000000000000L);
        testEncodedDecodedTuple64(-72057594037927937L, 0x200000000000001L);
        testEncodedDecodedTuple64(72057594037927937L, 0x200000000000002L);

        testEncodedDecodedTuple64(-144115188075855871L, 0x3FFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(144115188075855871L, 0x3FFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-144115188075855872L, 0x3FFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(144115188075855872L, 0x400000000000000L);
        testEncodedDecodedTuple64(-144115188075855873L, 0x400000000000001L);
        testEncodedDecodedTuple64(144115188075855873L, 0x400000000000002L);

        testEncodedDecodedTuple64(-288230376151711743L, 0x7FFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(288230376151711743L, 0x7FFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-288230376151711744L, 0x7FFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(288230376151711744L, 0x800000000000000L);
        testEncodedDecodedTuple64(-288230376151711745L, 0x800000000000001L);
        testEncodedDecodedTuple64(288230376151711745L, 0x800000000000002L);

        testEncodedDecodedTuple64(-576460752303423487L, 0xFFFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(576460752303423487L, 0xFFFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-576460752303423488L, 0xFFFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(576460752303423488L, 0x1000000000000000L);
        testEncodedDecodedTuple64(-576460752303423489L, 0x1000000000000001L);
        testEncodedDecodedTuple64(576460752303423489L, 0x1000000000000002L);

        testEncodedDecodedTuple64(-1152921504606846975L, 0x1FFFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(1152921504606846975L, 0x1FFFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-1152921504606846976L, 0x1FFFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(1152921504606846976L, 0x2000000000000000L);
        testEncodedDecodedTuple64(-1152921504606846977L, 0x2000000000000001L);
        testEncodedDecodedTuple64(1152921504606846977L, 0x2000000000000002L);

        testEncodedDecodedTuple64(-2305843009213693951L, 0x3FFFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(2305843009213693951L, 0x3FFFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-2305843009213693952L, 0x3FFFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(2305843009213693952L, 0x4000000000000000L);
        testEncodedDecodedTuple64(-2305843009213693953L, 0x4000000000000001L);
        testEncodedDecodedTuple64(2305843009213693953L, 0x4000000000000002L);

        testEncodedDecodedTuple64(-4611686018427387903L, 0x7FFFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(4611686018427387903L, 0x7FFFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-4611686018427387904L, 0x7FFFFFFFFFFFFFFFL);
        testEncodedDecodedTuple64(4611686018427387904L, 0x8000000000000000L);
        testEncodedDecodedTuple64(-4611686018427387905L, 0x8000000000000001L);
        testEncodedDecodedTuple64(4611686018427387905L, 0x8000000000000002L);

        testEncodedDecodedTuple64(-9223372036854775807L, 0xFFFFFFFFFFFFFFFDL);
        testEncodedDecodedTuple64(9223372036854775807L, 0xFFFFFFFFFFFFFFFEL);
        testEncodedDecodedTuple64(-9223372036854775808L, 0xFFFFFFFFFFFFFFFFL);
    }

    private static void testEncodedDecodedTuple16(short signedUnencoded, short unsignedEncoded) {
        assertEquals(unsignedEncoded, ZigzagHelper.encodeZigzag16(signedUnencoded));
        assertEquals(signedUnencoded, ZigzagHelper.decodeZigzag16(unsignedEncoded));
    }

    private static void testEncodedDecodedTuple32(int signedUnencoded, int unsignedEncoded) {
        assertEquals(unsignedEncoded, ZigzagHelper.encodeZigzag32(signedUnencoded));
        assertEquals(signedUnencoded, ZigzagHelper.decodeZigzag32(unsignedEncoded));
    }

    private static void testEncodedDecodedTuple64(long signedUnencoded, long unsignedEncoded) {
        assertEquals(unsignedEncoded, ZigzagHelper.encodeZigzag64(signedUnencoded));
        assertEquals(signedUnencoded, ZigzagHelper.decodeZigzag64(unsignedEncoded));
    }
}
