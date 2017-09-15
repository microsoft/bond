// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.bondlib.TestHelper;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.EOFException;
import java.io.IOException;
import java.util.Arrays;

import static org.junit.Assert.*;

public class VarUIntHelperTest {

    @Test
    public void staticClass() {
        TestHelper.verifyStaticHelperClass(VarUIntHelper.class);
    }

    @Test
    public void testVarUInt16Encoding() {
        testEncodedDecodedTuple16((short) 0x0000, new byte[]{(byte) 0x00});

        testEncodedDecodedTuple16((short) 0x0001, new byte[]{(byte) 0x01});

        testEncodedDecodedTuple16((short) 0x0002, new byte[]{(byte) 0x02});
        testEncodedDecodedTuple16((short) 0x0003, new byte[]{(byte) 0x03});

        testEncodedDecodedTuple16((short) 0x0004, new byte[]{(byte) 0x04});
        testEncodedDecodedTuple16((short) 0x0005, new byte[]{(byte) 0x05});

        testEncodedDecodedTuple16((short) 0x0007, new byte[]{(byte) 0x07});
        testEncodedDecodedTuple16((short) 0x0008, new byte[]{(byte) 0x08});
        testEncodedDecodedTuple16((short) 0x0009, new byte[]{(byte) 0x09});

        testEncodedDecodedTuple16((short) 0x000F, new byte[]{(byte) 0x0F});
        testEncodedDecodedTuple16((short) 0x0010, new byte[]{(byte) 0x10});
        testEncodedDecodedTuple16((short) 0x0011, new byte[]{(byte) 0x11});

        testEncodedDecodedTuple16((short) 0x001F, new byte[]{(byte) 0x1F});
        testEncodedDecodedTuple16((short) 0x0020, new byte[]{(byte) 0x20});
        testEncodedDecodedTuple16((short) 0x0021, new byte[]{(byte) 0x21});

        testEncodedDecodedTuple16((short) 0x003F, new byte[]{(byte) 0x3F});
        testEncodedDecodedTuple16((short) 0x0040, new byte[]{(byte) 0x40});
        testEncodedDecodedTuple16((short) 0x0041, new byte[]{(byte) 0x41});

        testEncodedDecodedTuple16((short) 0x007F, new byte[]{(byte) 0x7F});
        testEncodedDecodedTuple16((short) 0x0080, new byte[]{(byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple16((short) 0x0081, new byte[]{(byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple16((short) 0x00FF, new byte[]{(byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple16((short) 0x0100, new byte[]{(byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple16((short) 0x0101, new byte[]{(byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple16((short) 0x0181, new byte[]{(byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple16((short) 0x01FF, new byte[]{(byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple16((short) 0x0200, new byte[]{(byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple16((short) 0x0201, new byte[]{(byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple16((short) 0x0281, new byte[]{(byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple16((short) 0x03FF, new byte[]{(byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple16((short) 0x0400, new byte[]{(byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple16((short) 0x0401, new byte[]{(byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple16((short) 0x0481, new byte[]{(byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple16((short) 0x07FF, new byte[]{(byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple16((short) 0x0800, new byte[]{(byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple16((short) 0x0801, new byte[]{(byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple16((short) 0x0881, new byte[]{(byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple16((short) 0x0FFF, new byte[]{(byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple16((short) 0x1000, new byte[]{(byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple16((short) 0x1001, new byte[]{(byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple16((short) 0x1081, new byte[]{(byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple16((short) 0x1FFF, new byte[]{(byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple16((short) 0x2000, new byte[]{(byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple16((short) 0x2001, new byte[]{(byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple16((short) 0x2081, new byte[]{(byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple16((short) 0x3FFF, new byte[]{(byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple16((short) 0x4000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple16((short) 0x4001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple16((short) 0x4081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple16((short) 0x7FFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple16((short) 0x8000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple16((short) 0x8001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple16((short) 0x8081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple16((short) 0xC081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple16((short) 0xFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x03});
    }

    @Test
    public void testVarUInt32Encoding() {
        testEncodedDecodedTuple32(0x00000000, new byte[]{(byte) 0x00});

        testEncodedDecodedTuple32(0x00000001, new byte[]{(byte) 0x01});

        testEncodedDecodedTuple32(0x00000002, new byte[]{(byte) 0x02});
        testEncodedDecodedTuple32(0x00000003, new byte[]{(byte) 0x03});

        testEncodedDecodedTuple32(0x00000004, new byte[]{(byte) 0x04});
        testEncodedDecodedTuple32(0x00000005, new byte[]{(byte) 0x05});

        testEncodedDecodedTuple32(0x00000007, new byte[]{(byte) 0x07});
        testEncodedDecodedTuple32(0x00000008, new byte[]{(byte) 0x08});
        testEncodedDecodedTuple32(0x00000009, new byte[]{(byte) 0x09});

        testEncodedDecodedTuple32(0x0000000F, new byte[]{(byte) 0x0F});
        testEncodedDecodedTuple32(0x00000010, new byte[]{(byte) 0x10});
        testEncodedDecodedTuple32(0x00000011, new byte[]{(byte) 0x11});

        testEncodedDecodedTuple32(0x0000001F, new byte[]{(byte) 0x1F});
        testEncodedDecodedTuple32(0x00000020, new byte[]{(byte) 0x20});
        testEncodedDecodedTuple32(0x00000021, new byte[]{(byte) 0x21});

        testEncodedDecodedTuple32(0x0000003F, new byte[]{(byte) 0x3F});
        testEncodedDecodedTuple32(0x00000040, new byte[]{(byte) 0x40});
        testEncodedDecodedTuple32(0x00000041, new byte[]{(byte) 0x41});

        testEncodedDecodedTuple32(0x0000007F, new byte[]{(byte) 0x7F});
        testEncodedDecodedTuple32(0x00000080, new byte[]{(byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x00000081, new byte[]{(byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple32(0x000000FF, new byte[]{(byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple32(0x00000100, new byte[]{(byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x00000101, new byte[]{(byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple32(0x00000181, new byte[]{(byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple32(0x000001FF, new byte[]{(byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple32(0x00000200, new byte[]{(byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x00000201, new byte[]{(byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple32(0x00000281, new byte[]{(byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple32(0x000003FF, new byte[]{(byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple32(0x00000400, new byte[]{(byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x00000401, new byte[]{(byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple32(0x00000481, new byte[]{(byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple32(0x000007FF, new byte[]{(byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple32(0x00000800, new byte[]{(byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple32(0x00000801, new byte[]{(byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple32(0x00000881, new byte[]{(byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple32(0x00000FFF, new byte[]{(byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple32(0x00001000, new byte[]{(byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple32(0x00001001, new byte[]{(byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple32(0x00001081, new byte[]{(byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple32(0x00001FFF, new byte[]{(byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple32(0x00002000, new byte[]{(byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple32(0x00002001, new byte[]{(byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple32(0x00002081, new byte[]{(byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple32(0x00003FFF, new byte[]{(byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple32(0x00004000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x00004001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x00004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple32(0x00007FFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple32(0x00008000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x00008001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x00008081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple32(0x0000C081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple32(0x0000FFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple32(0x00010000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x00010001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x00010081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple32(0x00014081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple32(0x0001FFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple32(0x00020000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x00020001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x00020081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple32(0x00024081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple32(0x0003FFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple32(0x00040000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple32(0x00040001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple32(0x00040081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple32(0x00044081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple32(0x0007FFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple32(0x00080000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple32(0x00080001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple32(0x00080081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple32(0x00084081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple32(0x000FFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple32(0x00100000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple32(0x00100001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple32(0x00100081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple32(0x00104081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple32(0x001FFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple32(0x00200000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x00200001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x00200081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x00204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple32(0x003FFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple32(0x00400000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x00400001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x00400081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x00404081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple32(0x00604081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple32(0x007FFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple32(0x00800000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x00800001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x00800081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x00804081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple32(0x00A04081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple32(0x00FFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple32(0x01000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x01000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x01000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x01004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple32(0x01204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple32(0x01FFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple32(0x02000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple32(0x02000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple32(0x02000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple32(0x02004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple32(0x02204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple32(0x03FFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple32(0x04000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple32(0x04000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple32(0x04000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple32(0x04004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple32(0x04204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple32(0x07FFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple32(0x08000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple32(0x08000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple32(0x08000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple32(0x08004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple32(0x08204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple32(0x0FFFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple32(0x10000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x10000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x10000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x10004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple32(0x10204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple32(0x1FFFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple32(0x20000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x20000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x20000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x20004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple32(0x20204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple32(0x30204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple32(0x3FFFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple32(0x40000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x40000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x40000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x40004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple32(0x40204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple32(0x50204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple32(0x7FFFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple32(0x80000000, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x80000001, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x80000081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x80004081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple32(0x80204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple32(0x90204081, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple32(0xFFFFFFFF, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
    }

    @Test
    public void testVarUInt64Encoding() {
        testEncodedDecodedTuple64(0x0000000000000000L, new byte[]{(byte) 0x00});

        testEncodedDecodedTuple64(0x0000000000000001L, new byte[]{(byte) 0x01});

        testEncodedDecodedTuple64(0x0000000000000002L, new byte[]{(byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000000003L, new byte[]{(byte) 0x03});

        testEncodedDecodedTuple64(0x0000000000000004L, new byte[]{(byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000000005L, new byte[]{(byte) 0x05});

        testEncodedDecodedTuple64(0x0000000000000007L, new byte[]{(byte) 0x07});
        testEncodedDecodedTuple64(0x0000000000000008L, new byte[]{(byte) 0x08});
        testEncodedDecodedTuple64(0x0000000000000009L, new byte[]{(byte) 0x09});

        testEncodedDecodedTuple64(0x000000000000000FL, new byte[]{(byte) 0x0F});
        testEncodedDecodedTuple64(0x0000000000000010L, new byte[]{(byte) 0x10});
        testEncodedDecodedTuple64(0x0000000000000011L, new byte[]{(byte) 0x11});

        testEncodedDecodedTuple64(0x000000000000001FL, new byte[]{(byte) 0x1F});
        testEncodedDecodedTuple64(0x0000000000000020L, new byte[]{(byte) 0x20});
        testEncodedDecodedTuple64(0x0000000000000021L, new byte[]{(byte) 0x21});

        testEncodedDecodedTuple64(0x000000000000003FL, new byte[]{(byte) 0x3F});
        testEncodedDecodedTuple64(0x0000000000000040L, new byte[]{(byte) 0x40});
        testEncodedDecodedTuple64(0x0000000000000041L, new byte[]{(byte) 0x41});

        testEncodedDecodedTuple64(0x000000000000007FL, new byte[]{(byte) 0x7F});
        testEncodedDecodedTuple64(0x0000000000000080L, new byte[]{(byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000000081L, new byte[]{(byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x00000000000000FFL, new byte[]{(byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000000100L, new byte[]{(byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000000101L, new byte[]{(byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000000181L, new byte[]{(byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x00000000000001FFL, new byte[]{(byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0000000000000200L, new byte[]{(byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000000201L, new byte[]{(byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000000281L, new byte[]{(byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x00000000000003FFL, new byte[]{(byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0000000000000400L, new byte[]{(byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000000000401L, new byte[]{(byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000000000481L, new byte[]{(byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x00000000000007FFL, new byte[]{(byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x0000000000000800L, new byte[]{(byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000000000801L, new byte[]{(byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000000000881L, new byte[]{(byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x0000000000000FFFL, new byte[]{(byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x0000000000001000L, new byte[]{(byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000000001001L, new byte[]{(byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000000001081L, new byte[]{(byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x0000000000001FFFL, new byte[]{(byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x0000000000002000L, new byte[]{(byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000000002001L, new byte[]{(byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000000002081L, new byte[]{(byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x0000000000003FFFL, new byte[]{(byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x0000000000004000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000004001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x0000000000007FFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000008000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000008001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000008081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x000000000000C081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x000000000000FFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0000000000010000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000010001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000010081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000014081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x000000000001FFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0000000000020000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000000020001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000000020081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000000024081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x000000000003FFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x0000000000040000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000000040001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000000040081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000000044081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x000000000007FFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x0000000000080000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000000080001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000000080081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000000084081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x00000000000FFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x0000000000100000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000000100001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000000100081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000000104081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x00000000001FFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x0000000000200000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000200001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000200081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x00000000003FFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000000400000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000400001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000400081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000404081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000000604081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x00000000007FFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0000000000800000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000800001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000800081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000804081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000000A04081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x0000000000FFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0000000001000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000001000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000001000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000001004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000001204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x0000000001FFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x0000000002000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000002000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000002000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000002004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000002204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x0000000003FFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x0000000004000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000004000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000004000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000004004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000004204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x0000000007FFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x0000000008000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000008000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000008000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000008004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000008204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x000000000FFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x0000000010000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000010000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000010000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000010004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x000000001FFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000020000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000020000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000020000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000020004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000020204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000000030204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x000000003FFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0000000040000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000040000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000040000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000040004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000040204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000000050204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x000000007FFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0000000080000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000080000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000080000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000080004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000080204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000000090204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x00000000FFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x0000000100000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000100000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000100000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000100004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000100204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000000110204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x00000001FFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x0000000200000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000200000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000200000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000200004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000200204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000000210204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x00000003FFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x0000000400000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000400000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000400000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000400004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000400204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000000410204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x00000007FFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x0000000800000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000800000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000800000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000800004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000800204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x0000000FFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000001000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000001000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000001000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000001000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000001000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000001010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000001810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x0000001FFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0000002000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000002000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000002000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000002000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000002000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000002010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000002810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x0000003FFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0000004000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000004000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000004000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000004000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000004000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000004010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000004810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x0000007FFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x0000008000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000008000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000008000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000008000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000008000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000008010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000008810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x000000FFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x0000010000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000010000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000010000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000010000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000010000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000010010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000010810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x000001FFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x0000020000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000020000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000020000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000020000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000020000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000020010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x0000020810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x000003FFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x0000040000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000040000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000040000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000040000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000040000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000040010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x000007FFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0000080000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000080000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000080000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000080000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000080000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000080010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0000080810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x00000C0810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x00000FFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0000100000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000100000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000100000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000100000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000100000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000100010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000100810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x0000140810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x00001FFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0000200000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000200000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000200000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000200000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000200000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000200010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000200810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0000240810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x00003FFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x0000400000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000400000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000400000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000400000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000400000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000400010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000400810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x0000440810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x00007FFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x0000800000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000800000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000800000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000800000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000800000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000800010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000800810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x0000840810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x0000FFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x0001000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0001000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0001000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0001000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0001000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0001000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0001000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x0001040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x0001FFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x0002000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0002000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0002000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0002000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0002000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0002000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0002000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0002040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x0003FFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0004000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0004000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0004000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0004000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0004000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0004000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0004000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0004040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x0006040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x0007FFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0008000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0008000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0008000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0008000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0008000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0008000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0008000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0008040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x000A040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x000FFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0010000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0010000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0010000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0010000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0010000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0010000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0010000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0010040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0012040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x001FFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x0020000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0020000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0020000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0020000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0020000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0020000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0020000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x0020040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x0022040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x003FFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x0040000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0040000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0040000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0040000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0040000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0040000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0040000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x0040040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x0042040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x007FFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x0080000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0080000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0080000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0080000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0080000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0080000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0080000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x0080040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x0082040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x00FFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x0100000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0100000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0100000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0100000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0100000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0100000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0100000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0100040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x0102040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0x01FFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
        testEncodedDecodedTuple64(0x0200000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0200000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0200000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0200000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0200000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0200000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0200000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0200040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x02});
        testEncodedDecodedTuple64(0x0202040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x02});
        testEncodedDecodedTuple64(0x0302040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x03});

        testEncodedDecodedTuple64(0x03FFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x03});
        testEncodedDecodedTuple64(0x0400000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0400000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0400000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0400000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0400000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0400000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0400000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0400040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x04});
        testEncodedDecodedTuple64(0x0402040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x04});
        testEncodedDecodedTuple64(0x0502040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x05});

        testEncodedDecodedTuple64(0x07FFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x07});
        testEncodedDecodedTuple64(0x0800000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0800000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0800000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0800000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0800000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0800000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0800000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0800040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x08});
        testEncodedDecodedTuple64(0x0802040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x08});
        testEncodedDecodedTuple64(0x0902040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x09});

        testEncodedDecodedTuple64(0x0FFFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x0F});
        testEncodedDecodedTuple64(0x1000000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1000000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1000000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1000000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1000000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1000000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1000000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1000040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x10});
        testEncodedDecodedTuple64(0x1002040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x10});
        testEncodedDecodedTuple64(0x1102040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x11});

        testEncodedDecodedTuple64(0x1FFFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x1F});
        testEncodedDecodedTuple64(0x2000000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2000000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2000000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2000000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2000000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2000000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2000000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2000040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x20});
        testEncodedDecodedTuple64(0x2002040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x20});
        testEncodedDecodedTuple64(0x2102040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x21});

        testEncodedDecodedTuple64(0x3FFFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x3F});
        testEncodedDecodedTuple64(0x4000000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4000000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4000000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4000000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4000000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4000000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4000000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4000040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x40});
        testEncodedDecodedTuple64(0x4002040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x40});
        testEncodedDecodedTuple64(0x4102040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x41});

        testEncodedDecodedTuple64(0x7FFFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x7F});
        testEncodedDecodedTuple64(0x8000000000000000L, new byte[]{(byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8000000000000001L, new byte[]{(byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8000000000000081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8000000000004081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8000000000204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8000000010204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8000000810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8000040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8002040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x80, (byte) 0x01});
        testEncodedDecodedTuple64(0x8102040810204081L, new byte[]{(byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x81, (byte) 0x01});

        testEncodedDecodedTuple64(0xFFFFFFFFFFFFFFFFL, new byte[]{(byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0xFF, (byte) 0x01});
    }

    private static void testEncodedDecodedTuple16(short unsignedUnencoded, byte[] encoded) {
        // encode
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int encodeReturnValue = 0;
        try {
            encodeReturnValue = VarUIntHelper.encodeVarUInt16(unsignedUnencoded, baos);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify return value (number of bytes written)
        assertEquals(encoded.length, encodeReturnValue);

        // verify encoded
        assertEquals(encoded.length, baos.size());
        assertArrayEquals(encoded, baos.toByteArray());

        // decode
        ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
        short unsignedDecoded = 0;
        try {
            unsignedDecoded = VarUIntHelper.decodeVarUInt16(bais);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify all bytes are consumed
        assertEquals(-1, bais.read());

        // verify decoded
        assertEquals(unsignedUnencoded, unsignedDecoded);

        // verify byte count
        assertEquals(encoded.length, VarUIntHelper.getVarUInt16Length(unsignedUnencoded));

        // verify each high bit is set in every encoded byte except the last
        for (int i = 0; i < encoded.length - 1; ++i) {
            assertEquals((byte) 0xFF, (byte) (encoded[i] | 0x7F));
        }

        // verify EOFException is raised when input is incomplete
        for (int i = 0; i < encoded.length - 1; ++i) {
            byte[] encodedIncomplete = Arrays.copyOf(encoded, i);
            bais = new ByteArrayInputStream(encodedIncomplete);
            try {
                short decodedFromIncomplete = VarUIntHelper.decodeVarUInt16(bais);
                fail("Operation can't succeed: " + decodedFromIncomplete);
            } catch (EOFException e) {
                // success
            } catch (IOException e) {
                fail("Other IOException can't be thrown here: " + e);
            }
        }

        // verify encoding works when unused bits are not zero
        if (encoded.length == 3) {
            // make sure unused 6 bits are zero when encoding
            assertEquals((byte) 0x03, (byte) (encoded[2] | 0x03));

            // set unused bytes to one and decode
            encoded[2] |= 0xFC;
            bais = new ByteArrayInputStream(encoded);
            try {
                unsignedDecoded = VarUIntHelper.decodeVarUInt16(bais);
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }

            // verify decoded value is still correct
            assertEquals(unsignedUnencoded, unsignedDecoded);
        }
    }

    private static void testEncodedDecodedTuple32(int unsignedUnencoded, byte[] encoded) {
        // encode
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int encodeReturnValue = 0;
        try {
            encodeReturnValue = VarUIntHelper.encodeVarUInt32(unsignedUnencoded, baos);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify return value (number of bytes written)
        assertEquals(encoded.length, encodeReturnValue);

        // verify encoded
        assertEquals(encoded.length, baos.size());
        assertArrayEquals(encoded, baos.toByteArray());

        // decode
        ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
        int unsignedDecoded = 0;
        try {
            unsignedDecoded = VarUIntHelper.decodeVarUInt32(bais);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify all bytes are consumed
        assertEquals(-1, bais.read());

        // verify decoded
        assertEquals(unsignedUnencoded, unsignedDecoded);

        // verify byte count
        assertEquals(encoded.length, VarUIntHelper.getVarUInt32Length(unsignedUnencoded));

        // verify each high bit is set in every encoded byte except the last
        for (int i = 0; i < encoded.length - 1; ++i) {
            assertEquals((byte) 0xFF, (byte) (encoded[i] | 0x7F));
        }

        // verify EOFException is raised when input is incomplete
        for (int i = 0; i < encoded.length - 1; ++i) {
            byte[] encodedIncomplete = Arrays.copyOf(encoded, i);
            bais = new ByteArrayInputStream(encodedIncomplete);
            try {
                int decodedFromIncomplete = VarUIntHelper.decodeVarUInt32(bais);
                fail("Operation can't succeed: " + decodedFromIncomplete);
            } catch (EOFException e) {
                // success
            } catch (IOException e) {
                fail("Other IOException can't be thrown here: " + e);
            }
        }

        // verify encoding works when unused bits are not zero
        if (encoded.length == 5) {
            // make sure unused 4 bits are zero when encoding
            assertEquals((byte) 0x0F, (byte) (encoded[4] | 0x0F));

            // set unused bytes to one and decode
            encoded[4] |= 0xF0;
            bais = new ByteArrayInputStream(encoded);
            try {
                unsignedDecoded = VarUIntHelper.decodeVarUInt32(bais);
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }

            // verify decoded value is still correct
            assertEquals(unsignedUnencoded, unsignedDecoded);
        }
    }

    private static void testEncodedDecodedTuple64(long unsignedUnencoded, byte[] encoded) {
        // encode
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        int encodeReturnValue = 0;
        try {
            encodeReturnValue = VarUIntHelper.encodeVarUInt64(unsignedUnencoded, baos);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify return value (number of bytes written)
        assertEquals(encoded.length, encodeReturnValue);

        // verify encoded
        assertEquals(encoded.length, baos.size());
        assertArrayEquals(encoded, baos.toByteArray());

        // decode
        ByteArrayInputStream bais = new ByteArrayInputStream(encoded);
        long unsignedDecoded = 0;
        try {
            unsignedDecoded = VarUIntHelper.decodeVarUInt64(bais);
        } catch (IOException e) {
            fail("IOException can't be thrown here: " + e);
        }

        // verify all bytes are consumed
        assertEquals(-1, bais.read());

        // verify decoded
        assertEquals(unsignedUnencoded, unsignedDecoded);

        // verify byte count
        assertEquals(encoded.length, VarUIntHelper.getVarUInt64Length(unsignedUnencoded));

        // verify each high bit is set in every encoded byte except the last
        for (int i = 0; i < encoded.length - 1; ++i) {
            assertEquals((byte) 0xFF, (byte) (encoded[i] | 0x7F));
        }

        // verify EOFException is raised when input is incomplete
        for (int i = 0; i < encoded.length - 1; ++i) {
            byte[] encodedIncomplete = Arrays.copyOf(encoded, i);
            bais = new ByteArrayInputStream(encodedIncomplete);
            try {
                long decodedFromIncomplete = VarUIntHelper.decodeVarUInt64(bais);
                fail("Operation can't succeed: " + decodedFromIncomplete);
            } catch (EOFException e) {
                // success
            } catch (IOException e) {
                fail("Other IOException can't be thrown here: " + e);
            }
        }

        // verify encoding works when unused bits are not zero
        if (encoded.length == 10) {
            // make sure unused 4 bits are zero when encoding
            assertEquals((byte) 0x01, (byte) (encoded[9] | 0x01));

            // set unused bytes to one and decode
            encoded[9] |= 0xFE;
            bais = new ByteArrayInputStream(encoded);
            try {
                unsignedDecoded = VarUIntHelper.decodeVarUInt64(bais);
            } catch (IOException e) {
                fail("IOException can't be thrown here: " + e);
            }

            // verify decoded value is still correct
            assertEquals(unsignedUnencoded, unsignedDecoded);
        }
    }
}
