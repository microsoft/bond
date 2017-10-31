// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * Contains helper methods for encoding and decoding integers using the zigzag scheme.
 */
final class ZigzagHelper {

    // prevent instantiation
    private ZigzagHelper() {
    }

    static short encodeZigzag16(short signedValue) {
        return (short) ((signedValue << 1) ^ (signedValue >> 15));
    }

    static int encodeZigzag32(int signedValue) {
        return (signedValue << 1) ^ (signedValue >> 31);
    }

    static long encodeZigzag64(long signedValue) {
        return (signedValue << 1) ^ (signedValue >> 63);
    }

    static short decodeZigzag16(short unsignedValue) {
        return (short) (((unsignedValue & 0x0000FFFF) >>> 1) ^ -(unsignedValue & 1));
    }

    static int decodeZigzag32(int unsignedValue) {
        return (unsignedValue >>> 1) ^ -(unsignedValue & 1);
    }

    static long decodeZigzag64(long unsignedValue) {
        return (unsignedValue >>> 1) ^ -(unsignedValue & 1);
    }
}
