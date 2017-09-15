// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.nio.charset.Charset;

/**
 * Contains helper methods for working with strings.
 */
final class StringHelper {

    // prevent instantiation
    private StringHelper() {
    }

    private static final Charset UTF8 = Charset.forName("UTF-8");
    private static final Charset UTF16LE = Charset.forName("UTF-16LE");

    static byte[] encodeString(String str) {
        return str.getBytes(UTF8);
    }

    static byte[] encodeWString(String str) {
        return str.getBytes(UTF16LE);
    }

    static String decodeString(byte[] bytes) {
        return new String(bytes, UTF8);
    }

    static String decodeWString(byte[] bytes) {
        return new String(bytes, UTF16LE);
    }

    static int getEncodedStringLength(String str) {
        return encodeString(str).length;
    }

    static int getEncodedWStringLength(String str) {
        return encodeWString(str).length;
    }
}
