// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.math.BigInteger;

/**
 * Contains helper methods for working with unsigned integers represented by signed types.
 * Since Java doesn't have unsigned integral types, they are represented by signed integral
 * types of the same width (e.g. short for unsigned 16-bit integer) where the sign bit is
 * interpreted as the most significant bit of the unsigned value. Please note that code
 * using unsigned integers in numeric expressions is responsible for implementing overflow
 * protection such as type promotion (e.g. use int to represent unsigned 16-bit value).
 */
final class UnsignedHelper {

    // prevent instantiation
    private UnsignedHelper() {
    }

    private static final BigInteger TWO_TO_POWER_8 = BigInteger.valueOf(2L).pow(8);
    private static final BigInteger TWO_TO_POWER_16 = BigInteger.valueOf(2L).pow(16);
    private static final BigInteger TWO_TO_POWER_32 = BigInteger.valueOf(2L).pow(32);
    private static final BigInteger TWO_TO_POWER_64 = BigInteger.valueOf(2L).pow(64);

    static final short MAX_UINT8_VALUE = 255;
    static final int MAX_UINT16_VALUE = 65535;
    static final long MAX_UINT32_VALUE = 4294967295L;
    static final BigInteger MAX_UINT64_VALUE = new BigInteger("18446744073709551615");

    /**
     * Converts an unsigned 8-bit value (represented by the signed byte data type) to a 16-bit short value.
     * The result represents a non-negative value within the range of 8-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return an int containing the unsigned 8-bit value
     */
    static short asUnsignedShort(byte signedValue) {
        return (short) (signedValue & 0xFF);
    }

    /**
     * Converts an unsigned 8-bit value (represented by the signed byte data type) to a 32-bit int value.
     * The result represents a non-negative value within the range of 8-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return an int containing the unsigned 8-bit value
     */
    static int asUnsignedInt(byte signedValue) {
        return signedValue & 0xFF;
    }

    /**
     * Converts an unsigned 16-bit value (represented by the signed short data type) to a 32-bit int value.
     * The result represents a non-negative value within the range of 16-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return an int containing the unsigned 16-bit value
     */
    static int asUnsignedInt(short signedValue) {
        return signedValue & 0xFFFF;
    }

    /**
     * Converts an unsigned 8-bit value (represented by the signed byte data type) to a 64-bit long value.
     * The result represents a non-negative value within the range of 8-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return an int containing the unsigned 8-bit value
     */
    static long asUnsignedLong(byte signedValue) {
        return signedValue & 0xFFL;
    }

    /**
     * Converts an unsigned 16-bit value (represented by the signed short data type) to a 64-bit long value.
     * The result represents a non-negative value within the range of 16-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return an int containing the unsigned 16-bit value
     */
    static long asUnsignedLong(short signedValue) {
        return signedValue & 0xFFFFL;
    }

    /**
     * Converts an unsigned 32-bit value (represented by the signed int data type) to a 64-bit long value.
     * The result represents a non-negative value within the range of 32-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return an int containing the unsigned 32-bit value
     */
    static long asUnsignedLong(int signedValue) {
        return signedValue & 0xFFFFFFFFL;
    }

    /**
     * Converts an unsigned 8-bit value (represented by the signed byte data type) to a BigInteger value.
     * The result represents a non-negative value within the range of 8-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return a BigInteger object containing the unsigned 8-bit value
     */
    static BigInteger asUnsignedBigInt(byte signedValue) {
        if (signedValue >= 0) {
            return BigInteger.valueOf(signedValue);
        } else {
            return TWO_TO_POWER_8.add(BigInteger.valueOf(signedValue));
        }
    }

    /**
     * Converts an unsigned 16-bit value (represented by the signed short data type) to a BigInteger value.
     * The result represents a non-negative value within the range of 16-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return a BigInteger object containing the unsigned 16-bit value
     */
    static BigInteger asUnsignedBigInt(short signedValue) {
        if (signedValue >= 0) {
            return BigInteger.valueOf(signedValue);
        } else {
            return TWO_TO_POWER_16.add(BigInteger.valueOf(signedValue));
        }
    }

    /**
     * Converts an unsigned 32-bit value (represented by the signed int data type) to a BigInteger value.
     * The result represents a non-negative value within the range of 32-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return a BigInteger object containing the unsigned 32-bit value
     */
    static BigInteger asUnsignedBigInt(int signedValue) {
        if (signedValue >= 0) {
            return BigInteger.valueOf(signedValue);
        } else {
            return TWO_TO_POWER_32.add(BigInteger.valueOf(signedValue));
        }
    }

    /**
     * Converts an unsigned 64-bit value (represented by the signed long data type) to a BigInteger value.
     * The result represents a non-negative value within the range of 64-bit unsigned integers.
     *
     * @param signedValue the value, interpreted as unsigned
     * @return a BigInteger object containing the unsigned 64-bit value
     */
    static BigInteger asUnsignedBigInt(long signedValue) {
        if (signedValue >= 0) {
            return BigInteger.valueOf(signedValue);
        } else {
            return TWO_TO_POWER_64.add(BigInteger.valueOf(signedValue));
        }
    }
}
