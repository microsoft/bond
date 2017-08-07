// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.helpers;

/**
 * Contains helper methods for working with floating point values.
 */
public final class FloatingPointHelper {

    // prevent instantiation
    private FloatingPointHelper() {
    }

    /**
     * Compares two single-precision floating point values for equality.
     * This method should be used instead of the "==" operator to avoid issues such as NaN
     * (where "==" comparison is always false) or -0.0 (where two equal values have different bits).
     *
     * @param a single-precision value
     * @param b single-precision value
     * @return true if equal, false if not
     */
    public static boolean floatEquals(float a, float b) {
        return Float.floatToRawIntBits(a) == Float.floatToRawIntBits(b);
    }

    /**
     * Compares two double-precision floating point values for equality.
     * This method should be used instead of the "==" operator to avoid issues such as NaN
     * (where "==" comparison is always false) or -0.0 (where two equal values have different bits).
     *
     * @param a double-precision value
     * @param b double-precision value
     * @return true if equal, false if not
     */
    public static boolean doubleEquals(double a, double b) {
        return Double.doubleToRawLongBits(a) == Double.doubleToRawLongBits(b);
    }

    /**
     * Computes a hash code of a single-precision floating point value.
     *
     * @param v single-precision value
     * @return hash code
     */
    public static int floatHashCode(float v) {
        return Float.floatToRawIntBits(v);
    }

    /**
     * Computes a hash code of a double-precision floating point value.
     *
     * @param v double-precision value
     * @return hash code
     */
    public static int doubleHashCode(double v) {
        long bits = Double.doubleToRawLongBits(v);
        return (int) (bits ^ (bits >>> 32));
    }
}
