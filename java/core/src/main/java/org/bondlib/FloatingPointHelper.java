// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * Provides implementations of floating-point equality and hashing that take
 * primitives and are compatible with those in {@link Float} and
 * {@link Double}. These functions are used by generated and library code.
 */
public final class FloatingPointHelper {

    // prevent instantiation
    private FloatingPointHelper() {
    }

    /**
     * Implements the semantics of {@link Float#equals(Object)} for two
     * primitive floats.
     *
     * @param a single-precision value
     * @param b single-precision value
     * @return true if equal, false if not
     */
    public static boolean floatEquals(float a, float b) {
        return Float.floatToIntBits(a) == Float.floatToIntBits(b);
    }

    /**
     * Implements the semantics of {@link Double#equals(Object)} for two
     * primitive doubles.
     *
     * @param a double-precision value
     * @param b double-precision value
     * @return true if equal, false if not
     */
    public static boolean doubleEquals(double a, double b) {
        return Double.doubleToLongBits(a) == Double.doubleToLongBits(b);
    }

    /**
     * Computes a hash code of a single-precision floating point value.
     * Identical to {@link Float#hashCode(float)}, which was introduced in Java
     * 8. Once Bond targets Java 8, this function should be replaced with that
     * one.
     *
     * @param v single-precision value
     * @return hash code
     */
    public static int floatHashCode(float v) {
        return Float.floatToIntBits(v);
    }

    /**
     * Computes a hash code of a double-precision floating point value.
     * Identical to {@link Double#hashCode(double)}, which was introduced in
     * Java 8. Once Bond targets Java 8, this function should be replaced with
     * that one.
     *
     * @param v double-precision value
     * @return hash code
     */
    public static int doubleHashCode(double v) {
        long bits = Double.doubleToLongBits(v);
        return (int) (bits ^ (bits >>> 32));
    }
}
