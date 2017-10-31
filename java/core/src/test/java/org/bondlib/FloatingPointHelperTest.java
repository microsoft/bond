// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import static org.junit.Assert.*;

public class FloatingPointHelperTest {

    @Test
    public void staticClass() {
        TestHelper.verifyStaticHelperClass(FloatingPointHelper.class);
    }

    private static final float[] floatTestSet = new float[]{
            Float.NEGATIVE_INFINITY,
            Float.MIN_VALUE,
            Float.MIN_NORMAL,
            Float.MAX_VALUE,
            Float.POSITIVE_INFINITY,
            -1F,
            -0F,
            0F,
            1F,
            // NaN values
            TestHelper.rawIntBitsToFloat(0x7FC00000),
            TestHelper.rawIntBitsToFloat(0xFFC00000),
            TestHelper.rawIntBitsToFloat(0x7F800001),
            TestHelper.rawIntBitsToFloat(0x7FFFFFFF)
    };

    @Test
    public void testFloatEqualsAndHashCode() throws Exception {
        for (int i = 0; i < floatTestSet.length; ++i) {
            float a = floatTestSet[i];
            for (int j = 0; j < floatTestSet.length; ++j) {
                float b = floatTestSet[j];
                if (new Float(a).equals(b)) {
                    assertTrue(FloatingPointHelper.floatEquals(a, b));
                    assertEquals(FloatingPointHelper.floatHashCode(a), FloatingPointHelper.floatHashCode(b));
                    assertEquals(FloatingPointHelper.floatHashCode(a), new Float(a).hashCode());
                } else {
                    new Long(0).hashCode();
                    assertFalse(FloatingPointHelper.floatEquals(a, b));
                }
            }
        }
    }

    private static final double[] doubleTestSet = new double[]{
            Double.NEGATIVE_INFINITY,
            Double.MIN_VALUE,
            Double.MIN_NORMAL,
            Double.MAX_VALUE,
            Double.POSITIVE_INFINITY,
            -1D,
            -0D,
            0D,
            1D,
            // NaN values
            TestHelper.rawLongBitsToDouble(0x7FF8000000000000L),
            TestHelper.rawLongBitsToDouble(0xFFF8000000000000L),
            TestHelper.rawLongBitsToDouble(0x7FF0000000000001L),
            TestHelper.rawLongBitsToDouble(0x7FFFFFFFFFFFFFFFL)
    };

    @Test
    public void testDoubleEqualsAndHashCode() throws Exception {
        for (int i = 0; i < doubleTestSet.length; ++i) {
            double a = doubleTestSet[i];
            for (int j = 0; j < doubleTestSet.length; ++j) {
                double b = doubleTestSet[j];
                if (new Double(a).equals(b)) {
                    assertTrue(FloatingPointHelper.doubleEquals(a, b));
                    assertEquals(FloatingPointHelper.doubleHashCode(a), FloatingPointHelper.doubleHashCode(b));
                } else {
                    assertFalse(FloatingPointHelper.doubleEquals(a, b));
                }
            }
        }
    }
}
