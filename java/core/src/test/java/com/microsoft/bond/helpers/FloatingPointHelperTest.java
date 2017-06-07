package com.microsoft.bond.helpers;

import com.microsoft.bond.TestHelper;
import org.junit.Test;

import static org.junit.Assert.*;

public class FloatingPointHelperTest {

    @Test
    public void staticClass() {
        TestHelper.verifyStaticHelperClass(FloatingPointHelper.class);
    }

    private static final float[] distinctFloatValuesTestSet = new float[]{
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
        for (int i = 0; i < distinctFloatValuesTestSet.length; ++i) {
            float a = distinctFloatValuesTestSet[i];
            for (int j = 0; j < distinctFloatValuesTestSet.length; ++j) {
                float b = distinctFloatValuesTestSet[j];
                if (i == j) {
                    assertTrue(FloatingPointHelper.floatEquals(a, b));
                    assertEquals(FloatingPointHelper.floatHashCode(a), FloatingPointHelper.floatHashCode(b));
                } else {
                    assertFalse(FloatingPointHelper.floatEquals(a, b));
                }
            }
        }
    }

    private static final double[] distinctDoubleValuesTestSet = new double[]{
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
        for (int i = 0; i < distinctDoubleValuesTestSet.length; ++i) {
            double a = distinctDoubleValuesTestSet[i];
            for (int j = 0; j < distinctDoubleValuesTestSet.length; ++j) {
                double b = distinctDoubleValuesTestSet[j];
                if (i == j) {
                    assertTrue(FloatingPointHelper.doubleEquals(a, b));
                    assertEquals(FloatingPointHelper.doubleHashCode(a), FloatingPointHelper.doubleHashCode(b));
                } else {
                    assertFalse(FloatingPointHelper.doubleEquals(a, b));
                }
            }
        }
    }
}