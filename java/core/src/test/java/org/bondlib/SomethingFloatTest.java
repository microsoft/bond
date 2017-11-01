// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class SomethingFloatTest {

    private static final float[] testValues = new float[]{
            Float.NEGATIVE_INFINITY,
            Float.MIN_VALUE,
            Float.MIN_VALUE + 1,
            Float.MIN_VALUE / 2,
            -1F,
            -0F,
            0F,
            1F,
            Float.MAX_VALUE / 2,
            Float.MAX_VALUE - 1,
            Float.MAX_VALUE,
            Float.POSITIVE_INFINITY,
            Float.NaN,
            // various NaN values
            TestHelper.rawIntBitsToFloat(0x7FC00000),
            TestHelper.rawIntBitsToFloat(0xFFC00000),
            TestHelper.rawIntBitsToFloat(0x7F800001),
            TestHelper.rawIntBitsToFloat(0x7FFFFFFF)
    };

    @Test
    public void testConstructorAndMethods() {
        for (float testValue : testValues) {
            SomethingFloat testObject = new SomethingFloat(testValue);
            assertEquals(testValue, testObject.value, 0);
            assertEquals(new Float(testValue), testObject.getValue());
            float newTestValue = -testValue;
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.value, 0);
            assertEquals(new Float(newTestValue), testObject.getValue());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testSetterDoesNotAcceptNull() {
        SomethingFloat testObject = new SomethingFloat(0F);
        testObject.setValue(null);
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (float testValue : testValues) {
            testObjects.add(new SomethingFloat(testValue));
            testObjects.add(new SomethingFloat(testValue));
            testObjects.add(new SomethingFloat(testValue));
            testObjects.add(new SomethingObject<Float>(testValue));
            testObjects.add(new Float(testValue));
            testObjects.add("");
        }

        for (Object a : testObjects) {
            for (Object b : testObjects) {
                if (a instanceof SomethingFloat) {
                    assertFalse(a.equals(null));
                    SomethingFloat av = (SomethingFloat) a;
                    if (b instanceof SomethingFloat) {
                        SomethingFloat bv = (SomethingFloat) b;
                        assertEquals(FloatingPointHelper.floatEquals(av.value, bv.value), a.equals(b));
                    } else {
                        assertFalse(a.equals(b));
                    }
                }
            }
        }

        TestHelper.verifyEqualsAndHashCodeConsistency(testObjects);
    }
}
