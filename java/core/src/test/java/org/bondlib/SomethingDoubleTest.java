// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import org.junit.Test;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class SomethingDoubleTest {

    private static final double[] testValues = new double[]{
            Double.NEGATIVE_INFINITY,
            Double.MIN_VALUE,
            Double.MIN_VALUE + 1,
            Double.MIN_VALUE / 2,
            -1D,
            -0D,
            0D,
            1D,
            Double.MAX_VALUE / 2,
            Double.MAX_VALUE - 1,
            Double.MAX_VALUE,
            Double.POSITIVE_INFINITY,
            Double.NaN,
            // various NaN values
            TestHelper.rawLongBitsToDouble(0x7FF8000000000000L),
            TestHelper.rawLongBitsToDouble(0xFFF8000000000000L),
            TestHelper.rawLongBitsToDouble(0x7FF0000000000001L),
            TestHelper.rawLongBitsToDouble(0x7FFFFFFFFFFFFFFFL)
    };

    @Test
    public void testConstructorAndMethods() {
        for (double testValue : testValues) {
            SomethingDouble testObject = new SomethingDouble(testValue);
            assertEquals(testValue, testObject.value, 0);
            assertEquals(new Double(testValue), testObject.getValue());
            double newTestValue = -testValue;
            testObject.setValue(newTestValue);
            assertEquals(newTestValue, testObject.value, 0);
            assertEquals(new Double(newTestValue), testObject.getValue());
        }
    }

    @Test(expected = NullPointerException.class)
    public void testSetterDoesNotAcceptNull() {
        SomethingDouble testObject = new SomethingDouble((double) 0);
        testObject.setValue(null);
    }

    @Test
    public void testEqualsAndHashCode() {
        // 3 copies of each value, plus some other types
        ArrayList<Object> testObjects = new ArrayList<Object>();
        for (double testValue : testValues) {
            testObjects.add(new SomethingDouble(testValue));
            testObjects.add(new SomethingDouble(testValue));
            testObjects.add(new SomethingDouble(testValue));
            testObjects.add(new SomethingObject<Double>(testValue));
            testObjects.add(new Double(testValue));
            testObjects.add("");
        }

        for (Object a : testObjects) {
            for (Object b : testObjects) {
                if (a instanceof SomethingDouble) {
                    assertFalse(a.equals(null));
                    SomethingDouble av = (SomethingDouble) a;
                    if (b instanceof SomethingDouble) {
                        SomethingDouble bv = (SomethingDouble) b;
                        assertEquals(FloatingPointHelper.doubleEquals(av.value, bv.value), a.equals(b));
                    } else {
                        assertFalse(a.equals(b));
                    }
                }
            }
        }

        TestHelper.verifyEqualsAndHashCodeConsistency(testObjects);
    }
}
