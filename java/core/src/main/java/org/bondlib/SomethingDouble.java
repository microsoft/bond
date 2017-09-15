// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * An implementation of the {@link Something} construct that wraps a primitive double value which can be
 * accessed directly and is never set to null.
 *
 * Used for values of non-nullable "double" fields with "nothing" as the default value.
 */
public final class SomethingDouble extends Something<Double> {

    /**
     * The wrapped primitive value.
     */
    public double value;

    // restrict instantiation to this package only
    SomethingDouble(double value) {
        this.value = value;
    }

    @Override
    public void setValue(Double value) {
        this.value = value;
    }

    @Override
    public Double getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return FloatingPointHelper.doubleHashCode(this.value);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SomethingDouble) {
            return FloatingPointHelper.doubleEquals(this.value, ((SomethingDouble) obj).value);
        }
        return false;
    }
}
