// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * An implementation of the {@link Something} construct that wraps a primitive float value which can be
 * accessed directly and is never set to null.
 *
 * Used for values of non-nullable "float" fields with "nothing" as the default value.
 */
public final class SomethingFloat extends Something<Float> {

    /**
     * The wrapped primitive value.
     */
    public float value;

    // restrict instantiation to this package only
    SomethingFloat(float value) {
        this.value = value;
    }

    @Override
    public void setValue(Float value) {
        this.value = value;
    }

    @Override
    public Float getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return FloatingPointHelper.floatHashCode(this.value);
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SomethingFloat) {
            return FloatingPointHelper.floatEquals(this.value, ((SomethingFloat) obj).value);
        }
        return false;
    }
}
