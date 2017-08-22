// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * An implementation of the {@link Something} construct that wraps a primitive int value which can be
 * accessed directly and is never set to null.
 *
 * Used for values of non-nullable "uint32" and "int32" fields with "nothing" as the default value.
 */
public final class SomethingInteger extends Something<Integer> {

    /**
     * The wrapped primitive value.
     */
    public int value;

    // restrict instantiation to this package only
    SomethingInteger(int value) {
        this.value = value;
    }

    @Override
    public void setValue(Integer value) {
        this.value = value;
    }

    @Override
    public Integer getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return this.value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SomethingInteger) {
            return this.value == ((SomethingInteger) obj).value;
        }
        return false;
    }
}
