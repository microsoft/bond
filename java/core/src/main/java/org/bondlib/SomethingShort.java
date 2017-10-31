// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * An implementation of the {@link Something} construct that wraps a primitive short value which can be
 * accessed directly and is never set to null.
 *
 * Used for values of non-nullable "uint16" and "int16" fields with "nothing" as the default value.
 */
public final class SomethingShort extends Something<Short> {

    /**
     * The wrapped primitive value.
     */
    public short value;

    // restrict instantiation to this package only
    SomethingShort(short value) {
        this.value = value;
    }

    @Override
    public void setValue(Short value) {
        this.value = value;
    }

    @Override
    public Short getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return this.value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SomethingShort) {
            return this.value == ((SomethingShort) obj).value;
        }
        return false;
    }
}
