// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * An implementation of the {@link Something} construct that wraps a primitive byte value which can be
 * accessed directly and is never set to null.
 *
 * Used for values of non-nullable "uint8" and "int8" fields with "nothing" as the default value.
 */
public final class SomethingByte extends Something<Byte> {

    /**
     * The wrapped primitive value.
     */
    public byte value;

    // restrict instantiation to this package only
    SomethingByte(byte value) {
        this.value = value;
    }

    @Override
    public void setValue(Byte value) {
        this.value = value;
    }

    @Override
    public Byte getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return this.value;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SomethingByte) {
            return this.value == ((SomethingByte) obj).value;
        }
        return false;
    }
}
