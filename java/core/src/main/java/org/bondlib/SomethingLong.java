// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * An implementation of the {@link Something} construct that wraps a primitive long value which can be
 * accessed directly and is never set to null.
 *
 * Used for values of non-nullable "uint64" and "int64" fields with "nothing" as the default value.
 */
public final class SomethingLong extends Something<Long> {

    /**
     * The wrapped primitive value.
     */
    public long value;

    // restrict instantiation to this package only
    SomethingLong(long value) {
        this.value = value;
    }

    @Override
    public void setValue(Long value) {
        this.value = value;
    }

    @Override
    public Long getValue() {
        return this.value;
    }

    @Override
    public int hashCode() {
        return (int) (this.value ^ (this.value >>> 32));
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SomethingLong) {
            return this.value == ((SomethingLong) obj).value;
        }
        return false;
    }
}
