// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * An implementation of the {@link Something} construct that wraps an object value.
 * @param <T> the type of the wrapped value
 */
public final class SomethingObject<T> extends Something<T> {

    /**
     * The wrapped object.
     */
    public T value;

    // restrict instantiation to this package only
    SomethingObject(T value) {
        this.value = value;
    }

    @Override
    public T getValue() {
        return this.value;
    }

    @Override
    public void setValue(T value) {
        this.value = value;
    }

    @Override
    public int hashCode() {
        return this.value != null ? this.value.hashCode() : 0;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof SomethingObject) {
            Something<?> that = (Something<?>) obj;
            return this.value == null ?
                    that.getValue() == null :
                    this.value.equals(that.getValue());
        }
        return false;
    }
}
