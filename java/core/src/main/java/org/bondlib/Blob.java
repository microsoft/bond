// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.util.Arrays;

/**
 * Provides implementation for the Bond blob type as a simple wrapper around {@code byte []}.
 */
public final class Blob {

    /**
     * Creates a new Blob instance
     * @return new Blob instance with an empty {@code byte[]}
     */
    public Blob() {
        this.data = new byte[0];
    }

    /**
     * Creates a new Blob instance wrapping an existing {@code byte[]}
     * @param data
     * @return new Blob instance wrapping the data array
     * @throws IllegalArgumentException if the argument is null
     */
    public Blob(byte[] data) {
        ArgumentHelper.ensureNotNull(data, "data");
        this.data = data;
    }

    /**
     * Compare for equality by contents of underlying {@code byte[]}
     * @param other object to compare against
     * @return {@code true} if object is also a Blob and the underlying arrays have equivalent contents
     */
    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof Blob)) {
            return false;
        }
        Blob otherBlob = (Blob) other;

        return Arrays.equals(this.data, otherBlob.data);
    }

    /**
     * Generate hash code based on contents of underlying {@code byte[]}
     * @return hash code of underlying array contents
     */
    @Override
    public int hashCode() {
        return Arrays.hashCode(this.data);
    }

    /**
     * Provide access to underlying {@code byte[]}
     * @return the underying {@code byte[]}
     */
    public byte[] getData()
    {
        return this.data;
    }

    private final byte[] data;
}
