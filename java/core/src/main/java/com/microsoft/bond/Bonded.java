// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import com.microsoft.bond.protocol.ProtocolWriter;

import java.io.IOException;

/**
 * Placeholder Bonded for initial development. Just a box around a T.
 */
public class Bonded<T extends BondSerializable> implements IBonded<T> {
    private T value;

    public Bonded(T instance) {
        value = instance;
    }

    @Override
    public void serialize(ProtocolWriter writer) throws IOException {
        // TODO: implement serialization/deserialization of Bonded types
        throw new UnsupportedOperationException();
    }

    @Override
    public T deserialize() {
        return value;
    }
}
