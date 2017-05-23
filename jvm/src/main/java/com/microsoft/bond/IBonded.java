// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import com.microsoft.bond.protocol.ProtocolWriter;

import java.io.IOException;

public interface IBonded<T extends BondSerializable> {
    <W extends ProtocolWriter> void Serialize(W writer) throws IOException;

    T Deserialize();
}
