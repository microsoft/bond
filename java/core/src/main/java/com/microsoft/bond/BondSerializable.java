// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

/**
 * Denotes a Bond enumeration type.
 * All generated Bond struct classes implement this interface.
 */
public interface BondSerializable {

    /**
     * Returns the type descriptor object for the current struct.
     *
     * @return the type descriptor object
     */
    StructBondType<? extends BondSerializable> getStruct();
}
