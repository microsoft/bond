// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

/**
 * Denotes a Bond enumeration type.
 * All generated Bond enum classes implement this interface.
 * @param <TEnum> the enumerated type that implements this interface
 */
public interface BondEnum<TEnum extends BondEnum<TEnum>> extends Comparable<TEnum> {

    /**
     * Returns the underlying integer value.
     *
     * @return the integer value
     */
    int getValue();

    /**
     * Returns the {@link BondType} type descriptor for the current enum type.
     *
     * @return the type descriptor
     */
    EnumBondType<TEnum> getBondType();
}
