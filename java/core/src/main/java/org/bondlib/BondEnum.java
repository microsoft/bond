// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

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
     * Returns the label of this enum value if specified or null otherwise.
     * The label is defined as the name of the first declared enum constant assigned to a particular integer value.
     * Subsequent declarations of constants assigned to the same integer value are interpreted as aliases for the
     * first declared constant and use its label. Enum values that were not declared do not have a label and this
     * method returns null for them.
     *
     * @return the label if specified or null otherwise
     */
    String getLabel();

    /**
     * Returns the {@link BondType} type descriptor for the current enum type.
     *
     * @return the type descriptor
     */
    EnumBondType<TEnum> getBondType();
}
