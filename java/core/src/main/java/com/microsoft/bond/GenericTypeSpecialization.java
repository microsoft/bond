// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

/**
 * Contains details of generic specialization of a generic struct type.
 */
public final class GenericTypeSpecialization {

    // accessible to StructBondType
    final BondType<?>[] genericTypeArguments;

    /**
     * Initializes object with non-null type arguments, in the declaration order.
     *
     * @param genericTypeArguments type descriptors for the generic type arguments
     */
    public GenericTypeSpecialization(BondType<?>... genericTypeArguments) {
        this.genericTypeArguments = genericTypeArguments;
    }

    /**
     * Gets the generic type argument of the given type at the given position.
     *
     * @param index the position of the argument
     * @param <T>   the type to which to cast the argument
     * @return type descriptor for the argument
     */
    public final <T> BondType<T> getGenericTypeArgument(int index) {
        @SuppressWarnings("unchecked")
        BondType<T> genericTypeArgument = (BondType<T>) this.genericTypeArguments[index];
        return genericTypeArgument;
    }

    @Override
    public final int hashCode() {
        int hash = 0;
        for (BondType<?> genericTypeArgument : this.genericTypeArguments) {
            hash = hash * 31 + genericTypeArgument.hashCode();
        }
        return hash;
    }

    @Override
    public final boolean equals(Object obj) {
        if (obj instanceof GenericTypeSpecialization) {
            GenericTypeSpecialization that = (GenericTypeSpecialization) obj;
            if (this.genericTypeArguments.length == that.genericTypeArguments.length) {
                for (int i = 0; i < this.genericTypeArguments.length; ++i) {
                    if (!this.genericTypeArguments[i].equals(that.genericTypeArguments[i])) {
                        return false;
                    }
                }
            }
        }
        return true;
    }
}