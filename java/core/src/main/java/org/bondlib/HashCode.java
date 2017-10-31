// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

/**
 * Implements logic of computing hash codes for {@link BondType} class hierarchy.
 * The purpose is to provide a good distribution of hash values, which is why
 * different container types use different functions/constants, so that for
 * example lists have different hash codes of sets, vectors, etc.
 */
final class HashCode {

    static int computeHashCodeForBondedContainer(StructBondType<?> valueType) {
        int valueTypeHashCode = valueType.hashCode();
        // flip the bits (can't have bonded<bonded<>> so hash can't cancel out
        return ~valueTypeHashCode;
    }

    static int computeHashCodeForNullableContainer(BondType<?> valueType) {
        int valueTypeHashCode = valueType.hashCode();
        // times a prime number and shift a few upper bits to lower position
        return (valueTypeHashCode * 3) ^ (valueTypeHashCode >>> 29);
    }

    static int computeHashCodeForListContainer(BondType<?> elementType) {
        int elementTypeHashCode = elementType.hashCode();
        // times a prime number and shift that many bits to lower position
        return (elementTypeHashCode * 5) ^ (elementTypeHashCode >>> 27);
    }

    static int computeHashCodeForVectorContainer(BondType<?> elementType) {
        int elementTypeHashCode = elementType.hashCode();
        // times a prime number and shift that many bits to lower position
        return (elementTypeHashCode * 7) ^ (elementTypeHashCode >>> 25);
    }

    static int computeHashCodeForSetContainer(BondType<?> elementType) {
        int elementTypeHashCode = elementType.hashCode();
        // times a prime number and shift that many bits to lower position
        return (elementTypeHashCode * 11) ^ (elementTypeHashCode >>> 21);
    }

    static int computeHashCodeForMapContainer(PrimitiveBondType<?> keyType, BondType<?> valueType) {
        int keyTypeHashCode = keyType.hashCode();
        int valueTypeHashCode = valueType.hashCode();
        // times a prime number and shift that many bits to lower position, for both key and value
        return (keyTypeHashCode * 5) ^ (keyTypeHashCode >>> 27) ^
                (valueTypeHashCode * 7) ^ (valueTypeHashCode >>> 25);
    }
}
