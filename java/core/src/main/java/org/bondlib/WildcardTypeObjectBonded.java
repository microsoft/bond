// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * An implementation of {@link Bonded} backed by an object of an unknown type constrained by a wildcard.
 *
 * @param <T> the constraining struct type of this Bonded instance
 */
final class WildcardTypeObjectBonded<T extends BondSerializable> extends Bonded<T> {

    // Invariant condition: the actual (most derived) class of objectInstance is always the same as the class
    // which bondType represents, i.e. objectInstance.getClass() == bondType.getValueClass() is always true.
    // This guarantees the safety of the casts of StructBondType performed in serialize and deserialize methods.
    private final T objectInstance;
    private final StructBondType<? extends T> bondType;

    WildcardTypeObjectBonded(T objectInstance) {
        this.objectInstance = objectInstance;
        @SuppressWarnings("unchecked")
        StructBondType<? extends T> bondType = (StructBondType<? extends T>) objectInstance.getBondType();
        this.bondType = bondType;
    }

    @Override
    public final StructBondType<? extends T> getBondType() {
        return this.bondType;
    }

    @Override
    public void serialize(ProtocolWriter protocolWriter) throws IOException {
        ArgumentHelper.ensureNotNull(protocolWriter, "protocolWriter");
        @SuppressWarnings("unchecked")
        StructBondType<BondSerializable> castBondType = (StructBondType<BondSerializable>) this.bondType;
        castBondType.serialize(this.objectInstance, protocolWriter);
    }

    @Override
    void serialize(BondType.SerializationContext context) throws IOException {
        @SuppressWarnings("unchecked")
        StructBondType<BondSerializable> castBondType = (StructBondType<BondSerializable>) this.bondType;
        castBondType.serializeValue(context, this.objectInstance);
    }

    @Override
    public <U extends BondSerializable>
    void serialize(ProtocolWriter protocolWriter, StructBondType<U> asBondType) throws IOException {
        ArgumentHelper.ensureNotNull(protocolWriter, "protocolWriter");
        ArgumentHelper.ensureNotNull(asBondType, "asBondType");
        if (this.bondType.isSubtypeOf(asBondType)) {
            // we can always serialize the base struct type portion by slicing
            @SuppressWarnings("unchecked")
            U castObjectInstance = (U) this.objectInstance;
            asBondType.serialize(castObjectInstance, protocolWriter);
        } else {
            // but we need to transcode in all other cases
            // TODO: complete this implementation after transcoding is implemented
            throw new UnsupportedOperationException(
                    "Serialization as a non-ancestor struct type is not currently implemented.");
        }
    }

    @Override
    public T deserialize() throws IOException {
        @SuppressWarnings("unchecked")
        StructBondType<BondSerializable> castBondType = (StructBondType<BondSerializable>) this.bondType;
        @SuppressWarnings("unchecked")
        T clonedObjectInstance = (T) castBondType.cloneValue(this.objectInstance);
        return clonedObjectInstance;
    }

    @Override
    public <U extends BondSerializable> U deserialize(StructBondType<U> toBondType) throws IOException {
        ArgumentHelper.ensureNotNull(toBondType, "toBondType");
        if (this.bondType.isSubtypeOf(toBondType)) {
            // we can always clone from a base struct type by slicing
            @SuppressWarnings("unchecked")
            U castObjectInstance = (U) this.objectInstance;
            return toBondType.cloneValue(castObjectInstance);
        } else {
            // but we need to transcode in all other cases
            // TODO: complete this implementation after transcoding is implemented
            throw new UnsupportedOperationException(
                    "Deserialization as a non-ancestor struct type is not currently implemented.");
        }
    }

    @Override
    public <U extends BondSerializable> Bonded<U> convert(StructBondType<U> toBondType) {
        ArgumentHelper.ensureNotNull(toBondType, "toBondType");
        if (this.bondType.isSubtypeOf(toBondType)) {
            // we can always convert to a base struct type by upcasting
            @SuppressWarnings("unchecked")
            U castObjectInstance = (U) this.objectInstance;
            return new SpecificTypeObjectBonded<U>(castObjectInstance, toBondType);
        } else {
            // but we can't convert to any other type
            return null;
        }
    }
}
