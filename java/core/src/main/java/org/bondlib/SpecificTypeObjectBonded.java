// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * An implementation of {@link Bonded} backed by an object of a specific type that is known
 * when the Bonded instance is constructed.
 *
 * @param <T> the specific struct type of this Bonded instance
 */
final class SpecificTypeObjectBonded<T extends BondSerializable> extends Bonded<T> {

    private final T objectInstance;
    private final StructBondType<T> bondType;

    SpecificTypeObjectBonded(T objectInstance, StructBondType<T> bondType) {
        this.objectInstance = objectInstance;
        this.bondType = bondType;
    }

    @Override
    public final StructBondType<T> getBondType() {
        return this.bondType;
    }

    @Override
    public void serialize(ProtocolWriter protocolWriter) throws IOException {
        ArgumentHelper.ensureNotNull(protocolWriter, "protocolWriter");
        this.bondType.serialize(this.objectInstance, protocolWriter);
    }

    @Override
    void serialize(BondType.SerializationContext context) throws IOException {
        this.bondType.serializeValue(context, this.objectInstance);
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
        return this.bondType.cloneValue(this.objectInstance);
    }

    @Override
    public <U extends BondSerializable>
    U deserialize(StructBondType<U> asBondType) throws IOException {
        ArgumentHelper.ensureNotNull(asBondType, "asBondType");
        if (this.bondType.isSubtypeOf(asBondType)) {
            // we can always clone from a base struct type by slicing
            @SuppressWarnings("unchecked")
            U castObjectInstance = (U) this.objectInstance;
            return asBondType.cloneValue(castObjectInstance);
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
