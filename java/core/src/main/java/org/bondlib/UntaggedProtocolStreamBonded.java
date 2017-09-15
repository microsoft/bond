// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * An implementation of {@link Bonded} backed by a tagged protocol reader, which effectively
 * implements delayed deserialization for untagged protocols.
 *
 * @param <T> the struct type of this Bonded instance or {@link BondSerializable} if unspecified
 */
final class UntaggedProtocolStreamBonded<T extends BondSerializable> extends Bonded<T> {

    private final UntaggedProtocolReader protocolReader;
    private final StructBondType<T> bondType;

    UntaggedProtocolStreamBonded(UntaggedProtocolReader protocolReader, StructBondType<T> bondType) {
        this.protocolReader = protocolReader;
        this.bondType = bondType;
    }

    @Override
    public final StructBondType<T> getBondType() {
        return this.bondType;
    }

    @Override
    public void serialize(ProtocolWriter protocolWriter) throws IOException {
        ArgumentHelper.ensureNotNull(protocolWriter, "protocolWriter");
        if (this.bondType == null) {
            throw new InvalidBondDataException("Cannot serialize an unknown struct type within a Bonded instance.");
        }
        // deserialize into object using object-backed temporary instance, then serialize it
        Bonded.fromObject(this.deserialize(), this.bondType).serialize(protocolWriter);
    }

    @Override
    void serialize(BondType.SerializationContext context) throws IOException {
        if (this.bondType == null) {
            throw new InvalidBondDataException("Cannot serialize an unknown struct type within a Bonded instance.");
        }
        // deserialize into object using object-backed temporary instance, then serialize it
        Bonded.fromObject(this.deserialize(), this.bondType).serialize(context);
    }

    @Override
    public <U extends BondSerializable>
    void serialize(ProtocolWriter protocolWriter, StructBondType<U> asBondType) throws IOException {
        ArgumentHelper.ensureNotNull(protocolWriter, "protocolWriter");
        ArgumentHelper.ensureNotNull(asBondType, "asBondType");
        // deserialize into object using object-backed temporary instance, then serialize it
        Bonded.fromObject(this.deserialize(asBondType), asBondType).serialize(protocolWriter);
    }

    @Override
    public T deserialize() throws IOException {
        if (this.bondType == null) {
            throw new InvalidBondDataException("Cannot deserialize an unknown struct type within a Bonded instance.");
        }
        UntaggedProtocolReader clonedProtocolReader = this.protocolReader.cloneProtocolReader();
        return this.bondType.deserialize(clonedProtocolReader);
    }

    @Override
    public <U extends BondSerializable> U deserialize(StructBondType<U> toBondType) throws IOException {
        ArgumentHelper.ensureNotNull(toBondType, "toBondType");
        // Reinterpret the data without type checking, deferring errors to the deserialization logic
        UntaggedProtocolReader clonedProtocolReader = this.protocolReader.cloneProtocolReader();
        return toBondType.deserialize(clonedProtocolReader);
    }

    @Override
    public <U extends BondSerializable> Bonded<U> convert(StructBondType<U> toBondType) {
        ArgumentHelper.ensureNotNull(toBondType, "toBondType");
        // Reinterpret the data without type checking, deferring errors to the deserialization logic
        return new UntaggedProtocolStreamBonded<U>(this.protocolReader, toBondType);
    }
}
