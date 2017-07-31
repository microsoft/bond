// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import com.microsoft.bond.helpers.ArgumentHelper;
import com.microsoft.bond.protocol.ProtocolWriter;
import com.microsoft.bond.protocol.TaggedProtocolReader;

import java.io.IOException;
import java.io.InputStream;

/**
 * A construct representing a Bonded payload of some struct type.
 *
 * @param <T> the struct type of this Bonded instance
 */
public abstract class Bonded<T extends BondSerializable> {

    /**
     * Creates a new Bonded instance from a protocol reader that is constrained to a given struct type.
     *
     * @param protocolReader protocol reader containing the serialized representation of this Bonded value
     * @param bondType       the type descriptor of the struct type
     * @param <T>            the struct type
     * @return new Bonded instance backed by the protocol reader
     * @throws IllegalArgumentException if an argument is null
     */
    public static <T extends BondSerializable> Bonded<T> fromProtocolReader(
            TaggedProtocolReader protocolReader, StructBondType<T> bondType) {
        ArgumentHelper.ensureNotNull(protocolReader, "protocolReader");
        ArgumentHelper.ensureNotNull(bondType, "bondType");
        return new TaggedProtocolStreamBonded<T>(protocolReader, bondType);
    }

    /**
     * Creates a new Bonded instance from a protocol reader that is unconstrained by any struct types.
     *
     * @param protocolReader protocol reader containing the serialized representation of this Bonded value
     * @return new Bonded instance backed by the protocol reader
     * @throws IllegalArgumentException if an argument is null
     */
    public static Bonded<?> fromProtocolReader(TaggedProtocolReader protocolReader) {
        ArgumentHelper.ensureNotNull(protocolReader, "protocolReader");
        return new TaggedProtocolStreamBonded<BondSerializable>(protocolReader, null);
    }

    /**
     * Creates a new Bonded instance from an instance of a struct type, with the given underlying Bond type.
     *
     * @param objectInstance the object backing this Bonded
     * @param bondType       the type descriptor of the struct type
     * @param <T>            the struct type
     * @return new Bonded instance backed by the object
     * @throws IllegalArgumentException if an argument is null
     */
    public static <T extends BondSerializable> Bonded<T> fromObject(T objectInstance, StructBondType<T> bondType) {
        ArgumentHelper.ensureNotNull(objectInstance, "objectInstance");
        ArgumentHelper.ensureNotNull(bondType, "bondType");
        return new SpecificTypeObjectBonded<T>(objectInstance, bondType);
    }

    /**
     * Creates a new Bonded instance from an instance of a struct type, with the underlying Bond type obtained
     * from the instance by calling the {@link BondSerializable#getBondType()} method.
     *
     * @param objectInstance the object backing this Bonded
     * @param <T>            the struct type
     * @return new Bonded instance backed by the object
     * @throws IllegalArgumentException if an argument is null
     */
    public static <T extends BondSerializable> Bonded<? extends T> fromObject(T objectInstance) {
        ArgumentHelper.ensureNotNull(objectInstance, "objectInstance");
        return new WildcardTypeObjectBonded<T>(objectInstance);
    }

    /**
     * Returns the default type descriptor associated with this Bonded instance or null if unspecified.
     * The type descriptor determines the behavior of this Bonded instance,
     * such as allowed conversion rules and default serialization/deserialization.
     *
     * @return the Bond type descriptor or null if unspecified.
     */
    public abstract StructBondType<? extends T> getBondType();

    /**
     * Serializes content of this Bonded instance to a protocol writer using the default Bond type descriptor.
     * Throws an exception if there is no default type descriptor associated with this Bonded instance.
     *
     * @param protocolWriter the protocol writer
     * @throws IOException if an I/O error occurred during serialization
     */
    public abstract void serialize(ProtocolWriter protocolWriter) throws IOException;

    /**
     * Serializes content of this Bonded instance to a protocol writer using provided Bond type descriptor.
     *
     * @param protocolWriter the protocol writer
     * @param asBondType     type descriptor for the Bond struct type to serialize as
     * @param <U>            the Bond struct type to serialize as
     * @throws IOException              if an I/O error occurred during serialization
     * @throws IllegalArgumentException if an argument is null
     */
    public abstract <U extends BondSerializable> void
    serialize(ProtocolWriter protocolWriter, StructBondType<U> asBondType) throws IOException;

    /**
     * Deserializes content of tbis Bonded instance as an object of the default Bond struct type.
     *
     * @return a new instance deserialized from this Bonded
     * @throws IOException if an I/O error occurred during deserialization
     */
    public abstract T deserialize() throws IOException;

    /**
     * Deserializes content of tbis Bonded instance as an object of provided Bond struct type.
     *
     * @param asBondType type descriptor for the Bond struct type to deserialize as
     * @param <U>        the Bond struct type to deserialize as
     * @return a new instance deserialized from this Bonded
     * @throws IOException              if an I/O error occurred during deserialization
     * @throws IllegalArgumentException if an argument is null
     */
    public abstract <U extends BondSerializable> U
    deserialize(StructBondType<U> asBondType) throws IOException;

    /**
     * Tries to convert this Bonded to a Bonded constrained to another type, and returns the new Bonded instance if the
     * conversion was successful or null otherwise. Please note that a successful conversion of Bonded instance does
     * not promise successful deserialization which may still fail when {@link #deserialize(StructBondType)} is called.
     *
     * @param toBondType type descriptor for the Bond struct type to convert to
     * @param <U>        the Bond struct type to convert to
     * @return a new Bonded instance constrained to the given type or null if this conversion is not possible
     */
    public abstract <U extends BondSerializable> Bonded<U> convert(StructBondType<U> toBondType);
}
