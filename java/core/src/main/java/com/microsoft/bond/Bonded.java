// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import com.microsoft.bond.helpers.ArgumentHelper;
import com.microsoft.bond.protocol.ProtocolWriter;

import java.io.IOException;
import java.io.InputStream;

/**
 * A construct representing a Bonded payload of some struct type. If the struct type is known in advance,
 * a Bonded instance may be constrained to that type by specifying the type parameter; otherwise, the
 * type parameter may be left unconstrained (i.e. the constraint is just the {@link BondSerializable}).
 *
 * @param <T> constraint on the payload type.
 */
public abstract class Bonded<T extends BondSerializable> {

    private final StructBondType<T> bondType;

    /**
     * Package-private constructor to restrict inheritance to the Bond Core package only.
     *
     * @param bondType the type descriptor of the struct type or null if the type is unknown
     */
    Bonded(StructBondType<T> bondType) {
        this.bondType = bondType;
    }

    /**
     * Returns the type descriptor of the struct type of this Bonded instance or null if the type if unspecified.
     *
     * @return type descriptor or null if unspecified.
     */
    public final StructBondType<T> getBondType() {
        return this.bondType;
    }

    /**
     * Creates a new Bonded instance from an input stream that is constrained to a given struct type.
     *
     * @param inputStream input stream containing the serialized representation of this Bonded value
     * @param bondType    the type descriptor of the struct type
     * @param <T>         the struct type
     * @return new Bonded instance backed by the stream
     * @throws IllegalArgumentException if an argument is null
     */
    public static <T extends BondSerializable> Bonded<T> fromStream(
            InputStream inputStream, StructBondType<T> bondType) {
        ArgumentHelper.ensureNotNull(inputStream, "inputStream");
        ArgumentHelper.ensureNotNull(bondType, "bondType");

        // TODO: return stream-backed implementation with type constraint
        throw new UnsupportedOperationException();
    }

    /**
     * Creates a new Bonded instance from an input stream that is unconstrained by any struct types.
     *
     * @param inputStream input stream containing the serialized representation of this Bonded value
     * @return new Bonded instance backed by the stream
     * @throws IllegalArgumentException if an argument is null
     */
    public static Bonded<?> fromStream(InputStream inputStream) {
        ArgumentHelper.ensureNotNull(inputStream, "inputStream");

        // TODO: return stream-backed implementation without type constraint
        throw new UnsupportedOperationException();
    }

    /**
     * Creates a new Bonded instance from an instance of a struct type.
     *
     * @param objectInstance
     * @param bondType       the type descriptor of the struct type
     * @param <T>            the struct type
     * @return new Bonded instance backed by the object
     * @throws IllegalArgumentException if an argument is null
     */
    public static <T extends BondSerializable> Bonded<T> fromObject(
            T objectInstance, StructBondType<T> bondType) {
        ArgumentHelper.ensureNotNull(objectInstance, "objectInstance");
        ArgumentHelper.ensureNotNull(bondType, "bondType");

        // TODO: return object-backed implementation
        throw new UnsupportedOperationException();
    }

    /**
     * Serializes content of the Bonded instance to a protocol writer.
     *
     * @param protocolWriter the protocol writer
     * @throws IOException if an I/O error occurred during serialization
     */
    public abstract void serialize(ProtocolWriter protocolWriter) throws IOException;

    /**
     * Deserializes content of tbe Bonded instance to an object of the given Bond struct type.
     *
     * @param toBondType type descriptor for the Bond struct type to deserialize into
     * @param <U>        the Bond struct type to deserialize into
     * @return a new instance deserialized from this Bonded
     * @throws IOException if an I/O error occurred during deserialization
     */
    public abstract <U extends BondSerializable> U deserialize(StructBondType<U> toBondType) throws IOException;

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
