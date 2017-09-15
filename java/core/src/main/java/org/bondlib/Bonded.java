// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * A construct representing a Bonded payload of some struct type.
 *
 * The type parameter is expected to implement {@link BondSerializable} but this is not enforced at compile-time
 * since the details of the type may not be known in advance (e.g. consider a generic type that contains a bonded
 * field wrapping an instance of an unknown type specified through a type parameter). It shall be noted that
 * although this class doesn't explicitly constrain its type parameter, all public API that creates {@link Bonded}
 * instances does have this enforcement.
 *
 * @param <T> the struct type of this Bonded instance which is expected to implement {@link BondSerializable}
 */
public abstract class Bonded<T> {

    /**
     * Creates a new Bonded instance from a tagged protocol reader that is constrained to a given struct type.
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
     * Creates a new Bonded instance from a tagged protocol reader that is unconstrained by any struct types.
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
     * Creates a new Bonded instance from an untagged protocol reader that is constrained to a given struct type.
     *
     * @param protocolReader protocol reader containing the serialized representation of this Bonded value
     * @param bondType       the type descriptor of the struct type
     * @param <T>            the struct type
     * @return new Bonded instance backed by the protocol reader
     * @throws IllegalArgumentException if an argument is null
     */
    public static <T extends BondSerializable> Bonded<T> fromProtocolReader(
        UntaggedProtocolReader protocolReader, StructBondType<T> bondType) {
        ArgumentHelper.ensureNotNull(protocolReader, "protocolReader");
        ArgumentHelper.ensureNotNull(bondType, "bondType");
        return new UntaggedProtocolStreamBonded<T>(protocolReader, bondType);
    }

    /**
     * Creates a new Bonded instance from an untagged protocol reader that is unconstrained by any struct types.
     *
     * @param protocolReader protocol reader containing the serialized representation of this Bonded value
     * @return new Bonded instance backed by the protocol reader
     * @throws IllegalArgumentException if an argument is null
     */
    public static Bonded<?> fromProtocolReader(UntaggedProtocolReader protocolReader) {
        ArgumentHelper.ensureNotNull(protocolReader, "protocolReader");
        return new UntaggedProtocolStreamBonded<BondSerializable>(protocolReader, null);
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
     * Serializes content of this Bonded instance to using the given context and the default Bond type descriptor.
     * Throws an exception if there is no default type descriptor associated with this Bonded instance.
     * This method is called when this Bonded is serialized as a field of another object, as opposed to top-level
     * serializaton. For the top-level serialization, use the {@link #serialize(ProtocolWriter)} method.
     *
     * @param context the serialization context
     * @throws IOException if an I/O error occurred during serialization
     */
    abstract void serialize(BondType.SerializationContext context) throws IOException;

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
     * Deserializes content of this Bonded instance as an object of the default Bond struct type.
     * Throws an exception if there is no default type descriptor associated with this Bonded instance.
     *
     * @return a new instance deserialized from this Bonded
     * @throws IOException if an I/O error occurred during deserialization
     */
    public abstract T deserialize() throws IOException;

    /**
     * Deserializes content of this Bonded instance as an object of provided Bond struct type.
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
     * If you need to avoid slicing, see {@link #cast(StructBondType)}.
     *
     * @param toBondType type descriptor for the Bond struct type to convert to
     * @param <U>        the Bond struct type to convert to
     * @return a new Bonded instance constrained to the given type or null if this conversion is not possible
     */
    public abstract <U extends BondSerializable> Bonded<U> convert(StructBondType<U> toBondType);

    /**
     * Returns this instance of Bonded, cast to a Bonded constrained to another type. The other type must be assignable
     * from the current Bond type, and this method will throw if it isn't.
     *
     * Note that this preserves the underlying Bond type, meaning that a Bonded&lt;T&gt; that has been cast to a
     * Bonded&lt;U&gt; will still serialize and deserialize as a T.
     *
     * If you need to slice, see {@link #convert(StructBondType)}.
     *
     * @param castBondType type descriptor for the Bond struct type to cast to
     * @param <U>        the Bond struct type to cast to
     * @return this Bonded instance, cast to the given type
     * @throws ClassCastException if U is not assignable from T
     */
    public <U extends BondSerializable> Bonded<U> cast(StructBondType<U> castBondType) {
        if (!(getBondType().isSubtypeOf(castBondType))) {
            final String err = String.format("Bonded<%s> cannot be cast to Bonded<%s>",
                getBondType().getQualifiedName(), castBondType.getQualifiedName()
            );
            throw new ClassCastException(err);
        }

        @SuppressWarnings("unchecked")
        final Bonded<U> casted = (Bonded<U>) this;
        return casted;
    }

    /**
     * Equality on Bonded and Bonded subtypes is always reference equality
     * (i.e., the same as Object.equals()). The semantics of .equals() in
     * codegen'd types depend on this.
     *
     * This is reimplemented as a final override here to enforce it on Bonded
     * subtypes.
     */
    @Override
    public final boolean equals(Object other) {
        return this == other;
    }

    /**
     * Implemented as a final override to ensure consistency with .equals() on
     * Bonded and Bonded subtypes. See {@link Bonded#equals(Object)}.
     *
     * @return see {@link Object#hashCode()}
     */
    @Override
    public final int hashCode() {
        return super.hashCode();
    }
}
