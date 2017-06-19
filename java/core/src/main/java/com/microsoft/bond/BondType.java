// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import com.microsoft.bond.helpers.ArgumentHelper;
import com.microsoft.bond.protocol.ProtocolWriter;
import com.microsoft.bond.protocol.TaggedProtocolReader;

import java.io.IOException;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Represents a type in the Bond type system and implements behavior specific to that type, such as
 * schema inference, initialization, serialization and deserialization. A Bond type cane be a primitive,
 * a struct, a specialization of a generic type (including containers, nullable, or generic Bond structs).
 * @param <T> the class of the value
 */
public abstract class BondType<T> {

    // The global type cache
    static final TypeCache typeCache = new TypeCache();

    // Registry of struct type resolvers; entries are added by static class initializers
    private static final ConcurrentHashMap<
            Class<? extends BondSerializable>,
            StructBondTypeResolver<? extends BondSerializable>> structTypeResolverRegistry =
            new ConcurrentHashMap<
                    Class<? extends BondSerializable>,
                    StructBondTypeResolver<? extends BondSerializable>>();

    // Registry of enum types; entries are added by static class initializers
    private static final ConcurrentHashMap<
            Class<? extends BondEnum>,
            EnumBondType<? extends BondEnum>> enumTypeRegistry =
            new ConcurrentHashMap<
                    Class<? extends BondEnum>,
                    EnumBondType<? extends BondEnum>>();

    // precomputed hash code helps to speed up object retrieval from the type cache
    private final int precomputedHashCode;

    // restrict subclasses to the current package
    BondType(int precomputedHashCode) {
        this.precomputedHashCode = precomputedHashCode;
    }

    /**
     * Gets the simple name of the Bond type, which excludes the namespace.
     * The names of generic type parameters, if any, are not included in the name.
     *
     * @return short simple type name
     */
    public abstract String getName();

    /**
     * Gets the qualified name of the Bond type, which includes the namespace.
     * The names of generic type parameters, if any, are not included in the name.
     *
     * @return short namespace-qualified type name
     */
    public abstract String getQualifiedName();

    /**
     * Gets the full name of the Bond type, which includes namespaces and generic type parameters.
     * This method is used in exception reporting.
     *
     * @return full type name
     */
    public final String getFullName() {
        StringBuilder sb = new StringBuilder();
        sb.append(this.getQualifiedName());
        BondType<?>[] typeArguments = this.getGenericTypeArguments();
        if (typeArguments != null) {
            sb.append("<");
            sb.append(typeArguments[0].getFullName());
            for (int i = 1; i < typeArguments.length; ++i) {
                sb.append(", ");
                sb.append(typeArguments[i].getFullName());
            }
            sb.append(">");
        }
        return sb.toString();
    }

    /**
     * Gets the {@link BondDataType} value for this type.
     *
     * @return the bond data type enumeration value
     */
    public abstract BondDataType getBondDataType();

    /**
     * Gets the {@link Class} instance for the values (objects) of this type.
     * The actual class of a value of this Bond type may not be the same as the returned class,
     * but its is always assignable to it (i.e. a subclass or a Java primitive type).
     *
     * @return the class object
     */
    public abstract Class<T> getValueClass();

    /**
     * Gets the {@link Class} instance for the Java primitive values (non-objects) of this type
     * or null if a Java primitive type does not exist. This method complements the
     * {@link BondType#getValueClass()} method which returns the classes for object instances.
     *
     * @return the class object for a primitive type or null if a primitive type doesn't exists
     */
    public abstract Class<T> getPrimitiveValueClass();

    /**
     * Gets a value indicating whether values of this type can be assigned to null.
     *
     * @return whether value of this type can be assigned to null
     */
    public abstract boolean isNullableType();

    /**
     * Indicates whether this type is a generic type that is specialized with one or more generic type arguments.
     *
     * @return true if this type is a specialization of a generic type
     */
    public abstract boolean isGenericType();

    /**
     * Gets a new array instance containing the type descriptors of the generic type arguments or null
     * if this type is not a specialization of a generic type.
     *
     * @return an array containing the descriptors of the generic type arguments or null
     */
    public abstract BondType<?>[] getGenericTypeArguments();

    /**
     * Returns the default value of this type as a shared instance for immutable primitive types
     * or a new instance for all other types. For non-nullable structs and containers this method
     * returns an initialized value equivalent to invoking the default constructor. For nullable
     * values this method returns null.
     *
     * @return the default initialized value of this type
     */
    protected abstract T newDefaultValue();

    /**
     * Serializes a value of this type into a protocol writer.
     * This method is not recommended for Java primitive (non-object) types. To serialize primitive
     * values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize
     * @throws IOException if an I/O error occurred
     */
    protected abstract void serializeValue(SerializationContext context, T value) throws IOException;

    /**
     * Deserializes a value of this type from a tagged protocol reader.
     * This method is not recommended for Java primitive (non-object) types. To deserialize primitive
     * values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected abstract T deserializeValue(TaggedDeserializationContext context) throws IOException;

    /**
     * Serializes a struct field of this type into a protocol writer, including field metadata.
     * This method is not recommended for fields with Java primitive (non-object) types. To serialize fields with
     * primitive values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize
     * @param field   descriptor of the field
     * @throws IOException if an I/O error occurred
     */
    protected abstract void serializeField(
            SerializationContext context,
            T value,
            StructBondType.StructField<T> field) throws IOException;

    /**
     * Serializes a struct field of this type with "nothing" as the default value into a protocol writer,
     * including field metadata.
     * This method is not recommended for fields with Java primitive (non-object) types. To serialize fields with
     * primitive values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize
     * @param field   descriptor of the field
     * @throws IOException if an I/O error occurred
     */
    protected final void serializeSomethingField(
            SerializationContext context,
            SomethingObject<T> value,
            StructBondType.StructField<T> field) throws IOException {
        if (value != null) {
            serializeField(context, value.getValue(), field);
        }
    }

    /**
     * Deserializes a struct field of this type from a tagged protocol reader, excluding field metadata
     * which is assumed to be already deserialized earlier and whose value is available in the
     * {@link TaggedDeserializationContext#readFieldResult} field of the passed context argument.
     * This method is not recommended for fields with Java primitive (non-object) types. To deserialize fields with
     * primitive values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the deserialization
     * @param field   descriptor of the field
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected abstract T deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<T> field) throws IOException;

    /**
     * Deserializes a struct field of this type with "nothing" as the default value from a tagged protocol reader,
     * excluding field metadata which is assumed to be already deserialized earlier and whose value is available
     * in the {@link TaggedDeserializationContext#readFieldResult} field of the passed context argument.
     * This method is not recommended for fields with Java primitive (non-object) types. To deserialize fields with
     * primitive values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the deserialization
     * @param field   descriptor of the field
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected final SomethingObject<T> deserializeSomethingField(
            TaggedDeserializationContext context,
            StructBondType.StructField<T> field) throws IOException {
        return Something.wrap(this.deserializeField(context, field));
    }

    @Override
    public final int hashCode() {
        return this.precomputedHashCode;
    }

    @Override
    public final boolean equals(Object obj) {
        if (this == obj) {
            return true;
        } else if (obj == null) {
            return false;
        } else if (this.getClass() != obj.getClass()) {
            return false;
        } else {
            BondType<?> that = (BondType<?>) obj;
            if (this.precomputedHashCode != that.precomputedHashCode) {
                return false;
            } else {
                return this.equalsInternal(that);
            }
        }
    }

    // helper for subclasses to compute hash code
    static int multiplyAndShift(int value, int n) {
        return (value * n) ^ (value >>> (32 - n));
    }

    // implemented by subclasses to test type decriptors for equality
    // (the argument is not null and can be safely cast to the actual subclass)
    abstract boolean equalsInternal(BondType<?> obj);

    @Override
    public final String toString() {
        return this.getFullName();
    }

    // contains tuple (struct def, zero-based position it was discovered when traversing type tree)
    static final class StructDefOrdinalTuple {
        final StructDef structDef;
        final int ordinal;

        StructDefOrdinalTuple(StructDef structDef, int ordinal) {
            this.structDef = structDef;
            this.ordinal = ordinal;
        }
    }

    // a helper to create schema; initializes a new type def instance and
    // maintains a hash map of all distinct struct defs discovered so far
    abstract TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap);

    // validates non-nullable value
    final void verifyNonNullableValueIsNotSetToNull(
            Object value) throws InvalidBondDataException {
        if (value == null) {
            Throw.raiseNonNullableValueSetTuNullError(this.getFullName());
        }
    }

    // validates field during serialization
    final void verifySerializedNonNullableFieldIsNotSetToNull(
            Object value,
            StructBondType.StructField<?> field) throws InvalidBondDataException {
        // delegate to the value verification method and chain the thrown exception (if any);
        // this approach lets to preserve the original exception pertaining to the null value,
        // wrapped by the exception specifically pertaining to the failure to serialize a field
        try {
            this.verifyNonNullableValueIsNotSetToNull(value);
        } catch (InvalidBondDataException e) {
            Throw.raiseStructFieldSerializationError(false, field, e, null);
        }
    }

    /**
     * Gets a type descriptor for the Bond "nullable" container type.
     * Nullable values are represented by the same class but may be set to null.
     *
     * @param valueType a type descriptor for the underlying value class
     * @param <TValue>  the class of the underlying value
     * @return a type descriptor instance
     */
    public static <TValue> NullableBondType<TValue> nullableOf(
            BondType<TValue> valueType) {
        ArgumentHelper.ensureNotNull(valueType, "valueType");
        return (NullableBondType<TValue>) typeCache.get(new NullableBondType<TValue>(valueType));
    }

    /**
     * Gets a type descriptor for the Bond "bonded" container type.
     *
     * @param valueType a type descriptor for the underlying value class
     * @param <TStruct> the class of the underlying struct value
     * @return a type descriptor instance
     */
    public static <TStruct extends BondSerializable> BondedBondType<TStruct> bondedOf(
            StructBondType<TStruct> valueType) {
        ArgumentHelper.ensureNotNull(valueType, "valueType");
        return (BondedBondType<TStruct>) typeCache.get(new BondedBondType<TStruct>(valueType));
    }

    /**
     * Gets a type descriptor for the Bond "vector" container type.
     *
     * @param elementType a type descriptor for the element value class
     * @param <TElement>  the class of the element values
     * @return a type descriptor instance
     */
    public static <TElement> VectorBondType<TElement> vectorOf(
            BondType<TElement> elementType) {
        ArgumentHelper.ensureNotNull(elementType, "elementType");
        return (VectorBondType<TElement>) typeCache.get(new VectorBondType<TElement>(elementType));
    }

    /**
     * Gets a type descriptor for the Bond "list" container type.
     *
     * @param elementType a type descriptor for the element value class
     * @param <TElement>  the class of the element values
     * @return a type descriptor instance
     */
    public static <TElement> ListBondType<TElement> listOf(
            BondType<TElement> elementType) {
        ArgumentHelper.ensureNotNull(elementType, "elementType");
        return (ListBondType<TElement>) typeCache.get(new ListBondType<TElement>(elementType));
    }

    /**
     * Gets a type descriptor for the Bond "set" container type.
     *
     * @param elementType a type descriptor for the element value class
     * @param <TElement>  the class of the element values
     * @return a type descriptor instance
     */
    public static <TElement> SetBondType<TElement> setOf(
            PrimitiveBondType<TElement> elementType) {
        ArgumentHelper.ensureNotNull(elementType, "elementType");
        return (SetBondType<TElement>) typeCache.get(new SetBondType<TElement>(elementType));
    }

    /**
     * Gets a type descriptor that denotes a generic Bond "map" container type.
     *
     * @param keyType   a type descriptor for the map key class
     * @param valueType a type descriptor for the mapped values class
     * @param <TKey>    the class of the map keys
     * @param <TValue>  the class of the mapped values
     * @return a type descriptor instance
     */
    public static <TKey, TValue> MapBondType<TKey, TValue> mapOf(
            PrimitiveBondType<TKey> keyType, BondType<TValue> valueType) {
        ArgumentHelper.ensureNotNull(keyType, "keyType");
        ArgumentHelper.ensureNotNull(valueType, "valueType");
        return (MapBondType<TKey, TValue>) typeCache.get(new MapBondType<TKey, TValue>(keyType, valueType));
    }

    /**
     * Retrieves type resolver for the given struct class, or null if none exists.
     *
     * @param clazz     the struct class
     * @param <TStruct> the Bond struct value class
     * @return type resolver or null
     */
    public static <TStruct extends BondSerializable> StructBondType<TStruct> getStructType(
            Class<TStruct> clazz, BondType<?>... genericTypeArguments) {
        ArgumentHelper.ensureNotNull(clazz, "clazz");
        ArgumentHelper.ensureNotNull(genericTypeArguments, "genericTypeArguments");
        @SuppressWarnings("unchecked")
        StructBondTypeResolver<TStruct> structTypeResolver =
                (StructBondTypeResolver<TStruct>) structTypeResolverRegistry.get(clazz);
        if (structTypeResolver != null) {
            return structTypeResolver.resolveAndInitialize(genericTypeArguments);
        } else {
            return null;
        }
    }

    /**
     * Registers a struct type resolver from generated code.
     *
     * @param clazz              the struct class
     * @param structTypeResolver type resolver for the given struct type
     * @param <TStruct>          the Bond struct value class
     */
    protected static <TStruct extends BondSerializable> void registerStructType(
            Class<TStruct> clazz, StructBondTypeResolver<TStruct> structTypeResolver) {
        structTypeResolverRegistry.put(clazz, structTypeResolver);
    }

    /**
     * Retrieves type descriptor for the given enum class, or null if none exists.
     *
     * @param clazz   the enum class
     * @param <TEnum> the Bond enum value class
     * @return type descriptor or null
     */
    public static <TEnum extends BondEnum> EnumBondType<TEnum> getEnumType(Class<TEnum> clazz) {
        ArgumentHelper.ensureNotNull(clazz, "clazz");
        @SuppressWarnings("unchecked")
        EnumBondType<TEnum> enumType = (EnumBondType<TEnum>) enumTypeRegistry.get(clazz);
        return enumType;
    }

    /**
     * Registers an enum type from generated code, also caching it in the type cache.
     *
     * @param clazz    the enum class
     * @param enumType type descriptor for the given enum type
     * @param <TEnum>  the Bond enum value class
     * @return cached instance of the enum type descriptor
     */
    protected static <TEnum extends BondEnum> EnumBondType<TEnum> registerEnumTypeWithCaching(
            Class<TEnum> clazz, EnumBondType<TEnum> enumType) {
        EnumBondType<TEnum> cachedEnumType = (EnumBondType<TEnum>) typeCache.get(enumType);
        enumTypeRegistry.put(clazz, cachedEnumType);
        return cachedEnumType;
    }

    /**
     * Returns a cached type descriptor that is equal to the argument, or caches the argument otherwise.
     *
     * @param type the type descriptor
     * @param <T>  the Bond value class
     * @return a cached type descriptor
     */
    protected static <T> BondType<T> getCachedType(BondType<T> type) {
        return BondType.typeCache.get(type);
    }

    // Implements type cache
    static final class TypeCache {

        // maps a bond type descriptor to itself; used as the primary cache of bond type descriptors
        private final ConcurrentHashMap<BondType<?>, BondType<?>> typeCache =
                new ConcurrentHashMap<BondType<?>, BondType<?>>();

        /**
         * Returns a cached type descriptor that is equal to the argument, or caches the argument otherwise.
         *
         * @param type the type descriptor
         * @param <T>  the Bond value class
         * @return a cached type descriptor
         */
        final <T> BondType<T> get(BondType<T> type) {
            @SuppressWarnings("unchecked")
            BondType<T> cachedValue = (BondType<T>) this.typeCache.putIfAbsent(type, type);
            if (cachedValue == null) {
                cachedValue = type;
            }
            return cachedValue;
        }
    }

    /**
     * Contains runtime state of serialization.
     */
    protected static final class SerializationContext {
        public final ProtocolWriter writer;

        public SerializationContext(ProtocolWriter writer) {
            this.writer = writer;
        }
    }

    /**
     * Contains runtime state of tagged deserialization.
     */
    protected static final class TaggedDeserializationContext {
        public final TaggedProtocolReader reader;

        public final TaggedProtocolReader.ReadFieldResult readFieldResult =
                new TaggedProtocolReader.ReadFieldResult();
        public final TaggedProtocolReader.ReadContainerResult readContainerResult =
                new TaggedProtocolReader.ReadContainerResult();

        public TaggedDeserializationContext(TaggedProtocolReader reader) {
            this.reader = reader;
        }
    }
}
