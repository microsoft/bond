// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A type descriptor in the Bond type system that implements behavior specific to a particular type, such
 * as schema inference, initialization, serialization and deserialization. A Bond type can be a primitive,
 * an enum, a struct, or a specialization of a generic type (including containers, nullable, or generic
 * Bond structs). Each Bond type is associated with a single {@link Class} instance of the value, although
 * multiple Bond types can use the same value class (e.g. specializations of a generic type, or using the
 * same Java type to represent multiple primitive types in Bond such as signed vs. unsigned integers).
 * @param <T> the class of the value
 */
public abstract class BondType<T> {

    // The global type cache, which maps a Bond type descriptor to itself and is used to cache type descriptor
    // objects. Caching helps reducing memory footprint when working with generic types such as lists, nullables,
    // or user-defined generic structs. The implementation of type descriptors and generated classes makes sure
    // that there exists only one copy of each type descriptor (excluding short-lived temporary objects).
    //
    // Type descriptor caching is based on identity of a type descriptor, implemented by the equals and hashCode
    // methods, and which consists of the following two items:
    // 1. The Java class that implements the type descriptor (and thus inherits from BondType). The class
    //    identity alone is sufficient for type descriptors of non-generic Bond type since these are singletons.
    // 2. For specializations of generic types, the list of type descriptors for the generic type arguments. Due
    //    to Java type erasure, a Class object alone can't provide information on the generic type arguments and
    //    hence this additional list is necessary to precisely identify a type.
    //
    // Type descriptors for native Bond non-generic types (primitive types and Blob) do not need to be cached
    // since they are accessible from public static fields defined in the BondTypes class. The same is true about
    // generated enum types, whose type descriptors are accessible from public static fields of the generated enum
    // class. Therefore the type cache doesn't contain these type descriptors and implementation makes sure they
    // are never added to the cache (to slightly reduce cache footprint and a chance of hash collisions).
    //
    // Type descriptors for non-generic generated struct types are also accessible from public static fields
    // in their classes and it can be argued as above that they also do not need to be cached. However, these
    // static fields are not used during initialization of struct type descriptors since they may not yet be
    // initialized (e.g. a struct type may reference itself through a nullable field). Therefore, these type
    // descriptors are obtained using protected StructBondType.getStructType method which uses the type cache,
    // meaning that type descriptors for non-generic generated struct types are still cached.
    //
    // Type descriptors of all generic type specializations (either Bond native types or generated user-defined
    // types) are always cached. Methods such as nullableOf, listOf, or makeGenericType always return a cached
    // instance of the type descriptor.
    private static final ConcurrentHashMap<BondType<?>, BondType<?>> typeCache =
            new ConcurrentHashMap<BondType<?>, BondType<?>>();

    /**
     * Package-private constructor (extending BondType hierarchy by user code is not supported).
     */
    BondType() {
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
     * but it is always assignable to it (i.e. a subclass or a Java primitive type).
     * For Bond types that are represented by Java primitive types, this method returns
     * the class of the boxed value (e.g. {@link Long} for int64. To get the class object
     * that actually represents the Java primitive type (i.e. {@link Long#TYPE} for int64),
     * use methog {@link #getPrimitiveValueClass()} instead.
     *
     * @return the class object
     */
    public abstract Class<T> getValueClass();

    /**
     * Gets the {@link Class} instance for the Java primitive values (non-objects) of this type
     * or null if a Java primitive type does not exist. This method complements the
     * {@link #getValueClass()} method which returns the classes for object instances.
     * Thus, for example, this method returns {@link Long#TYPE} for Bond int64 data type whereas
     * the other method returns the class object for the {@link Long} boxed type.
     *
     * @return the class object for a primitive type or null if a primitive type doesn't exists
     */
    public abstract Class<T> getPrimitiveValueClass();

    /**
     * Gets a value indicating whether values of this type can be assigned to null.
     * In Bond, only values explicitly declared as nullable can be assigned to the null value.
     *
     * @return whether value of this type can be assigned to null
     */
    public abstract boolean isNullableType();

    /**
     * Indicates whether this type is a generic type (either a native Bond container or a user-defined struct)
     * that is specialized with one or more generic type arguments. This method returns true if and only if
     * the {@link #isGenericType()} method returns a non-empty array with one or more elements.
     *
     * @return true if this type is a specialization of a generic type
     */
    public abstract boolean isGenericType();

    /**
     * Gets a new array instance containing the type descriptors of one or more generic type arguments or null
     * if this type is not a specialization of a generic type. This method returns null if the type is not
     * generic (i.e. if the {@link #isGenericType() returns false). Please note that this method never returns
     * an empty array since a generic type must have at least one unbound generic type parameter.
     *
     * @return an array containing the descriptors of the generic type arguments or null if not a generic type
     */
    public abstract BondType<?>[] getGenericTypeArguments();

    /**
     * Returns the default value of this type as a shared instance for immutable primitive types
     * or a new instance for all other types. For non-nullable structs and containers this method
     * returns an initialized value equivalent to invoking the public parameterless constructor
     * for collections and non-generic structs, or the public single-argument constructor for generic
     * structs. For nullable values this method always returns null.
     *
     * @return the default initialized value of this type
     */
    protected abstract T newDefaultValue();

    /**
     * Returns a deep clone of the argument value.
     * @param value the argument value.
     * @return a deep clone of the argument.
     */
    protected abstract T cloneValue(T value);

    /**
     * Serializes a value of this type into a protocol writer.
     * This method is intended for objects and is not suitable for Java primitive (non-object) types
     * since its argument is an object and would have to be boxed. To serialize primitive values use the
     * static helper method defined in each singleton class for a primitive Java type.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize (boxed if necessary)
     * @throws IOException if an I/O error occurred
     */
    protected abstract void serializeValue(SerializationContext context, T value) throws IOException;

    /**
     * Deserializes a value of this type from a tagged protocol reader.
     * This method is intended for objects and is not suitable for Java primitive (non-object) types
     * since its return value is an object and would have to be unboxed. To deserialize primitive values use
     * the static helper method defined in each singleton class for a primitive Java type.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value (boxed if necessary)
     * @throws IOException if an I/O error occurred
     */
    protected abstract T deserializeValue(TaggedDeserializationContext context) throws IOException;

    /**
     * Deserializes a value of this type from an untagged protocol reader and a typedef.
     * This method is intended for objects and is not suitable for Java primitive (non-object) types
     * since its return value is an object and would have to be unboxed. To deserialize primitive values use
     * the static helper method defined in each singleton class for a primitive Java type.
     *
     * @param context contains the runtime context of the deserialization
     * @param typeDef the typedef to use for deserialization
     * @return the deserialized value (boxed if necessary)
     * @throws IOException if an I/O error occurred
     */
    protected abstract T deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException;

    /**
     * Serializes a struct field of this type into a protocol writer, including field metadata.
     * This method is intended for objects and is not suitable for fields with Java primitive (non-object)
     * types since its argument is an object and would have to be boxed. To serialize fields with primitive
     * values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize (boxed if necessary)
     * @param field   descriptor of the field
     * @throws IOException if an I/O error occurred
     */
    protected abstract void serializeField(
            SerializationContext context,
            T value,
            StructBondType.StructField<T> field) throws IOException;

    /**
     * Serializes a struct field of this type with "nothing" as the default value into a protocol writer,
     * including field metadata. This method is intended for objects and is not suitable for fields with Java
     * primitive (non-object) types since its argument is a generic {@link SomethingObject} for objects instead
     * of more specific implementation for Java primitive types. To serialize fields with primitive values use
     * the static helper method defined in each singleton class for a primitive type.
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
        } else if (!field.isOptional()) {
            // throws
            Throw.raiseNonOptionalFieldValueSetToNothingError(field);
        }
    }

    /**
     * Deserializes a struct field of this type from a tagged protocol reader, excluding field metadata
     * which is assumed to be already deserialized earlier and whose value is available in the
     * {@link TaggedDeserializationContext#readFieldResult} field of the passed context argument.
     * This method is intended for objects and is not suitable for fields with Java primitive (non-object)
     * types since its return value is an object and would have to be unboxed. To deserialize fields with primitive
     * values use the static helper method defined in each singleton class for a primitive type.
     *
     * @param context contains the runtime context of the deserialization
     * @param field   descriptor of the field
     * @return the deserialized value (boxed if necessary)
     * @throws IOException if an I/O error occurred
     */
    protected abstract T deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<T> field) throws IOException;

    /**
     * Deserializes a struct field of this type with "nothing" as the default value from a tagged protocol reader,
     * excluding field metadata which is assumed to be already deserialized earlier and whose value is available
     * in the {@link TaggedDeserializationContext#readFieldResult} field of the passed context argument.
     * This method is intended for objects and is not suitable for fields with Java primitive (non-object)
     * types since its return value is a generic {@link SomethingObject} for objects instead of more specific
     * implementation for Java primitive types. To deserialize fields with primitive values use the static
     * helper method defined in each singleton class for a primitive type.
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
    public final String toString() {
        return this.getFullName();
    }

    /**
     * Used when building {@link SchemaDef} objects. Contains tuple (struct def, zero-based position
     * as it was discovered when traversing type tree).
     */
    static final class StructDefOrdinalTuple {
        final StructDef structDef;
        final int ordinal;

        StructDefOrdinalTuple(StructDef structDef, int ordinal) {
            this.structDef = structDef;
            this.ordinal = ordinal;
        }
    }

    /**
     * A helper to create schema that initializes a new type def instance and maintains a hash map
     * of all distinct struct defs discovered so far.
     * @param structDefMap maps struct bond types to their struct defs
     * @return a new type def for the current Bond type
     */
    abstract TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap);

    /**
     * Checks if the argument value is null and throws an exception of it is.
     * This method adds the information about the current type to the exception thrown,
     * which signifies that the value was expected to be of the current type but was null.
     * @param value the value
     * @throws InvalidBondDataException if the value is null
     */
    final void verifyNonNullableValueIsNotSetToNull(
            T value) throws InvalidBondDataException {
        if (value == null) {
            Throw.raiseNonNullableValueSetToNullError(this.getFullName());
        }
    }

    /**
     * Checks if the argument value is null and throws an exception of it is.
     * This method adds the information about the current type and the given field to the exception thrown,
     * which signifies that the value of the field was expected to be of the current type but was null.
     * @param value the value
     * @param field the field
     * @throws InvalidBondDataException if the value is null
     */
    final void verifySerializedNonNullableFieldIsNotSetToNull(
            T value,
            StructBondType.StructField<T> field) throws InvalidBondDataException {
        // delegate to the value verification method and chain the thrown exception (if any);
        // this approach lets us preserve the original exception pertaining to the null value,
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
        return (NullableBondType<TValue>) getCachedType(new NullableBondType<TValue>(valueType));
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
        return (BondedBondType<TStruct>) getCachedType(new BondedBondType<TStruct>(valueType));
    }

    /**
     * Gets a type descriptor for the Bond "bonded" container type. Throws an exception
     * if the value type is not a Bond struct. This method is intended for generated code.
     * This method returns {@link BondType} instead of more specific {@link BondedBondType},
     * due to the constraint on the latter's generic type parameter which must be a struct
     * type (i.e. derive from {@link StructBondType}. Since the type parameter TStruct is not
     * contrained in this method, that generic type constraint can't be satisfied. It shall be
     * noted however, that this method returns only instances of {@link BondedBondType} with
     * a valid Bond struct value underneath. All other cases result in throwing an exception.
     *
     * @param valueType a type descriptor for the underlying value class
     * @param <TStruct> the class of the underlying struct value
     * @return a type descriptor instance
     * @exception IllegalArgumentException if the argument is not a Bond struct type
     */
    protected static <TStruct> BondType<Bonded<TStruct>> bondedOf(
            BondType<TStruct> valueType) {
        ArgumentHelper.ensureNotNull(valueType, "valueType");
        if (!(valueType instanceof StructBondType)) {
            Throw.raiseInvalidBondedValueTypeError(valueType);
        }
        // It's not possible to delegate to the public bondedOf method using a generic type, since that method
        // constrains the TStruct type parameter. Thus, the call is made using non-generic type and the result
        // is upcast to a generic type to match the method's return type.
        @SuppressWarnings("unchecked")
        BondType<Bonded<TStruct>> upcastBondedType = (BondType<Bonded<TStruct>>) bondedOf((StructBondType) valueType);
        return upcastBondedType;
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
        return (VectorBondType<TElement>) getCachedType(new VectorBondType<TElement>(elementType));
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
        return (ListBondType<TElement>) getCachedType(new ListBondType<TElement>(elementType));
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
        return (SetBondType<TElement>) getCachedType(new SetBondType<TElement>(elementType));
    }

    /**
     * Gets a type descriptor for the Bond "set" container type. Throws an exception
     * if the element type is not a primitive Bond type. This method is intended for generated code.
     *
     * @param elementType a type descriptor for the element value class
     * @param <TElement>  the class of the element values
     * @return a type descriptor instance
     * @exception IllegalArgumentException if the argument is not a primitive Bond type
     */
    protected static <TElement> SetBondType<TElement> setOf(
            BondType<TElement> elementType) {
        ArgumentHelper.ensureNotNull(elementType, "elementType");
        if (!(elementType instanceof PrimitiveBondType)) {
            Throw.raiseInvalidSetElementTypeError(elementType);
        }
        return setOf((PrimitiveBondType<TElement>) elementType);
    }

    /**
     * Gets a type descriptor for the Bond "map" container type.
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
        return (MapBondType<TKey, TValue>) getCachedType(new MapBondType<TKey, TValue>(keyType, valueType));
    }

    /**
     * Gets a type descriptor for the Bond "map" container type. Throws an exception
     * if the key type is not a primitive Bond type. This method is intended for generated code.
     *
     * @param keyType   a type descriptor for the map key class
     * @param valueType a type descriptor for the mapped values class
     * @param <TKey>    the class of the map keys
     * @param <TValue>  the class of the mapped values
     * @return a type descriptor instance
     * @exception IllegalArgumentException if the key type argument is not a primitive Bond type
     */
    public static <TKey, TValue> MapBondType<TKey, TValue> mapOf(
            BondType<TKey> keyType, BondType<TValue> valueType) {
        ArgumentHelper.ensureNotNull(keyType, "keyType");
        ArgumentHelper.ensureNotNull(valueType, "valueType");
        if (!(keyType instanceof PrimitiveBondType)) {
            Throw.raiseInvalidMapKeyTypeError(keyType);
        }
        return mapOf((PrimitiveBondType<TKey>) keyType, valueType);
    }

    /**
     * Returns a cached type descriptor that is equal to the argument, or caches the argument otherwise.
     * The method returns a non-null reference to the type descriptor that is equal to the argument
     * (or the same as the argument if called for the first time with that class).
     *
     * @param type the type descriptor
     * @param <T>  the Bond value class
     * @return a cached type descriptor, never null
     */
    protected static <T> BondType<T> getCachedType(BondType<T> type) {
        @SuppressWarnings("unchecked")
        BondType<T> cachedValue = (BondType<T>) typeCache.putIfAbsent(type, type);
        if (cachedValue == null) {
            cachedValue = type;
        }
        return cachedValue;
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

    /**
     * Contains runtime state of untagged deserialization.
     */
    protected static final class UntaggedDeserializationContext {
        public final UntaggedProtocolReader reader;
        public final SchemaDef schema;

        public UntaggedDeserializationContext(UntaggedProtocolReader reader, SchemaDef schema) {
            this.reader = reader;
            this.schema = schema;
        }
    }
}
