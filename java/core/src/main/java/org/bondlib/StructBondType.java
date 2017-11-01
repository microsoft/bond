// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Partially implements the {@link BondType} contract for generated Bond struct data types.
 * Leaves the rest of implementation details to generated subclasses specific to the struct type,
 * which are generated private classes nested within the struct classes.
 * @param <TStruct> the class of the struct value
 */
// This API is consumed by codegen, which static analysis isn't aware of.
@SuppressWarnings({"unused", "WeakerAccess"})
public abstract class StructBondType<TStruct extends BondSerializable> extends BondType<TStruct> {

    // The registry of all loaded struct type builders, for generic and non-generic generated struct types.
    // Entries are added by static initializeBondType methods of the generated struct types which is called
    // by the static class initializer and can also be called manually if the static class initializer hasn't
    // completed yet (which happens with circular class dependencies).
    //
    // This registry's purpose is to provide an alternative solution to obtain a type descriptor for a struct
    // type, that can be used when executing generated code initializing fields of struct type descriptors.
    // An alternative solution is necessary since the user-facing solution of accessing public static fields
    // BOND_TYPE doesn't work when running as part of initialization of a generated class (the static fields
    // may not be set yet). Thus, there are two approaches to get a Bond type descriptor for a user-defined
    // generated struct:
    // 1. [public-facing API used by all user code] Use the public static field BOND_TYPE which is:
    //    (a) The type descriptor, for struct types that do not declare generic type parameters. or
    //    (b) A builder of type descriptor that takes generic type arguments and returns an instance
    //        of the type descriptor, for struct types that declare generic type parameters.
    // 2. [private API used only by generated initialization code] Use the protected method getStructType
    //    that returns the type descriptor given the Class and generic type arguments (empty for non-generic
    //    types. This method relies on the registry of struct type builders, and will initialize the class
    //    (thus executing its registration) if necessary.
    //
    // Note that #2 doesn't distinguish between generic and non-generic types, by abstracting the type builder
    // for all types, with empty generic type argument list for non-generic types. On the contrary, #1 has
    // that distinction since it's intended for public API and needs to be convenient (i.e. it's not elegant
    // to ask client code to "build" an invariant non-generic type).
    private static final ConcurrentHashMap<
            Class<? extends BondSerializable>,
            StructBondTypeBuilder<? extends BondSerializable>> structTypeBuilderRegistry =
            new ConcurrentHashMap<
                    Class<? extends BondSerializable>,
                    StructBondTypeBuilder<? extends BondSerializable>>();

    private static final String BOND_TYPE_INITIALIZATION_METHOD_NAME = "initializeBondType";

    // The global initialization lock, used to initialize type descriptors.
    // Global locking is necessary to avoid deadlocks when multiple threads initialize generated classes
    // that reference each other. The contention for this lock is generally not expected to be a problem because:
    // 1. A lock is acquired only once per type descriptor object when it is actually initialized. This is
    //    achieved by double-checked locking in the ensureInitialized method.
    // 2. Due to type caching, there is at most one lock acqusition for each type (or each specialization of
    //    a generic type), which is constrained by the application. Note that temporary type descriptors
    //    are used only to lookup the cached equivalent and are themselves not initialized.
    // This lock is also used to protect schema def initialization, and the same two points above apply.
    private static final Object initializationLock = new Object();

    // set by the constructor (part of the object identity)
    private final GenericTypeSpecialization genericTypeSpecialization;
    private final int precomputedHashCode;

    // set by the initialization method instead of the constructor since may have cyclic dependencies
    private StructBondType<? super TStruct> baseStructType;
    private StructField<?>[] structFields;

    // cached schema def, thread-safe (atomic) read and write from/to memory
    private volatile SchemaDef schemaDef = null;

    // indicates whether this instance is initialized, thread-safe (atomic) read and write from/to memory
    private volatile boolean isInitialized = false;

    // indicates whether this instance is currently being initialized by the thread holding the global lock;
    // the flag is used to prevent a thread from re-entering initialization method due to cyclic references
    private boolean isCurrentlyInitializing = false;

    /**
     * Used by generated subclasses to instantiate the type descriptor.
     *
     * @param genericTypeSpecialization specialization of a generic struct or null if not a generic struct
     */
    protected StructBondType(GenericTypeSpecialization genericTypeSpecialization) {
        this.genericTypeSpecialization = genericTypeSpecialization;
        precomputedHashCode = this.getClass().hashCode() +
                (genericTypeSpecialization == null ? 0 : genericTypeSpecialization.hashCode());
    }

    /**
     * Used by generated subclasses to initialize the base struct reference and fields of the type descriptor.
     * Field types (including fields of the base struct) may refer back to the declaring struct which causes
     * cyclic dependencies, and therefore this initialization is separated from the constructor.
     *
     * @param baseStructType the type descriptor of the base struct or null if it doesn't exist
     * @param structFields   an array of descriptors of the declared fields of the struct (excluding inherited)
     */
    protected final void initializeBaseAndFields(
            StructBondType<? super TStruct> baseStructType,
            StructField<?>... structFields) {
        this.baseStructType = baseStructType;
        this.structFields = structFields;
    }

    /**
     * Called from generated code to make sure the type descriptor is initialized.
     */
    protected final void ensureInitialized() {
        // double-checked locking to make sure initialization happens only one in a single thread;
        // the contention is restricted since there is at most one initialization per each distinct
        // non-generic struct type or per each distinct specialization of a generic struct type
        if (!this.isInitialized) {
            synchronized (initializationLock) {
                if (!this.isInitialized) {
                    // enter initialization only if not already initializing by the current thread
                    // (which already holds the lock and hence can be the only initializing thread)
                    if (!this.isCurrentlyInitializing) {
                        try {
                            // mark this object as currently being initialized, so that this thread
                            // does not re-enter initialization when there is a cyclic type reference
                            this.isCurrentlyInitializing = true;

                            // initialize (call generated method which initializes struct type fields
                            // and then calls initializeBaseAndFields) and mark this instance as
                            // initialized (using a volatile variable), so that from this point on
                            // every thread will skip initialization and lock acquisition
                            this.initialize();
                            this.isInitialized = true;
                        } finally {
                            // clean up after the current thread so that if the initialize method
                            // threw an exception and the object was not successfully initialized
                            // then some other thread can still try to initialize
                            //
                            // Please note that the initialize methods do not throw any exceptions
                            // under normal circumstances, and if they throw something then it is
                            // almost certainly a fatal error (e.g. OutOfMemory or ThreadDeath).
                            this.isCurrentlyInitializing = false;
                        }
                    }
                }
            }
        }
    }

    /**
     * Implemented by generated subclasses to initialize the type descriptor, speficially initialize the
     * fields and then call {@link #initializeBaseAndFields(StructBondType, StructField[])}.
     */
    protected abstract void initialize();

    /**
     * Used by generated subclasses to serialize declared fields of this struct, excluding inherited fields.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize from
     * @throws IOException if an I/O error occurred
     */
    protected abstract void serializeStructFields(
            SerializationContext context, TStruct value) throws IOException;

    /**
     * Used by generated subclasses to deserialize declared fields of this struct, excluding inherited fields.
     *
     * @param context contains the runtime context of the deserialization
     * @param value   the value to deserialize into
     * @throws IOException if an I/O error occurred
     */
    protected abstract void deserializeStructFields(
            TaggedDeserializationContext context, TStruct value) throws IOException;

    /**
     * Used by generated subclasses to deserialize fields of this struct using the runtime schema, excluding inherited
     * fields.
     *
     * @param context contains the runtime context of the deserialization
     * @param structDef the StructDef of this struct
     * @param value   the value to deserialize into
     * @throws IOException if an I/O error occurred
     */
    protected abstract void deserializeStructFields(
            UntaggedDeserializationContext context, StructDef structDef, TStruct value) throws IOException;

    /**
     * Used by generated subclasses to initialize declared fields of this struct, excluding inherited fields.
     *
     * @param value the value to initialize
     */
    protected abstract void initializeStructFields(TStruct value);

    /**
     * Used by generated subclasses to memberwise-copy declared fields of this struct, excluding inherited fields.
     *
     * @param fromValue the value to copy from
     * @param toValue   the value to copy to
     */
    protected abstract void cloneStructFields(TStruct fromValue, TStruct toValue);

    /**
     * Gets the generic specialization or null if not generic struct.
     *
     * @return the generic specialization or null if not generic struct
     */
    protected final GenericTypeSpecialization getGenericSpecialization() {
        return this.genericTypeSpecialization;
    }

    /**
     * Gets the descriptor of the base struct type.
     *
     * @return the type descriptor of the base struct or null if there is no base struct
     */
    public final StructBondType<? super TStruct> getBaseStructType() {
        return this.baseStructType;
    }

    /**
     * Gets an array containing the descriptors of the struct fields.
     *
     * @return an array containing the descriptors of the struct fields
     */
    public final StructField<?>[] getStructFields() {
        return this.structFields.clone();
    }

    /**
     * Builds a new {@link SchemaDef} instance describing the schema of this struct.
     *
     * @return a new schema definition instance
     */
    public final SchemaDef buildSchemaDef() {
        SchemaDef schemaDef = new SchemaDef();
        HashMap<StructBondType<?>, StructDefOrdinalTuple> typeDefMap =
                new HashMap<StructBondType<?>, StructDefOrdinalTuple>();
        schemaDef.root = this.createSchemaTypeDef(typeDefMap);
        StructDef[] tempArray = new StructDef[typeDefMap.size()];
        for (Map.Entry<StructBondType<?>, StructDefOrdinalTuple> e : typeDefMap.entrySet()) {
            StructDefOrdinalTuple structDefInfo = e.getValue();
            tempArray[structDefInfo.ordinal] = structDefInfo.structDef;
        }
        schemaDef.structs.addAll(Arrays.asList(tempArray));
        return schemaDef;
    }

    /**
     * Returns the {@link SchemaDef} object containing runtime schemas of this struct type
     * and any referenced struct types (ancestors or types of struct fields).
     * The returned instance is permanently cached in this type
     * descriptor and mutating it can have adverse side effects.
     *
     * @return schema definition instance
     */
    public final SchemaDef getSchemaDef() {
        // double-checked locking to make sure the schema is built and cached in a single thread;
        // the contention is restricted since there is at most one schema building per each distinct
        // non-generic struct type or per each distinct specialization of a generic struct type
        if (this.schemaDef == null) {
            synchronized (initializationLock) {
                if (this.schemaDef == null) {
                    this.schemaDef = this.buildSchemaDef();
                }
            }
        }
        return this.schemaDef;
    }

    /**
     * Returns the {@link StructDef} object represening runtime schema of this struct type.
     * The returned instance is permanently cached in this type
     * descriptor and mutating it can have adverse side effects.
     *
     * @return struct definition instance
     */
    public final StructDef getStructDef() {
        return this.getSchemaDef().structs.get(this.schemaDef.root.struct_def);
    }

    @Override
    public final BondDataType getBondDataType() {
        return BondDataType.BT_STRUCT;
    }

    @Override
    public final boolean isNullableType() {
        return false;
    }

    @Override
    public final boolean isGenericType() {
        return this.genericTypeSpecialization != null;
    }

    @Override
    public final BondType<?>[] getGenericTypeArguments() {
        return this.genericTypeSpecialization != null ?
                this.genericTypeSpecialization.genericTypeArguments.clone() : null;
    }

    @Override
    public final Class<TStruct> getPrimitiveValueClass() {
        return null;
    }

    @Override
    protected final TStruct newDefaultValue() {
        return this.newInstance();
    }

    @Override
    protected final TStruct cloneValue(TStruct value) {
        TStruct clonedValue = this.newInstance();
        StructBondType<? super TStruct> currentStructType = this;
        while (currentStructType != null) {
            currentStructType.cloneStructFields(value, clonedValue);
            currentStructType = currentStructType.baseStructType;
        }
        return clonedValue;
    }

    /**
     * Instantiates a new instance of this struct type.
     *
     * @return new struct instance
     */
    public abstract TStruct newInstance();

    /**
     * Returns a value indicating whether this type is a subtype of (or the same as) the argument type.
     *
     * @param other the argument type
     * @return true if this type is the same type or a subtype of the argument type
     */
    public final boolean isSubtypeOf(StructBondType<?> other) {
        ArgumentHelper.ensureNotNull(other, "other");
        StructBondType<?> currentType = this;
        while (currentType != null) {
            if (currentType.equals(other)) {
                return true;
            }
            currentType = currentType.baseStructType;
        }
        return false;
    }

    @Override
    protected final void serializeValue(SerializationContext context, TStruct value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        context.writer.writeStructBegin(this.getStructDef().metadata);
        if (this.baseStructType != null) {
            this.baseStructType.serializeValueAsBase(context, value);
        }
        this.serializeStructFields(context, value);
        context.writer.writeStructEnd();
    }

    private void serializeValueAsBase(
            SerializationContext context, TStruct value) throws IOException {
        if (this.baseStructType != null) {
            this.baseStructType.serializeValueAsBase(context, value);
        }
        context.writer.writeBaseBegin(this.getStructDef().metadata);
        this.serializeStructFields(context, value);
        context.writer.writeBaseEnd();
    }

    @Override
    protected final TStruct deserializeValue(TaggedDeserializationContext context) throws IOException {
        TStruct value = this.newDefaultValue();
        context.reader.readStructBegin();
        if (this.baseStructType != null) {
            this.baseStructType.deserializeValueAsBase(context, value);
        }
        this.deserializeStructFields(context, value);
        context.reader.readStructEnd();
        return value;
    }

    private void deserializeValueAsBase(TaggedDeserializationContext context, TStruct value) throws IOException {
        if (this.baseStructType != null) {
            this.baseStructType.deserializeValueAsBase(context, value);
        }
        context.reader.readBaseBegin();
        this.deserializeStructFields(context, value);
        context.reader.readBaseEnd();
    }

    @Override
    protected final TStruct deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        TStruct value = this.newDefaultValue();
        this.deserializeValue(context, typeDef, value);
        return value;
    }

    private void deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef,
        TStruct value) throws IOException {
        final StructDef structDef = context.schema.structs.get(typeDef.struct_def);
        if (this.baseStructType != null) {
            final TypeDef baseDef = structDef.base_def;
            this.baseStructType.deserializeValue(context, baseDef, value);
        }
        this.deserializeStructFields(context, structDef, value);
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            TStruct value,
            StructField<TStruct> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        // struct fields are never omitted
        context.writer.writeFieldBegin(BondDataType.BT_STRUCT, field.getId(), field.getFieldDef().metadata);
        try {
            this.serializeValue(context, value);
        } catch (InvalidBondDataException e) {
            // throws
            Throw.raiseStructFieldSerializationError(false, field, e, null);
        }
        context.writer.writeFieldEnd();
    }

    @Override
    protected final TStruct deserializeField(
            TaggedDeserializationContext context,
            StructField<TStruct> field) throws IOException {
        // a struct value may be deserialized only from BT_STRUCT
        if (context.readFieldResult.type.value != BondDataType.BT_STRUCT.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        TStruct value = null;
        try {
            value = this.deserializeValue(context);
        } catch (InvalidBondDataException e) {
            // throws
            Throw.raiseStructFieldSerializationError(true, field, e, null);
        }
        return value;
    }

    @Override
    public final int hashCode() {
        return this.precomputedHashCode;
    }

    @Override
    public final boolean equals(Object obj) {
        if (obj instanceof StructBondType<?>) {
            StructBondType<?> that = (StructBondType<?>) obj;
            return this.precomputedHashCode == that.precomputedHashCode &&
                    this.getClass().equals(that.getClass()) &&
                    (this.genericTypeSpecialization == null ?
                            that.genericTypeSpecialization == null :
                            this.genericTypeSpecialization.equals(that.genericTypeSpecialization));

        } else {
            return false;
        }
    }

    /**
     * Serializes an object into the given protocol writer.
     *
     * @param obj    the object to serialize
     * @param writer the protocol writer to write into
     * @throws IOException if an I/O error occurred
     */
    void serialize(TStruct obj, ProtocolWriter writer) throws IOException {
        // first pass
        if (writer instanceof TwoPassProtocolWriter) {
            ProtocolWriter firstPassWriter = ((TwoPassProtocolWriter) writer).getFirstPassWriter();
            if (firstPassWriter != null) {
                SerializationContext firstPassContext = new SerializationContext(firstPassWriter);
                this.serializeValue(firstPassContext, obj);
            }
        }

        // second pass
        SerializationContext context = new SerializationContext(writer);
        this.serializeValue(context, obj);
    }

    /**
     * Deserializes an object from the given tagged protocol reader.
     *
     * @param reader the protocol reader to read from
     * @return deserialized object
     * @throws IOException if an I/O error occurred
     */
    TStruct deserialize(TaggedProtocolReader reader) throws IOException {
        TaggedDeserializationContext context = new TaggedDeserializationContext(reader);
        return this.deserializeValue(context);
    }

    /**
     * Deserializes an object from the given untagged protocol reader.
     *
     * @param reader the protocol reader to read from
     * @return deserialized object
     * @throws IOException if an I/O error occurred
     */
    TStruct deserialize(UntaggedProtocolReader reader) throws IOException {
        return this.deserialize(reader, this.buildSchemaDef());
    }

    /**
     * Deserializes an object from the given untagged protocol reader using the supplied runtime schema.
     *
     * @param reader the protocol reader to read from
     * @param schema the runtime scheam
     * @return deserialized object
     * @throws IOException if an I/O error occurred
     */
    TStruct deserialize(UntaggedProtocolReader reader, SchemaDef schema) throws IOException {
        UntaggedDeserializationContext context = new UntaggedDeserializationContext(reader, schema);
        return this.deserializeValue(context, schema.root);
    }

    @Override
    final TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        // first need to get the struct def info since need to reference it by the ordinal
        StructDefOrdinalTuple structDefInfo = structDefMap.get(this);
        if (structDefInfo == null) {
            // struct def wasn't created yet, create one and associate with the current struct in the map;
            int nextOrdinal = structDefMap.size();
            StructDef structDef = new StructDef();
            structDefInfo = new StructDefOrdinalTuple(structDef, nextOrdinal);

            // the struct def instance that is associated in the map with the current struct type is not yet
            // initialized, but any descendants that use this struct will reference the same struct def instance
            structDefMap.put(this, structDefInfo);
            this.initializeSchemaStructDef(structDef, structDefMap);
        }

        TypeDef typeDef = new TypeDef();
        typeDef.id = this.getBondDataType();
        typeDef.struct_def = (short) structDefInfo.ordinal;
        return typeDef;
    }

    /**
     * Codegen helper method that tries to read a field from payload and indicates whether there is a field to read.
     *
     * @param context contains the runtime context of the deserialization
     * @return true if there are more fields to read
     * @throws IOException if an I/O error occurred
     */
    protected static boolean readField(TaggedDeserializationContext context) throws IOException {
        context.reader.readFieldBegin(context.readFieldResult);
        int statusValue = context.readFieldResult.type.value;
        return statusValue != BondDataType.BT_STOP.value && statusValue != BondDataType.BT_STOP_BASE.value;
    }

    private void initializeSchemaStructDef(
            StructDef structDef, HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        structDef.metadata.name = this.getName();
        structDef.metadata.qualified_name = this.getFullName();
        if (this.baseStructType != null) {
            structDef.base_def = this.baseStructType.createSchemaTypeDef(structDefMap);
        }
        for (StructField<?> field : this.structFields) {
            FieldDef fieldDef = new FieldDef();
            fieldDef.metadata.name = field.name;
            fieldDef.metadata.modifier = field.modifier;
            initializeSchemaVariantWithDefaultValue(fieldDef.metadata.default_value, field);
            fieldDef.id = field.id;
            fieldDef.type = field.fieldType.createSchemaTypeDef(structDefMap);
            structDef.fields.add(fieldDef);
            field.fieldDef = fieldDef;
        }
    }

    private void initializeSchemaVariantWithDefaultValue(Variant variant, StructField field) {
        variant.nothing = field.isDefaultNothing();
        if (!variant.nothing) {
            switch (field.fieldType.getBondDataType().value) {
                case BondDataType.Values.BT_UINT8:
                case BondDataType.Values.BT_UINT16:
                case BondDataType.Values.BT_UINT32:
                case BondDataType.Values.BT_UINT64:
                    variant.uint_value = ((Number) field.getDefaultValue()).longValue();
                    break;

                case BondDataType.Values.BT_INT8:
                case BondDataType.Values.BT_INT16:
                case BondDataType.Values.BT_INT64:
                    variant.int_value = ((Number) field.getDefaultValue()).longValue();
                    break;

                case BondDataType.Values.BT_INT32:
                    // could be an enum
                    if (field.fieldType instanceof EnumBondType) {
                        variant.int_value = ((BondEnum) field.getDefaultValue()).getValue();
                    } else {
                        variant.int_value = (Integer) field.getDefaultValue();
                    }
                    break;

                case BondDataType.Values.BT_BOOL:
                    // bool is piggy-backing on the int value
                    variant.int_value = (Boolean) field.getDefaultValue() ? 1 : 0;
                    break;

                case BondDataType.Values.BT_FLOAT:
                case BondDataType.Values.BT_DOUBLE:
                    variant.double_value = ((Number) field.getDefaultValue()).doubleValue();
                    break;

                case BondDataType.Values.BT_STRING:
                    variant.string_value = (String) field.getDefaultValue();
                    break;

                case BondDataType.Values.BT_WSTRING:
                    variant.wstring_value = (String) field.getDefaultValue();
                    break;

                default:
                    // the default is null for structs and containers
                    break;
            }
        }
    }

    /**
     * Registers a struct type builder from generated code.
     *
     * @param clazz             the struct class
     * @param structTypeBuilder type builder for the given struct type
     * @param <TStruct>         the Bond struct value class
     */
    protected static <TStruct extends BondSerializable> void registerStructType(
            Class<TStruct> clazz, StructBondTypeBuilder<TStruct> structTypeBuilder) {
        structTypeBuilderRegistry.putIfAbsent(clazz, structTypeBuilder);
    }

    /**
     * Gets a type descriptor for a struct represented by a particular generated class.
     * This method is used when initializing struct type descriptors, so that a particular
     * type descriptor may access a type descriptor of another type. This method is the only
     * accessor to a struct type descriptor that is used in generated code when initializing
     * struct type descriptors (generic type arguments, the base, or fields) and since the
     * type descriptor is returns is always initialized, there's the invariant condition that
     * all cached struct type descriptors are initialized.
     *
     * @param clazz                the struct class
     * @param genericTypeArguments the generic type arguments in the declaration order
     * @return a type descriptor instance
     */
    protected static StructBondType<? extends BondSerializable> getStructType(
            Class<? extends BondSerializable> clazz,
            BondType<?>... genericTypeArguments) {
        StructBondTypeBuilder<?> structTypeBuilder = structTypeBuilderRegistry.get(clazz);
        if (structTypeBuilder == null) {
            // the type builder is not found in the registry, which could be due to the following two reasons:
            // 1. The type builder class hasn't been registered yet, which means that the struct class
            //    (whose generated static initializer does this registration) hasn't been initialized yet, or
            // 2. The struct class was not generated by the same code generator, which should never happen
            //    since this method is protected and intended to be called only from generated code
            try {
                // Class initialization may not be enough since the class may already be in the middle of
                // initialization, where is needed to initialize a referenced class that had a circular
                // reference back to it. Therefore, call the static initialization method
                Method typeInitMethod = clazz.getMethod(BOND_TYPE_INITIALIZATION_METHOD_NAME);
                typeInitMethod.invoke(null);
            } catch (Exception e) {
                // if there is an error, then the class implemenation is invalid
                throw new RuntimeException(
                        "Unexpected program state: invalid struct implementation: " + clazz.getName(),
                        e);
            }

            // at this point the class initialization should register the struct type builder;
            // if it's still not registered then the struct class initialization is not doing what
            // it should (although the initialization method exists and was successfully invoked),
            // so the conclusion is that it's not generated per our expectations
            structTypeBuilder = structTypeBuilderRegistry.get(clazz);
            if (structTypeBuilder == null) {
                throw new RuntimeException(
                        "Unexpected program state: invalid struct implementation: " + clazz.getName());
            }
        }

        // make sure the generic type argument count matches the expected count;
        // since this should be called only be generated code, this must be always the case
        if (structTypeBuilder.getGenericTypeParameterCount() != genericTypeArguments.length) {
            throw new RuntimeException(
                    "Unexpected program state: generic argument count mismatch: " + clazz.getName() +
                            ", expected: " + structTypeBuilder.getGenericTypeParameterCount() +
                            ", actual: " + genericTypeArguments.length);
        }

        // build, cache, and initialize the type descriptor
        return structTypeBuilder.getInitializedFromCache(genericTypeArguments);
    }

    /**
     * Responsible for building and caching Bond struct types with generic type parameters. Please note
     * that a {@link StructBondType} instance for a generic type can exist only when all generic type
     * parameters are bound. This class is responsible for this binding: it takes the generic type
     * arguments and instantiates a new {@link StructBondType} instance representing a specialization
     * of the generic type. As a special case, it can instantiate new type descriptors for non-generic
     * types (for which the list of generic type arguments is null).
     *
     * @param <TStruct> the class of the struct value
     */
    protected static abstract class StructBondTypeBuilder<TStruct extends BondSerializable> {

        /**
         * Gets the number of generic type parameters or (as a special case) 0 for non-generic types.
         *
         * @return the number of generic type parameters
         */
        public abstract int getGenericTypeParameterCount();

        /**
         * Creates a new instance of type descriptor that is neither initialized nor cached. This instance
         * is used to locate the cached instance (or will be cached itself if no cached instance exists yet).
         *
         * @param genericTypeArguments generic type arguments
         * @return new uninitialized uncached type descriptor instance
         */
        protected abstract StructBondType<TStruct> buildNewInstance(BondType<?>[] genericTypeArguments);

        /**
         * Returns an initialized instance of the struct type descriptor from the cache,
         * building/caching/initializing it if necessary.
         *
         * @param genericTypeArguments generic type arguments
         * @return new initialized type descriptor instance (cached)
         */
        public final StructBondType<TStruct> getInitializedFromCache(BondType<?>... genericTypeArguments) {
            StructBondType<TStruct> cacheKey = this.buildNewInstance(genericTypeArguments);
            StructBondType<TStruct> cachedType = (StructBondType<TStruct>) getCachedType(cacheKey);
            cachedType.ensureInitialized();
            return cachedType;
        }
    }

    /**
     * A descriptor of a single field in a struct declaration that encapsulates the details
     * of that field behavior such as initialization, serialization and deserialization.
     * This class is the root of the class hierarchy for various Bond types (primitives and objects)
     * as well as for whether the field defaults to "nothing" (i.e. needs a "Something" wrapper).
     * The purpose of this class hierarchy is that field initialization/serialization/deserialization
     * code can just call initialize/serialize/deserialize method without any regard to the field type,
     * which is setup in the struct type descriptor's {@see StructBondType#initialize} method.
     *
     * @param <TField> the class of the field value, using corresponding wrappers for primitive types
     */
    protected static abstract class StructField<TField> {

        // accessed by subclasses
        final StructBondType<?> structType;
        final BondType<TField> fieldType;
        final short id;
        final String name;
        final Modifier modifier;

        private FieldDef fieldDef;

        // restrict subclasses to nested classes only
        private StructField(
                StructBondType<?> structType,
                BondType<TField> fieldType,
                int id,
                String name,
                Modifier modifier) {
            this.structType = structType;
            this.fieldType = fieldType;
            this.id = (short) id;
            this.name = name;
            this.modifier = modifier;
        }

        /**
         * Gets the declaring struct type descriptor.
         *
         * @return the declaring struct type descriptor
         */
        public final StructBondType<?> getStructType() {
            return this.structType;
        }

        /**
         * Gets the field type descriptor.
         *
         * @return the field type descriptor
         */
        public final BondType<TField> getFieldType() {
            return this.fieldType;
        }

        /**
         * Gets the field definition schema.
         *
         * @return field definition schema.
         */
        public final FieldDef getFieldDef() {
            return this.fieldDef;
        }

        /**
         * Gets the field ID.
         *
         * @return the field ID
         */
        public final short getId() {
            return this.id;
        }

        /**
         * Gets the field name.
         *
         * @return the field name
         */
        public final String getName() {
            return this.name;
        }

        /**
         * Gets the field modifier.
         *
         * @return the field modifier
         */
        public final Modifier getModifier() {
            return this.modifier;
        }

        /**
         * Gets the default value of the field if the field is of primitive data type, or null otherwise.
         *
         * @return the default value of the field or null
         */
        public abstract TField getDefaultValue();

        /**
         * Gets a value indicating whether the field is defaulting to "nothing", which means that its
         * value is wrapped by the {@link Something} wrapper.
         *
         * @return a value indicating whether the field is defaulting to "nothing"
         */
        public abstract boolean isDefaultNothing();

        /**
         * Codegen helper to verify deserialized field.
         *
         * @param isFieldSet a boolean tracking whether the field was set during deserialization
         * @throws InvalidBondDataException if the field is required and was not set
         */
        public final void verifyDeserialized(boolean isFieldSet) throws InvalidBondDataException {
            if (!isFieldSet && this.modifier.value == Modifier.Required.value) {
                // throws
                Throw.raiseRequiredStructFieldIsMissingDeserializationError(this);
            }
        }

        final void verifyFieldWasNotYetDeserialized(
                boolean wasAlreadyDeserialized) throws InvalidBondDataException {
            if (wasAlreadyDeserialized) {
                Throw.raiseStructFieldIsPresentMoreThanOnceDeserializationError(this);
            }
        }

        final boolean isOptional() {
            return this.modifier.value == Modifier.Optional.value;
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of general object data types
     * (i.e. any field that is not of a Java primitive type, string, or an enum) that are
     * not defaulting to "nothing". Since these field types cannot have explicit default
     * values, there is no constructor that takes a default value.
     */
    protected static final class ObjectStructField<TField> extends StructField<TField> {

        public ObjectStructField(
                StructBondType<?> structType,
                BondType<TField> fieldType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, fieldType, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final TField getDefaultValue() {
            return this.initialize();
        }

        public final TField initialize() {
            return this.fieldType.newDefaultValue();
        }

        public final TField clone(TField value) {
            return this.fieldType.cloneValue(value);
        }

        public final void serialize(
                SerializationContext context, TField value) throws IOException {
            this.fieldType.serializeField(context, value, this);
        }

        public final TField deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeField(context, this);
        }

        public final TField deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return this.fieldType.deserializeValue(context, typeDef);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of general object data types
     * (i.e. any field that is not of a Java primitive type, string, or an enum) that are
     * defaulting to "nothing". The default value for such fields is null (meaning "nothing").
     */
    protected static final class SomethingObjectStructField<TField> extends StructField<TField> {

        public SomethingObjectStructField(
                StructBondType<?> structType,
                BondType<TField> fieldType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, fieldType, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final TField getDefaultValue() {
            return null;
        }

        public final SomethingObject<TField> initialize() {
            return null;
        }

        public final SomethingObject<TField> clone(SomethingObject<TField> value) {
            return value == null ? null : Something.wrap(this.fieldType.cloneValue(value.value));
        }

        public final void serialize(
                SerializationContext context, SomethingObject<TField> value) throws IOException {
            this.fieldType.serializeSomethingField(context, value, this);
        }

        public final SomethingObject<TField> deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeSomethingField(context, this);
        }

        public final SomethingObject<TField> deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return Something.wrap(this.fieldType.deserializeValue(context, typeDef));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint8 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class UInt8StructField extends StructField<Byte> {

        private final byte defaultValue;

        public UInt8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                byte defaultValue) {
            super(structType, BondTypes.UINT8, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public UInt8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, UInt8BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Byte getDefaultValue() {
            // box
            return this.initialize();
        }

        public final byte initialize() {
            return this.defaultValue;
        }

        public final byte clone(byte value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, byte value) throws IOException {
            UInt8BondType.serializePrimitiveField(context, value, this);
        }

        public final byte deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt8BondType.deserializePrimitiveField(context, this);
        }

        public final byte deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return UInt8BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint8 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingUInt8StructField extends StructField<Byte> {

        public SomethingUInt8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.UINT8, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Byte getDefaultValue() {
            return null;
        }

        public final SomethingByte initialize() {
            return null;
        }

        public final SomethingByte clone(SomethingByte value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingByte value) throws IOException {
            UInt8BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingByte deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt8BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingByte deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(UInt8BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint16 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class UInt16StructField extends StructField<Short> {

        private final short defaultValue;

        public UInt16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                short defaultValue) {
            super(structType, BondTypes.UINT16, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public UInt16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, UInt16BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Short getDefaultValue() {
            // box
            return this.initialize();
        }

        public final short initialize() {
            return this.defaultValue;
        }

        public final short clone(short value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, short value) throws IOException {
            UInt16BondType.serializePrimitiveField(context, value, this);
        }

        public final short deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt16BondType.deserializePrimitiveField(context, this);
        }

        public final short deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return UInt16BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint16 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingUInt16StructField extends StructField<Short> {

        public SomethingUInt16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.UINT16, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Short getDefaultValue() {
            return null;
        }

        public final SomethingShort initialize() {
            return null;
        }

        public final SomethingShort clone(SomethingShort value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingShort value) throws IOException {
            UInt16BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingShort deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt16BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingShort deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(UInt16BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint32 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class UInt32StructField extends StructField<Integer> {

        private final int defaultValue;

        public UInt32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                int defaultValue) {
            super(structType, BondTypes.UINT32, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public UInt32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, UInt32BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Integer getDefaultValue() {
            // box
            return this.initialize();
        }

        public final int initialize() {
            return this.defaultValue;
        }

        public final int clone(int value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, int value) throws IOException {
            UInt32BondType.serializePrimitiveField(context, value, this);
        }

        public final int deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt32BondType.deserializePrimitiveField(context, this);
        }

        public final int deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return UInt32BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint32 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingUInt32StructField extends StructField<Integer> {

        public SomethingUInt32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.UINT32, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Integer getDefaultValue() {
            return null;
        }

        public final SomethingInteger initialize() {
            return null;
        }

        public final SomethingInteger clone(SomethingInteger value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingInteger value) throws IOException {
            UInt32BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingInteger deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt32BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingInteger deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(UInt32BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint64 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class UInt64StructField extends StructField<Long> {

        private final long defaultValue;

        public UInt64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                long defaultValue) {
            super(structType, BondTypes.UINT64, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public UInt64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, UInt64BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Long getDefaultValue() {
            // box
            return this.initialize();
        }

        public final long initialize() {
            return this.defaultValue;
        }

        public final long clone(long value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, long value) throws IOException {
            UInt64BondType.serializePrimitiveField(context, value, this);
        }

        public final long deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt64BondType.deserializePrimitiveField(context, this);
        }

        public final long deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return UInt64BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the uint64 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingUInt64StructField extends StructField<Long> {

        public SomethingUInt64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.UINT64, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Long getDefaultValue() {
            return null;
        }

        public final SomethingLong initialize() {
            return null;
        }

        public final SomethingLong clone(SomethingLong value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingLong value) throws IOException {
            UInt64BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingLong deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt64BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingLong deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(UInt64BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int8 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class Int8StructField extends StructField<Byte> {

        private final byte defaultValue;

        public Int8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                byte defaultValue) {
            super(structType, BondTypes.INT8, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public Int8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, Int8BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Byte getDefaultValue() {
            // box
            return this.initialize();
        }

        public final byte initialize() {
            return this.defaultValue;
        }

        public final byte clone(byte value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, byte value) throws IOException {
            Int8BondType.serializePrimitiveField(context, value, this);
        }

        public final byte deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int8BondType.deserializePrimitiveField(context, this);
        }

        public final byte deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Int8BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int8 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingInt8StructField extends StructField<Byte> {

        public SomethingInt8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.INT8, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Byte getDefaultValue() {
            return null;
        }

        public final SomethingByte initialize() {
            return null;
        }

        public final SomethingByte clone(SomethingByte value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingByte value) throws IOException {
            Int8BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingByte deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int8BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingByte deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(Int8BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int16 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class Int16StructField extends StructField<Short> {

        private final short defaultValue;

        public Int16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                short defaultValue) {
            super(structType, BondTypes.INT16, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public Int16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, Int16BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Short getDefaultValue() {
            // box
            return this.initialize();
        }

        public final short initialize() {
            return this.defaultValue;
        }

        public final short clone(short value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, short value) throws IOException {
            Int16BondType.serializePrimitiveField(context, value, this);
        }

        public final short deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int16BondType.deserializePrimitiveField(context, this);
        }

        public final short deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Int16BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int16 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingInt16StructField extends StructField<Short> {

        public SomethingInt16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.INT16, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Short getDefaultValue() {
            return null;
        }

        public final SomethingShort initialize() {
            return null;
        }

        public final SomethingShort clone(SomethingShort value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingShort value) throws IOException {
            Int16BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingShort deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int16BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingShort deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(Int16BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int32 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class Int32StructField extends StructField<Integer> {

        private final int defaultValue;

        public Int32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                int defaultValue) {
            super(structType, BondTypes.INT32, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public Int32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, Int32BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Integer getDefaultValue() {
            // box
            return this.initialize();
        }

        public final int initialize() {
            return this.defaultValue;
        }

        public final int clone(int value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, int value) throws IOException {
            Int32BondType.serializePrimitiveField(context, value, this);
        }

        public final int deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int32BondType.deserializePrimitiveField(context, this);
        }

        public final int deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Int32BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int32 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingInt32StructField extends StructField<Integer> {

        public SomethingInt32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.INT32, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Integer getDefaultValue() {
            return null;
        }

        public final SomethingInteger initialize() {
            return null;
        }

        public final SomethingInteger clone(SomethingInteger value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingInteger value) throws IOException {
            Int32BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingInteger deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int32BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingInteger deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(Int32BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int64 primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class Int64StructField extends StructField<Long> {

        private final long defaultValue;

        public Int64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                long defaultValue) {
            super(structType, BondTypes.INT64, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public Int64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, Int64BondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Long getDefaultValue() {
            // box
            return this.initialize();
        }

        public final long initialize() {
            return this.defaultValue;
        }

        public final long clone(long value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, long value) throws IOException {
            Int64BondType.serializePrimitiveField(context, value, this);
        }

        public final long deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int64BondType.deserializePrimitiveField(context, this);
        }

        public final long deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Int64BondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the int64 primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingInt64StructField extends StructField<Long> {

        public SomethingInt64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.INT64, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Long getDefaultValue() {
            return null;
        }

        public final SomethingLong initialize() {
            return null;
        }

        public final SomethingLong clone(SomethingLong value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingLong value) throws IOException {
            Int64BondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingLong deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int64BondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingLong deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(Int64BondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the bool primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class BoolStructField extends StructField<Boolean> {

        private final boolean defaultValue;

        public BoolStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean defaultValue) {
            super(structType, BondTypes.BOOL, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public BoolStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, BoolBondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Boolean getDefaultValue() {
            // box
            return this.initialize();
        }

        public final boolean initialize() {
            return this.defaultValue;
        }

        public final boolean clone(boolean value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, boolean value) throws IOException {
            BoolBondType.serializePrimitiveField(context, value, this);
        }

        public final boolean deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return BoolBondType.deserializePrimitiveField(context, this);
        }

        public final boolean deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return BoolBondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the bool primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingBoolStructField extends StructField<Boolean> {

        public SomethingBoolStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.BOOL, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Boolean getDefaultValue() {
            return null;
        }

        public final SomethingBoolean initialize() {
            return null;
        }

        public final SomethingBoolean clone(SomethingBoolean value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingBoolean value) throws IOException {
            BoolBondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingBoolean deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return BoolBondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingBoolean deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(BoolBondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the float primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class FloatStructField extends StructField<Float> {

        private final float defaultValue;

        public FloatStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                float defaultValue) {
            super(structType, BondTypes.FLOAT, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public FloatStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, FloatBondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Float getDefaultValue() {
            // box
            return this.initialize();
        }

        public final float initialize() {
            return this.defaultValue;
        }

        public final float clone(float value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, float value) throws IOException {
            FloatBondType.serializePrimitiveField(context, value, this);
        }

        public final float deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return FloatBondType.deserializePrimitiveField(context, this);
        }

        public final float deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return FloatBondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the float primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingFloatStructField extends StructField<Float> {

        public SomethingFloatStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.FLOAT, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Float getDefaultValue() {
            return null;
        }

        public final SomethingFloat initialize() {
            return null;
        }

        public final SomethingFloat clone(SomethingFloat value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingFloat value) throws IOException {
            FloatBondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingFloat deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return FloatBondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingFloat deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(FloatBondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the double primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class DoubleStructField extends StructField<Double> {

        private final double defaultValue;

        public DoubleStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                double defaultValue) {
            super(structType, BondTypes.DOUBLE, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public DoubleStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, DoubleBondType.DEFAULT_VALUE_AS_PRIMITIVE);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final Double getDefaultValue() {
            // box
            return this.initialize();
        }

        public final double initialize() {
            return this.defaultValue;
        }

        public final double clone(double value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, double value) throws IOException {
            DoubleBondType.serializePrimitiveField(context, value, this);
        }

        public final double deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return DoubleBondType.deserializePrimitiveField(context, this);
        }

        public final double deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return DoubleBondType.deserializePrimitiveValue(context);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the double primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingDoubleStructField extends StructField<Double> {

        public SomethingDoubleStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.DOUBLE, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final Double getDefaultValue() {
            return null;
        }

        public final SomethingDouble initialize() {
            return null;
        }

        public final SomethingDouble clone(SomethingDouble value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingDouble value) throws IOException {
            DoubleBondType.serializePrimitiveSomethingField(context, value, this);
        }

        public final SomethingDouble deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return DoubleBondType.deserializePrimitiveSomethingField(context, this);
        }

        public final SomethingDouble deserialize(UntaggedDeserializationContext context, TypeDef typeDef) throws IOException {
            return Something.wrap(DoubleBondType.deserializePrimitiveValue(context));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the string primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class StringStructField extends StructField<String> {

        private final String defaultValue;

        public StringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                String defaultValue) {
            super(structType, BondTypes.STRING, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        // used by codegen
        public StringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, StringBondType.DEFAULT_VALUE_AS_OBJECT);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final String getDefaultValue() {
            return this.initialize();
        }

        public final String initialize() {
            return this.defaultValue;
        }

        public final String clone(String value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, String value) throws IOException {
            this.fieldType.serializeField(context, value, this);
        }

        public final String deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeField(context, this);
        }

        public final String deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return this.fieldType.deserializeValue(context, typeDef);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the string primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingStringStructField extends StructField<String> {

        public SomethingStringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.STRING, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final String getDefaultValue() {
            return null;
        }

        public final SomethingObject<String> initialize() {
            return null;
        }

        public final SomethingObject<String> clone(SomethingObject<String> value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingObject<String> value) throws IOException {
            this.fieldType.serializeSomethingField(context, value, this);
        }

        public final SomethingObject<String> deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeSomethingField(context, this);
        }

        public final SomethingObject<String> deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return Something.wrap(this.fieldType.deserializeValue(context, typeDef));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the string primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class WStringStructField extends StructField<String> {

        private final String defaultValue;

        public WStringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                String defaultValue) {
            super(structType, BondTypes.WSTRING, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        // used by codegen
        public WStringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, id, name, modifier, WStringBondType.DEFAULT_VALUE_AS_OBJECT);
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final String getDefaultValue() {
            return this.initialize();
        }

        public final String initialize() {
            return this.defaultValue;
        }

        public final String clone(String value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, String value) throws IOException {
            this.fieldType.serializeField(context, value, this);
        }

        public final String deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeField(context, this);
        }

        public final String deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return this.fieldType.deserializeValue(context, typeDef);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the string primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingWStringStructField extends StructField<String> {

        // used by codegen
        public SomethingWStringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, BondTypes.WSTRING, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final String getDefaultValue() {
            return null;
        }

        public final SomethingObject<String> initialize() {
            return null;
        }

        public final SomethingObject<String> clone(SomethingObject<String> value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingObject<String> value) throws IOException {
            this.fieldType.serializeSomethingField(context, value, this);
        }

        public final SomethingObject<String> deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeSomethingField(context, this);
        }

        public final SomethingObject<String> deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return Something.wrap(this.fieldType.deserializeValue(context, typeDef));
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the string primitive data type
     * that are not defaulting to "nothing".
     */
    protected static final class EnumStructField<TEnum extends BondEnum<TEnum>> extends StructField<TEnum> {

        private final TEnum defaultValue;

        public EnumStructField(
                StructBondType<?> structType,
                EnumBondType<TEnum> fieldType,
                int id,
                String name,
                Modifier modifier,
                TEnum defaultValue) {
            super(structType, fieldType, id, name, modifier);
            this.defaultValue = defaultValue;
        }

        public EnumStructField(
                StructBondType<?> structType,
                EnumBondType<TEnum> fieldType,
                int id,
                String name,
                Modifier modifier) {
            this(structType, fieldType, id, name, modifier, fieldType.newDefaultValue());
        }

        @Override
        public final boolean isDefaultNothing() {
            return false;
        }

        @Override
        public final TEnum getDefaultValue() {
            return this.initialize();
        }

        public final TEnum initialize() {
            return this.defaultValue;
        }

        public final TEnum clone(TEnum value) {
            return value;
        }

        public final void serialize(
                SerializationContext context, TEnum value) throws IOException {
            this.fieldType.serializeField(context, value, this);
        }

        public final TEnum deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeField(context, this);
        }

        public final TEnum deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return this.fieldType.deserializeValue(context, typeDef);
        }
    }

    /**
     * Implements the {@link StructField} contract for fields of the string primitive data type
     * that are defaulting to "nothing".
     */
    protected static final class SomethingEnumStructField<TEnum extends BondEnum<TEnum>> extends StructField<TEnum> {

        // used by codegen
        public SomethingEnumStructField(
                StructBondType<?> structType,
                EnumBondType<TEnum> fieldType,
                int id,
                String name,
                Modifier modifier) {
            super(structType, fieldType, id, name, modifier);
        }

        @Override
        public final boolean isDefaultNothing() {
            return true;
        }

        @Override
        public final TEnum getDefaultValue() {
            return null;
        }

        public final SomethingObject<TEnum> initialize() {
            return null;
        }

        public final SomethingObject<TEnum> clone(SomethingObject<TEnum> value) {
            return value == null ? null : Something.wrap(value.value);
        }

        public final void serialize(
                SerializationContext context, SomethingObject<TEnum> value) throws IOException {
            this.fieldType.serializeSomethingField(context, value, this);
        }

        public final SomethingObject<TEnum> deserialize(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeSomethingField(context, this);
        }

        public final SomethingObject<TEnum> deserialize(
            UntaggedDeserializationContext context,
            TypeDef typeDef) throws IOException {
            return Something.wrap(this.fieldType.deserializeValue(context, typeDef));
        }
    }
}
