// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond;

import com.microsoft.bond.protocol.*;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

/**
 * Partially implements the {@link BondType} contract for struct data types.
 * Leaves some implementation details to subclasses specific to the a particular
 * struct type, which are generated together with the struct type.
 * @param <TStruct> the class of the struct value
 */
public abstract class StructBondType<TStruct extends BondSerializable>
        extends BondType<TStruct> implements StructMetadata {

    // set by the constructor
    private final StructBondType<? super TStruct> baseStructType;
    private final GenericTypeSpecialization genericTypeSpecialization;

    // set by the initialization method instead of the constructor since may have cyclic dependencies
    private StructField<?>[] structFields;

    private boolean isInitialized = false;

    /**
     * Used by generated subclasses to instantiate the type descriptor.
     *
     * @param baseStructType            the type descriptor of the base struct or null if it doesn't exist
     * @param genericTypeSpecialization specialization of a generic struct or null if not a generic struct
     */
    protected StructBondType(
            Class<? extends StructBondType> thisClass,
            StructBondType<? super TStruct> baseStructType,
            GenericTypeSpecialization genericTypeSpecialization) {
        super(thisClass.hashCode() + ((genericTypeSpecialization == null) ? 0 : genericTypeSpecialization.hashCode()));
        this.baseStructType = baseStructType;
        this.genericTypeSpecialization = genericTypeSpecialization;
    }

    /**
     * Used by generated subclasses to initialize fields of the type descriptor.
     * Field types may refer back to the declaring struct which causes cyclic dependencies,
     * and therefore this initialization is separated from the constructor.
     *
     * @param structFields an array of descriptors of the declared fields of the struct (excluding inherited)
     */
    protected final void initializeFields(
            StructField<?>... structFields) {
        this.structFields = structFields;
    }

    /**
     * Called from generated code to make sure the type descriptor is initialized
     * as well as all its referenced struct types.
     */
    protected synchronized final void ensureInitialized() {
        if (!this.isInitialized) {
            this.initialize();

            // mark as initialized before recursive calls
            this.isInitialized = true;

            // recurse to the base struct if any
            if (this.baseStructType != null) {
                baseStructType.ensureInitialized();
            }

            // recurse to generic type arguments if any
            if (this.genericTypeSpecialization != null) {
                for (BondType genericTypeArgument : this.genericTypeSpecialization.genericTypeArguments) {
                    if (genericTypeArgument instanceof StructBondType) {
                        ((StructBondType) genericTypeArgument).ensureInitialized();
                    }
                }
            }

            // recurse to fields
            for (StructField structField : this.structFields) {
                if (structField.fieldType instanceof StructBondType) {
                    ((StructBondType) structField.fieldType).ensureInitialized();
                }
            }
        }
    }

    /**
     * Implemented by generated subclasses to initialize the type descriptor.
     */
    protected abstract void initialize();

    /**
     * Used by generated subclasses to serialize declared fields of this struct, excluding inherited fields.
     *
     * @param context contains the runtime context of the serialization
     * @throws IOException if an I/O error occurred
     */
    protected abstract void serializeStructFields(
            SerializationContext context, TStruct value) throws IOException;

    /**
     * Used by generated subclasses to deserialize declared fields of this struct, excluding inherited fields.
     *
     * @param context contains the runtime context of the deserialization
     * @throws IOException if an I/O error occurred
     */
    protected abstract void deserializeStructFields(
            TaggedDeserializationContext context, TStruct value) throws IOException;

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
    public final StructField<?>[] geStructFields() {
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

    @Override
    public final String getName() {
        // rely on generated class
        return this.getValueClass().getSimpleName();
    }

    @Override
    public final String getQualifiedName() {
        // rely on generated class
        return this.getValueClass().getName();
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

    /**
     * Instantiates a new instance of this struct type.
     *
     * @return new struct instance
     */
    public abstract TStruct newInstance();

    @Override
    protected final void serializeValue(SerializationContext context, TStruct value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        context.writer.writeStructBegin(this);
        if (this.baseStructType != null) {
            this.baseStructType.serializeValueHelperForBase(context, value);
        }
        this.serializeStructFields(context, value);
        context.writer.writeStructEnd();
    }

    // recursive helper
    private void serializeValueHelperForBase(
            SerializationContext context, TStruct value) throws IOException {
        if (this.baseStructType != null) {
            this.baseStructType.serializeValueHelperForBase(context, value);
        }
        context.writer.writeBaseBegin(this);
        this.serializeStructFields(context, value);
        context.writer.writeBaseEnd();
    }

    @Override
    protected final TStruct deserializeValue(TaggedDeserializationContext context) throws IOException {
        TStruct value = this.newDefaultValue();
        context.reader.readStructBegin();
        if (this.baseStructType != null) {
            this.baseStructType.deserializeValueHelperForBase(context, value);
        }
        this.deserializeStructFields(context, value);
        context.reader.readStructEnd();
        return value;
    }

    // recursive helper
    private void deserializeValueHelperForBase(
            TaggedDeserializationContext context, TStruct value) throws IOException {
        if (this.baseStructType != null) {
            this.baseStructType.deserializeValueHelperForBase(context, value);
        }
        context.reader.readBaseBegin();
        this.deserializeStructFields(context, value);
        context.reader.readBaseEnd();
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            TStruct value,
            StructField<TStruct> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        // struct fields are never omitted
        context.writer.writeFieldBegin(BondDataType.BT_STRUCT, field.getId(), field);
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
    final boolean equalsInternal(BondType<?> obj) {
        // the caller makes sure that the class of the argument is the same as the class of this object
        StructBondType that = (StructBondType) obj;
        if (this.genericTypeSpecialization != null) {
            return this.genericTypeSpecialization.equals(that.genericTypeSpecialization);
        } else {
            return that.genericTypeSpecialization == null;
        }
    }

    // private top-level serialization entry point
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

    // private top-level deserialization entry point
    TStruct deserialize(TaggedProtocolReader reader) throws IOException {
        TaggedDeserializationContext context = new TaggedDeserializationContext(reader);
        return this.deserializeValue(context);
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

    /**
     * Resolves generic type arguments and caches the result in the type cache.
     *
     * @param resolver             type resolver
     * @param genericTypeArguments generic type arguments
     * @param <TStruct>            the class of the struct value
     * @return a cached uninitialized type descriptor
     */
    protected static <TStruct extends BondSerializable> StructBondType<TStruct> resolveUninitializedWithCaching(
            StructBondTypeResolver<TStruct> resolver, BondType<?>... genericTypeArguments) {
        BondType<?>[] cachedGenericTypeArguments = new BondType[genericTypeArguments.length];
        for (int i = 0; i < genericTypeArguments.length; ++i) {
            cachedGenericTypeArguments[i] = typeCache.get(genericTypeArguments[i]);
        }
        StructBondType<TStruct> type = resolver.resolveUninitialized(cachedGenericTypeArguments);
        return (StructBondType<TStruct>) BondType.typeCache.get(type);
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
        }
    }

    private void initializeSchemaVariantWithDefaultValue(Variant variant, StructField field) {
        variant.nothing = field.isDefaultNothing;
        switch (field.fieldType.getBondDataType().value) {
            case BondDataType.Values.BT_UINT8:
            case BondDataType.Values.BT_UINT16:
            case BondDataType.Values.BT_UINT32:
            case BondDataType.Values.BT_UINT64:
                variant.uint_value = ((Number) field.getDefaultValue()).longValue();
                break;

            case BondDataType.Values.BT_INT8:
            case BondDataType.Values.BT_INT16:
            case BondDataType.Values.BT_INT32:
            case BondDataType.Values.BT_INT64:
                variant.int_value = ((Number) field.getDefaultValue()).longValue();
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

    /**
     * A descriptor of a single field in a struct declaration that encapsulates the details
     * of that field behavior such as initialization, serialization and deserialization.
     *
     * @param <TField> the class of the field value, using corresponding wrappers for primitive types
     */
    protected static abstract class StructField<TField> implements FieldMetadata {

        // accessed by subclasses
        final StructBondType<?> structType;
        final BondType<TField> fieldType;
        final short id;
        final String name;
        final Modifier modifier;
        final boolean isDefaultNothing;

        // used by codegen (and subclasses)
        public StructField(
                StructBondType<?> structType,
                BondType<TField> fieldType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing) {
            this.structType = structType;
            this.fieldType = fieldType;
            this.id = (short) id;
            this.name = name;
            this.modifier = modifier;
            this.isDefaultNothing = isDefaultNothing;
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

        @Override
        public final short getId() {
            return this.id;
        }

        @Override
        public final String getName() {
            return this.name;
        }

        @Override
        public final Modifier getModifier() {
            return this.modifier;
        }

        @Override
        public final boolean isDefaultNothing() {
            return this.isDefaultNothing;
        }

        @Override
        public final TField getDefaultValue() {
            // the default value is the value that we initialize fields with
            return this.initializeObject();
        }

        /**
         * Codegen helper to verify deserialized field.
         *
         * @param isFieldSet a boolean tracking whether the field was set during deserialization
         * @return true if the argument is false and the field needs to be set to the default value
         * @throws InvalidBondDataException if the field is required and was not set
         */
        public final boolean verifyDeserializedField(boolean isFieldSet) throws InvalidBondDataException {
            if (!isFieldSet && this.modifier.value == Modifier.Required.value) {
                // throws
                Throw.raiseRequiredStructFieldIsMissingDeserializationError(this);
            }
            // indicate to the generated code that the field needs to be set to the default value
            return !isFieldSet;
        }

        // codegen helper - initialization
        public abstract TField initializeObject();

        // codegen helper - initialization
        public final SomethingObject<TField> initializeSomethingObject() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeObject(
                SerializationContext context, TField value) throws IOException {
            this.fieldType.serializeField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingObject(
                SerializationContext context, SomethingObject<TField> value) throws IOException {
            this.fieldType.serializeSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final TField deserializeObject(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingObject<TField> deserializeSomethingObject(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return this.fieldType.deserializeSomethingField(context, this);
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
     * Implements the {@link StructField} contract for general object data types.
     * The default value is not explicitly set on the field but rather is determined
     * by the field's type.
     */
    protected static final class ObjectStructField<TField> extends StructField<TField> {

        // used by codegen
        public ObjectStructField(
                StructBondType<?> structType,
                BondType<TField> fieldType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing) {
            super(structType, fieldType, id, name, modifier, isDefaultNothing);
        }

        // codegen helper - initialization
        @Override
        public final TField initializeObject() {
            return this.fieldType.newDefaultValue();
        }
    }

    /**
     * Implements the {@link StructField} contract for the uint8 primitive data type.
     */
    protected static final class UInt8StructField extends StructField<Byte> {

        private final byte defaultValue;

        // used by codegen
        public UInt8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                byte defaultValue) {
            super(structType, BondTypes.UINT8, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Byte initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final byte initializeUInt8() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingByte initializeSomethingUInt8() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeUInt8(
                SerializationContext context, byte value) throws IOException {
            UInt8BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingUInt8(
                SerializationContext context, SomethingByte value) throws IOException {
            UInt8BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final byte deserializeUInt8(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt8BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingByte deserializeSomethingUInt8(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt8BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the uint16 primitive data type.
     */
    protected static final class UInt16StructField extends StructField<Short> {

        private final short defaultValue;

        // used by codegen
        public UInt16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                short defaultValue) {
            super(structType, BondTypes.UINT16, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Short initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final short initializeUInt16() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingShort initializeSomethingUInt16() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeUInt16(
                SerializationContext context, short value) throws IOException {
            UInt16BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingUInt16(
                SerializationContext context, SomethingShort value) throws IOException {
            UInt16BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final short deserializeUInt16(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt16BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingShort deserializeSomethingUInt16(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt16BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the uint32 primitive data type.
     */
    protected static final class UInt32StructField extends StructField<Integer> {

        private final int defaultValue;

        // used by codegen
        public UInt32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                int defaultValue) {
            super(structType, BondTypes.UINT32, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Integer initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final int initializeUInt32() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingInteger initializeSomethingUInt32() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeUInt32(
                SerializationContext context, int value) throws IOException {
            UInt32BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingUInt32(
                SerializationContext context, SomethingInteger value) throws IOException {
            UInt32BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final int deserializeUInt32(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt32BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingInteger deserializeSomethingUInt32(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt32BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the uint64 primitive data type.
     */
    protected static final class UInt64StructField extends StructField<Long> {

        private final long defaultValue;

        // used by codegen
        public UInt64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                long defaultValue) {
            super(structType, BondTypes.UINT64, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Long initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final long initializeUInt64() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingLong initializeSomethingUInt64() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeUInt64(
                SerializationContext context, long value) throws IOException {
            UInt64BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingUInt64(
                SerializationContext context, SomethingLong value) throws IOException {
            UInt64BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final long deserializeUInt64(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt64BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingLong deserializeSomethingUInt64(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return UInt64BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the int8 primitive data type.
     */
    protected static final class Int8StructField extends StructField<Byte> {

        private final byte defaultValue;

        // used by codegen
        public Int8StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                byte defaultValue) {
            super(structType, BondTypes.INT8, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Byte initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final byte initializeInt8() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingByte initializeSomethingInt8() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeInt8(
                SerializationContext context, byte value) throws IOException {
            Int8BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingInt8(
                SerializationContext context, SomethingByte value) throws IOException {
            Int8BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final byte deserializeInt8(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int8BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingByte deserializeSomethingInt8(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int8BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the int16 primitive data type.
     */
    protected static final class Int16StructField extends StructField<Short> {

        private final short defaultValue;

        // used by codegen
        public Int16StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                short defaultValue) {
            super(structType, BondTypes.INT16, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Short initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final short initializeInt16() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingShort initializeSomethingInt16() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeInt16(
                SerializationContext context, short value) throws IOException {
            Int16BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingInt16(
                SerializationContext context, SomethingShort value) throws IOException {
            Int16BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final short deserializeInt16(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int16BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingShort deserializeSomethingInt16(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int16BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the int32 primitive data type.
     */
    protected static final class Int32StructField extends StructField<Integer> {

        private final int defaultValue;

        // used by codegen
        public Int32StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                int defaultValue) {
            super(structType, BondTypes.INT32, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Integer initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final int initializeInt32() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingInteger initializeSomethingInt32() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeInt32(
                SerializationContext context, int value) throws IOException {
            Int32BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingInt32(
                SerializationContext context, SomethingInteger value) throws IOException {
            Int32BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final int deserializeInt32(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int32BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingInteger deserializeSomethingInt32(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int32BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the int64 primitive data type.
     */
    protected static final class Int64StructField extends StructField<Long> {

        private final long defaultValue;

        // used by codegen
        public Int64StructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                long defaultValue) {
            super(structType, BondTypes.INT64, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Long initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final long initializeInt64() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingLong initializeSomethingInt64() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeInt64(
                SerializationContext context, long value) throws IOException {
            Int64BondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingInt64(
                SerializationContext context, SomethingLong value) throws IOException {
            Int64BondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final long deserializeInt64(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int64BondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingLong deserializeSomethingInt64(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return Int64BondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the bool primitive data type.
     */
    protected static final class BoolStructField extends StructField<Boolean> {

        private final boolean defaultValue;

        // used by codegen
        public BoolStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                boolean defaultValue) {
            super(structType, BondTypes.BOOL, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Boolean initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final boolean initializeBool() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingBoolean initializeSomethingBool() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeBool(
                SerializationContext context, boolean value) throws IOException {
            BoolBondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingBool(
                SerializationContext context, SomethingBoolean value) throws IOException {
            BoolBondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final boolean deserializeBool(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return BoolBondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingBoolean deserializeSomethingBool(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return BoolBondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the float primitive data type.
     */
    protected static final class FloatStructField extends StructField<Float> {

        private final float defaultValue;

        // used by codegen
        public FloatStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                float defaultValue) {
            super(structType, BondTypes.FLOAT, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Float initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final float initializeFloat() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingFloat initializeSomethingFloat() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeFloat(
                SerializationContext context, float value) throws IOException {
            FloatBondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingFloat(
                SerializationContext context, SomethingFloat value) throws IOException {
            FloatBondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final float deserializeFloat(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return FloatBondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingFloat deserializeSomethingFloat(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return FloatBondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the double primitive data type.
     */
    protected static final class DoubleStructField extends StructField<Double> {

        private final double defaultValue;

        // used by codegen
        public DoubleStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                double defaultValue) {
            super(structType, BondTypes.DOUBLE, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final Double initializeObject() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final double initializeDouble() {
            return this.defaultValue;
        }

        // codegen helper - initialization
        public final SomethingDouble initializeSomethingDouble() {
            return null;
        }

        // codegen helper - serialization
        public final void serializeDouble(
                SerializationContext context, double value) throws IOException {
            DoubleBondType.serializePrimitiveField(context, value, this);
        }

        // codegen helper - serialization
        public final void serializeSomethingDouble(
                SerializationContext context, SomethingDouble value) throws IOException {
            DoubleBondType.serializePrimitiveSomethingField(context, value, this);
        }

        // codegen helper - deserialization
        public final double deserializeDouble(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return DoubleBondType.deserializePrimitiveField(context, this);
        }

        // codegen helper - deserialization
        public final SomethingDouble deserializeSomethingDouble(
                TaggedDeserializationContext context, boolean wasAlreadyDeserialized) throws IOException {
            this.verifyFieldWasNotYetDeserialized(wasAlreadyDeserialized);
            return DoubleBondType.deserializePrimitiveSomethingField(context, this);
        }
    }

    /**
     * Implements the {@link StructField} contract for the string primitive data type.
     */
    protected static final class StringStructField extends StructField<String> {

        private final String defaultValue;

        // used by codegen
        public StringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                String defaultValue) {
            super(structType, BondTypes.STRING, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final String initializeObject() {
            return this.defaultValue;
        }
    }

    /**
     * Implements the {@link StructField} contract for the wstring primitive data type.
     */
    protected static final class WStringStructField extends StructField<String> {

        private final String defaultValue;

        // used by codegen
        public WStringStructField(
                StructBondType<?> structType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                String defaultValue) {
            super(structType, BondTypes.WSTRING, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final String initializeObject() {
            return this.defaultValue;
        }
    }

    /**
     * Implements the {@link StructField} contract for enum data types.
     */
    protected static final class EnumStructField<TEnum extends BondEnum> extends StructField<TEnum> {

        private final TEnum defaultValue;

        // used by codegen
        public EnumStructField(
                StructBondType<?> structType,
                EnumBondType<TEnum> fieldType,
                int id,
                String name,
                Modifier modifier,
                boolean isDefaultNothing,
                TEnum defaultValue) {
            super(structType, fieldType, id, name, modifier, isDefaultNothing);
            this.defaultValue = defaultValue;
        }

        // codegen helper - initialization
        @Override
        public final TEnum initializeObject() {
            return this.defaultValue;
        }
    }
}
