// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Implements the {@link BondType} contract for the Bond "int64" data type.
 */
public final class Int64BondType extends PrimitiveBondType<Long> {

    /**
     * The default of primitive values of this type.
     */
    public static final long DEFAULT_VALUE_AS_PRIMITIVE = 0L;

    /**
     * The default of object values of this type.
     */
    public static final Long DEFAULT_VALUE_AS_OBJECT = DEFAULT_VALUE_AS_PRIMITIVE;

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "int64";

    /**
     * Singleton, public access is via constants in the BondTypes class.
     */
    static final Int64BondType INSTANCE = new Int64BondType();

    private Int64BondType() {
    }

    @Override
    public final String getName() {
        return TYPE_NAME;
    }

    @Override
    public final String getQualifiedName() {
        return TYPE_NAME;
    }

    @Override
    public final BondDataType getBondDataType() {
        return BondDataType.BT_INT64;
    }

    @Override
    public final Class<Long> getValueClass() {
        return Long.class;
    }

    @Override
    public final Class<Long> getPrimitiveValueClass() {
        return Long.TYPE;
    }

    @Override
    protected final Long newDefaultValue() {
        return DEFAULT_VALUE_AS_OBJECT;
    }

    @Override
    protected final void serializeValue(SerializationContext context, Long value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        serializePrimitiveValue(context, value);
    }

    @Override
    protected final Long deserializeValue(TaggedDeserializationContext context) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final Long deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Long value,
            StructBondType.StructField<Long> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        serializePrimitiveField(context, value, field);
    }

    @Override
    protected final Long deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Long> field) throws IOException {
        return deserializePrimitiveField(context, field);
    }

    /**
     * Implements the behavior of the {@link BondType#serializeValue(SerializationContext, Object)} method
     * for primitive values.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize
     * @throws IOException if an I/O error occurred
     */
    protected static void serializePrimitiveValue(SerializationContext context, long value) throws IOException {
        context.writer.writeInt64(value);
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(TaggedDeserializationContext)} method
     * for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static long deserializePrimitiveValue(TaggedDeserializationContext context) throws IOException {
        return context.reader.readInt64();
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(UntaggedDeserializationContext, TypeDef)}
     * method for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static long deserializePrimitiveValue(UntaggedDeserializationContext context) throws IOException {
        return context.reader.readInt64();
    }

    /**
     * Implements the behavior of the
     * {@link BondType#serializeField(SerializationContext, Object, StructBondType.StructField)}
     * for primitive values.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize
     * @param field   descriptor of the field
     * @throws IOException if an I/O error occurred
     */
    protected static void serializePrimitiveField(
            SerializationContext context,
            long value,
            StructBondType.StructField<Long> field) throws IOException {
        if (!field.isDefaultNothing() && field.isOptional() && (value == field.getDefaultValue())) {
            context.writer.writeFieldOmitted(BondDataType.BT_INT64, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_INT64, field.getId(), field.getFieldDef().metadata);
            context.writer.writeInt64(value);
            context.writer.writeFieldEnd();
        }
    }

    /**
     * Implements the behavior of the
     * {@link BondType#serializeSomethingField(SerializationContext, SomethingObject, StructBondType.StructField)}
     * for primitive values.
     *
     * @param context contains the runtime context of the serialization
     * @param value   the value to serialize
     * @param field   descriptor of the field
     * @throws IOException if an I/O error occurred
     */
    protected static void serializePrimitiveSomethingField(
            SerializationContext context,
            SomethingLong value,
            StructBondType.StructField<Long> field) throws IOException {
        if (value != null) {
            serializePrimitiveField(context, value.value, field);
        } else if (!field.isOptional()) {
            // throws
            Throw.raiseNonOptionalFieldValueSetToNothingError(field);
        }
    }

    /**
     * Implements the behavior of the
     * {@link BondType#deserializeField(TaggedDeserializationContext, StructBondType.StructField)}
     * for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @param field   descriptor of the field
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static long deserializePrimitiveField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Long> field) throws IOException {
        // an int64 value may be deserialized from BT_INT64, BT_INT32, BT_INT16 or BT_INT8
        if (context.readFieldResult.type.value != BondDataType.BT_INT64.value) {
            if (context.readFieldResult.type.value == BondDataType.BT_INT32.value) {
                return context.reader.readInt32();
            } else if (context.readFieldResult.type.value == BondDataType.BT_INT16.value) {
                return context.reader.readInt16();
            } else if (context.readFieldResult.type.value == BondDataType.BT_INT8.value) {
                return context.reader.readInt8();
            }
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        return context.reader.readInt64();
    }

    /**
     * Implements the behavior of the
     * {@link BondType#deserializeSomethingField(TaggedDeserializationContext, StructBondType.StructField)}
     * for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @param field   descriptor of the field
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static SomethingLong deserializePrimitiveSomethingField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Long> field) throws IOException {
        return Something.wrap(deserializePrimitiveField(context, field));
    }
}
