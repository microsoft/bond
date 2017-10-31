// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Implements the {@link BondType} contract for the Bond "float" data type.
 */
public final class FloatBondType extends PrimitiveBondType<Float> {

    /**
     * The default of primitive values of this type.
     */
    public static final float DEFAULT_VALUE_AS_PRIMITIVE = 0.0F;

    /**
     * The default of object values of this type.
     */
    public static final Float DEFAULT_VALUE_AS_OBJECT = DEFAULT_VALUE_AS_PRIMITIVE;

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "float";

    /**
     * Singleton, public access is via constants in the BondTypes class.
     */
    static final FloatBondType INSTANCE = new FloatBondType();

    private FloatBondType() {
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
        return BondDataType.BT_FLOAT;
    }

    @Override
    public final Class<Float> getValueClass() {
        return Float.class;
    }

    @Override
    public final Class<Float> getPrimitiveValueClass() {
        return Float.TYPE;
    }

    @Override
    protected final Float newDefaultValue() {
        return DEFAULT_VALUE_AS_OBJECT;
    }

    @Override
    protected final void serializeValue(SerializationContext context, Float value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        serializePrimitiveValue(context, value);
    }

    @Override
    protected final Float deserializeValue(TaggedDeserializationContext context) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final Float deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Float value,
            StructBondType.StructField<Float> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        serializePrimitiveField(context, value, field);
    }

    @Override
    protected final Float deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Float> field) throws IOException {
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
    protected static void serializePrimitiveValue(SerializationContext context, float value) throws IOException {
        context.writer.writeFloat(value);
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(TaggedDeserializationContext)} method
     * for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static float deserializePrimitiveValue(TaggedDeserializationContext context) throws IOException {
        return context.reader.readFloat();
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(UntaggedDeserializationContext, TypeDef)}
     * method for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static float deserializePrimitiveValue(UntaggedDeserializationContext context) throws IOException {
        return context.reader.readFloat();
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
            float value,
            StructBondType.StructField<Float> field) throws IOException {
        if (!field.isDefaultNothing() && field.isOptional() &&
                FloatingPointHelper.floatEquals(value, field.getDefaultValue())) {
            context.writer.writeFieldOmitted(BondDataType.BT_FLOAT, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_FLOAT, field.getId(), field.getFieldDef().metadata);
            context.writer.writeFloat(value);
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
            SomethingFloat value,
            StructBondType.StructField<Float> field) throws IOException {
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
    protected static float deserializePrimitiveField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Float> field) throws IOException {
        // a float value may deserialized only from BT_FLOAT
        if (context.readFieldResult.type.value != BondDataType.BT_FLOAT.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        return context.reader.readFloat();
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
    protected static SomethingFloat deserializePrimitiveSomethingField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Float> field) throws IOException {
        return Something.wrap(deserializePrimitiveField(context, field));
    }
}
