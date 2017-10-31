// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Implements the {@link BondType} contract for the Bond "double" data type.
 */
public final class DoubleBondType extends PrimitiveBondType<Double> {

    /**
     * The default of primitive values of this type.
     */
    public static final double DEFAULT_VALUE_AS_PRIMITIVE = 0.0D;

    /**
     * The default of object values of this type.
     */
    public static final Double DEFAULT_VALUE_AS_OBJECT = DEFAULT_VALUE_AS_PRIMITIVE;

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "double";

    /**
     * Singleton, public access is via constants in the BondTypes class.
     */
    static final DoubleBondType INSTANCE = new DoubleBondType();

    private DoubleBondType() {
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
        return BondDataType.BT_DOUBLE;
    }

    @Override
    public final Class<Double> getValueClass() {
        return Double.class;
    }

    @Override
    public final Class<Double> getPrimitiveValueClass() {
        return Double.TYPE;
    }

    @Override
    protected final Double newDefaultValue() {
        return DEFAULT_VALUE_AS_OBJECT;
    }

    @Override
    protected final void serializeValue(SerializationContext context, Double value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        serializePrimitiveValue(context, value);
    }

    @Override
    protected final Double deserializeValue(TaggedDeserializationContext context) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final Double deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Double value,
            StructBondType.StructField<Double> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        serializePrimitiveField(context, value, field);
    }

    @Override
    protected final Double deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Double> field) throws IOException {
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
    protected static void serializePrimitiveValue(SerializationContext context, double value) throws IOException {
        context.writer.writeDouble(value);
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(TaggedDeserializationContext)} method
     * for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static double deserializePrimitiveValue(TaggedDeserializationContext context) throws IOException {
        return context.reader.readDouble();
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(UntaggedDeserializationContext, TypeDef)}
     * method for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static double deserializePrimitiveValue(UntaggedDeserializationContext context) throws IOException {
        return context.reader.readDouble();
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
            double value,
            StructBondType.StructField<Double> field) throws IOException {
        if (!field.isDefaultNothing() && field.isOptional() &&
                FloatingPointHelper.doubleEquals(value, field.getDefaultValue())) {
            context.writer.writeFieldOmitted(BondDataType.BT_DOUBLE, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_DOUBLE, field.getId(), field.getFieldDef().metadata);
            context.writer.writeDouble(value);
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
            SomethingDouble value,
            StructBondType.StructField<Double> field) throws IOException {
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
    protected static double deserializePrimitiveField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Double> field) throws IOException {
        // a double value may deserialized from BT_DOUBLE or BT_FLOAT
        if (context.readFieldResult.type.value != BondDataType.BT_DOUBLE.value) {
            if (context.readFieldResult.type.value == BondDataType.BT_FLOAT.value) {
                return context.reader.readFloat();
            }
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        return context.reader.readDouble();
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
    protected static SomethingDouble deserializePrimitiveSomethingField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Double> field) throws IOException {
        return Something.wrap(deserializePrimitiveField(context, field));
    }
}
