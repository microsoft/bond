// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Implements the {@link BondType} contract for the Bond "uint32" data type.
 */
public final class UInt32BondType extends PrimitiveBondType<Integer> {

    /**
     * The default of primitive values of this type.
     */
    public static final int DEFAULT_VALUE_AS_PRIMITIVE = 0;

    /**
     * The default of object values of this type.
     */
    public static final Integer DEFAULT_VALUE_AS_OBJECT = DEFAULT_VALUE_AS_PRIMITIVE;

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "uint32";

    /**
     * Singleton, public access is via constants in the BondTypes class.
     */
    static final UInt32BondType INSTANCE = new UInt32BondType();

    private UInt32BondType() {
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
        return BondDataType.BT_UINT32;
    }

    @Override
    public final Class<Integer> getValueClass() {
        return Integer.class;
    }

    @Override
    public final Class<Integer> getPrimitiveValueClass() {
        return Integer.TYPE;
    }

    @Override
    protected final Integer newDefaultValue() {
        return DEFAULT_VALUE_AS_OBJECT;
    }

    @Override
    protected final void serializeValue(SerializationContext context, Integer value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        serializePrimitiveValue(context, value);
    }

    @Override
    protected final Integer deserializeValue(TaggedDeserializationContext context) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final Integer deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final void serializeField(
        SerializationContext context,
        Integer value,
        StructBondType.StructField<Integer> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        serializePrimitiveField(context, value, field);
    }

    @Override
    protected final Integer deserializeField(
        TaggedDeserializationContext context,
        StructBondType.StructField<Integer> field) throws IOException {
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
    protected static void serializePrimitiveValue(SerializationContext context, int value) throws IOException {
        context.writer.writeUInt32(value);
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(TaggedDeserializationContext)} method
     * for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static int deserializePrimitiveValue(TaggedDeserializationContext context) throws IOException {
        return context.reader.readUInt32();
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(UntaggedDeserializationContext, TypeDef)}
     * method for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static int deserializePrimitiveValue(UntaggedDeserializationContext context) throws IOException {
        return context.reader.readUInt32();
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
        int value,
        StructBondType.StructField<Integer> field) throws IOException {
        if (!field.isDefaultNothing() && field.isOptional() && (value == field.getDefaultValue())) {
            context.writer.writeFieldOmitted(BondDataType.BT_UINT32, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_UINT32, field.getId(), field.getFieldDef().metadata);
            context.writer.writeUInt32(value);
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
        SomethingInteger value,
        StructBondType.StructField<Integer> field) throws IOException {
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
    protected static int deserializePrimitiveField(
        TaggedDeserializationContext context,
        StructBondType.StructField<Integer> field) throws IOException {
        // an uint32 value may be deserialized from BT_UINT32, BT_UINT16 or BT_UINT8
        if (context.readFieldResult.type.value != BondDataType.BT_UINT32.value) {
            if (context.readFieldResult.type.value == BondDataType.BT_UINT16.value) {
                return context.reader.readUInt16();
            } else if (context.readFieldResult.type.value == BondDataType.BT_UINT8.value) {
                return context.reader.readUInt8();
            }
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        return context.reader.readUInt32();
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
    protected static SomethingInteger deserializePrimitiveSomethingField(
        TaggedDeserializationContext context,
        StructBondType.StructField<Integer> field) throws IOException {
        return Something.wrap(deserializePrimitiveField(context, field));
    }
}
