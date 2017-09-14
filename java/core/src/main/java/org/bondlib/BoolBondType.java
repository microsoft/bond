// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Implements the {@link BondType} contract for the Bond "bool" data type.
 */
public final class BoolBondType extends PrimitiveBondType<Boolean> {

    /**
     * The default of primitive values of this type.
     */
    public static final boolean DEFAULT_VALUE_AS_PRIMITIVE = false;

    /**
     * The default of object values of this type.
     */
    public static final Boolean DEFAULT_VALUE_AS_OBJECT = Boolean.FALSE;

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "bool";

    /**
     * Singleton, public access is via constants in the BondTypes class.
     */
    static final BoolBondType INSTANCE = new BoolBondType();

    private BoolBondType() {
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
        return BondDataType.BT_BOOL;
    }

    @Override
    public final Class<Boolean> getValueClass() {
        return Boolean.class;
    }

    @Override
    public final Class<Boolean> getPrimitiveValueClass() {
        return Boolean.TYPE;
    }

    @Override
    protected final Boolean newDefaultValue() {
        return DEFAULT_VALUE_AS_OBJECT;
    }

    @Override
    protected final void serializeValue(SerializationContext context, Boolean value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        serializePrimitiveValue(context, value);
    }

    @Override
    protected final Boolean deserializeValue(TaggedDeserializationContext context) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final Boolean deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        return deserializePrimitiveValue(context);
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Boolean value,
            StructBondType.StructField<Boolean> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        serializePrimitiveField(context, value, field);
    }

    @Override
    protected final Boolean deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Boolean> field) throws IOException {
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
    protected static void serializePrimitiveValue(SerializationContext context, boolean value) throws IOException {
        context.writer.writeBool(value);
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(TaggedDeserializationContext)} method
     * for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static boolean deserializePrimitiveValue(TaggedDeserializationContext context) throws IOException {
        return context.reader.readBool();
    }

    /**
     * Implements the behavior of the {@link BondType#deserializeValue(UntaggedDeserializationContext, TypeDef)}
     * method for primitive values.
     *
     * @param context contains the runtime context of the deserialization
     * @return the deserialized value
     * @throws IOException if an I/O error occurred
     */
    protected static boolean deserializePrimitiveValue(UntaggedDeserializationContext context) throws IOException {
        return context.reader.readBool();
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
            boolean value,
            StructBondType.StructField<Boolean> field) throws IOException {
        if (!field.isDefaultNothing() && field.isOptional() && (value == field.getDefaultValue())) {
            context.writer.writeFieldOmitted(BondDataType.BT_BOOL, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_BOOL, field.getId(), field.getFieldDef().metadata);
            context.writer.writeBool(value);
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
            SomethingBoolean value,
            StructBondType.StructField<Boolean> field) throws IOException {
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
    protected static boolean deserializePrimitiveField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Boolean> field) throws IOException {
        // a bool value may be deserialized only from BT_BOOL
        if (context.readFieldResult.type.value != BondDataType.BT_BOOL.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        return context.reader.readBool();
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
    protected static SomethingBoolean deserializePrimitiveSomethingField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Boolean> field) throws IOException {
        return Something.wrap(deserializePrimitiveField(context, field));
    }
}
