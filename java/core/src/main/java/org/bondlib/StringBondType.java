// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Implements the {@link BondType} contract for the Bond "string" data type.
 */
public final class StringBondType extends PrimitiveBondType<String> {

    /**
     * The default of object values of this type.
     */
    public static final String DEFAULT_VALUE_AS_OBJECT = "";

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "string";

    /**
     * Singleton, public access is via constants in the BondTypes class.
     */
    static final StringBondType INSTANCE = new StringBondType();

    private StringBondType() {
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
        return BondDataType.BT_STRING;
    }

    @Override
    public final Class<String> getValueClass() {
        return String.class;
    }

    @Override
    public final Class<String> getPrimitiveValueClass() {
        return null;
    }

    @Override
    protected final String newDefaultValue() {
        return DEFAULT_VALUE_AS_OBJECT;
    }

    @Override
    protected final void serializeValue(SerializationContext context, String value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        context.writer.writeString(value);
    }

    @Override
    protected final String deserializeValue(TaggedDeserializationContext context) throws IOException {
        return context.reader.readString();
    }

    @Override
    protected final String deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        return context.reader.readString();
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            String value,
            StructBondType.StructField<String> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        if (field.isOptional() && value.equals(field.getDefaultValue())) {
            context.writer.writeFieldOmitted(BondDataType.BT_STRING, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_STRING, field.getId(), field.getFieldDef().metadata);
            context.writer.writeString(value);
            context.writer.writeFieldEnd();
        }
    }

    @Override
    protected final String deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<String> field) throws IOException {
        // a string value may be deserialized only from BT_STRING
        if (context.readFieldResult.type.value != BondDataType.BT_STRING.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        return context.reader.readString();
    }
}
