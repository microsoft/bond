// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;

/**
 * Partially implements the {@link BondType} contract for generated Bond enum data types.
 * Leaves the rest of implementation details to generated subclasses specific to the enum type,
 * which are private classes nested within the enum class and implement the Singleton pattern.
 * @param <TEnum> the class of the enum value
 */
public abstract class EnumBondType<TEnum extends BondEnum<TEnum>> extends PrimitiveBondType<TEnum> {

    /**
     * Used by generated subclasses (and only by generated subclasses) to instantiate the type descriptor.
     */
    protected EnumBondType() {
    }

    @Override
    public final BondDataType getBondDataType() {
        // enum values are represented as int32
        return BondDataType.BT_INT32;
    }

    @Override
    public final String getName() {
        // rely on the generated class
        return this.getValueClass().getSimpleName();
    }

    @Override
    public final String getQualifiedName() {
        // rely on the generated class
        return this.getValueClass().getName();
    }

    @Override
    public final Class<TEnum> getPrimitiveValueClass() {
        return null;
    }

    @Override
    protected final TEnum newDefaultValue() {
        // a default for enum type is the enum value with the underlying integer set to 0
        return this.getEnumValue(0);
    }

    /**
     * Returns an enum value for the given integer.
     *
     * @param value an integer value
     * @return the enum value corresponding to the argument, never null
     */
    public abstract TEnum getEnumValue(int value);

    @Override
    protected final void serializeValue(SerializationContext context, TEnum value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        context.writer.writeInt32(value.getValue());
    }

    @Override
    protected final TEnum deserializeValue(TaggedDeserializationContext context) throws IOException {
        return this.getEnumValue(context.reader.readInt32());
    }

    @Override
    protected final TEnum deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        return this.getEnumValue(context.reader.readInt32());
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            TEnum value,
            StructBondType.StructField<TEnum> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        int intValueToSerialize = value.getValue();
        if (!field.isDefaultNothing() && field.isOptional() &&
                intValueToSerialize == field.getDefaultValue().getValue()) {
            context.writer.writeFieldOmitted(BondDataType.BT_INT32, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_INT32, field.getId(), field.getFieldDef().metadata);
            context.writer.writeInt32(intValueToSerialize);
            context.writer.writeFieldEnd();
        }
    }

    @Override
    protected final TEnum deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<TEnum> field) throws IOException {
        // an enum (int32) value may be deserialized from BT_INT32, BT_INT16 or BT_INT8
        if (context.readFieldResult.type.value != BondDataType.BT_INT32.value) {
            if (context.readFieldResult.type.value == BondDataType.BT_INT16.value) {
                return this.getEnumValue(context.reader.readInt16());
            } else if (context.readFieldResult.type.value == BondDataType.BT_INT8.value) {
                return this.getEnumValue(context.reader.readInt8());
            }
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        return this.getEnumValue(context.reader.readInt32());
    }
}
