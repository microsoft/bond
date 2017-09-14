// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.util.HashMap;

/**
 * Implements the {@link BondType} contract for "nullable" container data types.
 * @param <TValue> the class of the underlying value
 */
public final class NullableBondType<TValue> extends BondType<TValue> {

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "nullable";

    private final BondType<TValue> valueType;
    private final int precomputedHashCode;

    NullableBondType(BondType<TValue> valueType) {
        this.valueType = valueType;
        this.precomputedHashCode = HashCode.computeHashCodeForNullableContainer(valueType);
    }

    /**
     * Retrieves the underlying value type descriptor.
     *
     * @return the underlying value type descriptor
     */
    public final BondType<TValue> getValueType() {
        return this.valueType;
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
        // nullable values are represented in Bond by lists with 0 or 1 elements
        return BondDataType.BT_LIST;
    }

    @Override
    public final Class<TValue> getValueClass() {
        // the same class as the wrapped value
        return this.valueType.getValueClass();
    }

    @Override
    public final Class<TValue> getPrimitiveValueClass() {
        return null;
    }

    @Override
    public final boolean isNullableType() {
        return true;
    }

    @Override
    public final boolean isGenericType() {
        return true;
    }

    @Override
    public final BondType<?>[] getGenericTypeArguments() {
        return new BondType<?>[]{this.valueType};
    }

    @Override
    protected final TValue newDefaultValue() {
        return null;
    }

    @Override
    protected final TValue cloneValue(TValue value) {
        return value == null ? null : this.valueType.cloneValue(value);
    }

    @Override
    protected final void serializeValue(SerializationContext context, TValue value) throws IOException {
        if (value == null) {
            context.writer.writeContainerBegin(0, this.valueType.getBondDataType());
        } else {
            context.writer.writeContainerBegin(1, this.valueType.getBondDataType());
            this.valueType.serializeValue(context, value);
        }
        context.writer.writeContainerEnd();
    }

    @Override
    protected final TValue deserializeValue(TaggedDeserializationContext context) throws IOException {
        context.reader.readListBegin(context.readContainerResult);
        if (context.readContainerResult.elementType.value != this.valueType.getBondDataType().value) {
            // throws
            Throw.raiseContainerElementTypeIsNotCompatibleDeserializationError(
                    "value",
                    context.readContainerResult.elementType,
                    this.valueType.getBondDataType(),
                    this.getFullName());
        }
        TValue value = null;
        if (context.readContainerResult.count == 1) {
            value = this.valueType.deserializeValue(context);
        } else if (context.readContainerResult.count > 1){
            // throws
            Throw.raiseNullableListValueHasMultipleElementsDeserializationError(this.getFullName());
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final TValue deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        TValue value = null;
        final int count = context.reader.readContainerBegin();
        // If count == 0, all we need to do is return null.
        if (count == 1) {
            value = this.valueType.deserializeValue(context, typeDef.element);
        } else if (count > 1) {
            // throws
            Throw.raiseNullableListValueHasMultipleElementsDeserializationError(this.getFullName());
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            TValue value,
            StructBondType.StructField<TValue> field) throws IOException {
        if (!field.isDefaultNothing() && value == null && field.isOptional()) {
            context.writer.writeFieldOmitted(BondDataType.BT_LIST, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_LIST, field.getId(), field.getFieldDef().metadata);
            try {
                this.serializeValue(context, value);
            } catch (InvalidBondDataException e) {
                // throws
                Throw.raiseStructFieldSerializationError(false, field, e, null);
            }
            context.writer.writeFieldEnd();
        }
    }

    @Override
    protected final TValue deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<TValue> field) throws IOException {
        // a nullable value may be deserialized only from BT_LIST
        if (context.readFieldResult.type.value != BondDataType.BT_LIST.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        TValue value = null;
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
        if (obj instanceof NullableBondType<?>) {
            NullableBondType<?> that = (NullableBondType<?>) obj;
            return this.precomputedHashCode == that.precomputedHashCode &&
                    this.valueType.equals(that.valueType);
        } else {
            return false;
        }
    }

    @Override
    final TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        // initialize only with non-default values
        TypeDef typeDef = new TypeDef();
        typeDef.id = this.getBondDataType();
        typeDef.element = this.valueType.createSchemaTypeDef(structDefMap);
        return typeDef;
    }
}
