// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Implements the {@link BondType} contract for (contiguous) "vector" container data types.
 * @param <TElement> the class of the element values
 */
public final class VectorBondType<TElement> extends BondType<List<TElement>> {

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "vector";

    private final BondType<TElement> elementType;
    private final int precomputedHashCode;

    VectorBondType(BondType<TElement> elementType) {
        this.elementType = elementType;
        this.precomputedHashCode = HashCode.computeHashCodeForVectorContainer(elementType);
    }

    /**
     * Retrieves the element value type descriptor.
     *
     * @return the element value type descriptor
     */
    public final BondType<TElement> getElementType() {
        return this.elementType;
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
        // vectors are represented as lists in serialized form
        return BondDataType.BT_LIST;
    }

    @Override
    public final Class<List<TElement>> getValueClass() {
        // can't do direct cast
        @SuppressWarnings("unchecked")
        Class<List<TElement>> valueClass = (Class<List<TElement>>) (Class<?>) List.class;
        return valueClass;
    }

    @Override
    public final Class<List<TElement>> getPrimitiveValueClass() {
        return null;
    }

    @Override
    public final boolean isNullableType() {
        return false;
    }

    @Override
    public final boolean isGenericType() {
        return true;
    }

    @Override
    public final BondType<?>[] getGenericTypeArguments() {
        return new BondType<?>[]{this.elementType};
    }

    @Override
    protected final List<TElement> newDefaultValue() {
        return this.newInstance();
    }

    @Override
    protected final List<TElement> cloneValue(List<TElement> value) {
        List<TElement> clonedValue = newDefaultValue(value.size());
        for (TElement element : value) {
            clonedValue.add(this.elementType.cloneValue(element));
        }
        return clonedValue;
    }

    /**
     * Instantiates a new instance of this list type.
     *
     * @return new list instance
     */
    public final List<TElement> newInstance() {
        // default initial capacity
        return new ArrayList<TElement>();
    }

    private static <TElement> List<TElement> newDefaultValue(int initialCapacity) {
        // custom initial capacity to match element count when deserializing
        return new ArrayList<TElement>(initialCapacity);
    }

    @Override
    protected final void serializeValue(SerializationContext context, List<TElement> value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        final int count = value.size();
        context.writer.writeContainerBegin(count, this.elementType.getBondDataType());
        for (int i = 0; i < count; ++i) {
            try {
                TElement element = value.get(i);
                this.elementType.serializeValue(context, element);
            } catch (InvalidBondDataException e) {
                Throw.raiseListContainerElementSerializationError(false, false, this.getFullName(), i, e, null);
            }
        }
        context.writer.writeContainerEnd();
    }

    @Override
    protected final List<TElement> deserializeValue(TaggedDeserializationContext context) throws IOException {
        context.reader.readListBegin(context.readContainerResult);
        if (context.readContainerResult.elementType.value != this.elementType.getBondDataType().value) {
            // throws
            Throw.raiseContainerElementTypeIsNotCompatibleDeserializationError(
                    "element",
                    context.readContainerResult.elementType,
                    this.elementType.getBondDataType(),
                    this.getFullName());
        }

        // store count in a local variable since readContainerResult may be modified
        // if there are nested containers and thus can't be used inside the loop
        final int count = context.readContainerResult.count;
        final List<TElement> value = newDefaultValue(count);
        for (int i = 0; i < count; ++i) {
            try {
                TElement element = this.elementType.deserializeValue(context);
                value.add(element);
            } catch (InvalidBondDataException e) {
                Throw.raiseListContainerElementSerializationError(true, false, this.getFullName(), i, e, null);
            }
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final List<TElement> deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        final int count = context.reader.readContainerBegin();
        final List<TElement> value = newDefaultValue(count);
        final TypeDef elementType = typeDef.element;
        for (int i = 0; i < count; ++i) {
            try {
                TElement element = this.elementType.deserializeValue(context, elementType);
                value.add(element);
            } catch (InvalidBondDataException e) {
                Throw.raiseListContainerElementSerializationError(true, false, this.getFullName(), i, e, null);
            }
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            List<TElement> value,
            StructBondType.StructField<List<TElement>> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        final int count = value.size();
        if (!field.isDefaultNothing() && count == 0 && field.isOptional()) {
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
    protected final List<TElement> deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<List<TElement>> field) throws IOException {
        // a list value may be deserialized only from BT_LIST
        if (context.readFieldResult.type.value != BondDataType.BT_LIST.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        List<TElement> value = null;
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
        if (obj instanceof VectorBondType<?>) {
            VectorBondType<?> that = (VectorBondType<?>) obj;
            return this.precomputedHashCode == that.precomputedHashCode &&
                    this.elementType.equals(that.elementType);
        } else {
            return false;
        }
    }

    @Override
    final TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        // initialize only with non-default values
        TypeDef typeDef = new TypeDef();
        typeDef.id = this.getBondDataType();
        typeDef.element = this.elementType.createSchemaTypeDef(structDefMap);
        return typeDef;
    }
}
