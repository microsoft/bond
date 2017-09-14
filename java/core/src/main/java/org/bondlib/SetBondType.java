// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Set;

/**
 * Implements the {@link BondType} contract for "set" container data types.
 * @param <TElement> the class of the element values
 */
public final class SetBondType<TElement> extends BondType<Set<TElement>> {

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "set";

    private final PrimitiveBondType<TElement> elementType;
    private final int precomputedHashCode;

    SetBondType(PrimitiveBondType<TElement> elementType) {
        this.elementType = elementType;
        this.precomputedHashCode = HashCode.computeHashCodeForSetContainer(elementType);
    }

    /**
     * Retrieves the element value type descriptor.
     *
     * @return the element value type descriptor
     */
    public final PrimitiveBondType<TElement> getElementType() {
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
        return BondDataType.BT_SET;
    }

    @Override
    public final Class<Set<TElement>> getValueClass() {
        // can't do direct cast
        @SuppressWarnings("unchecked")
        Class<Set<TElement>> valueClass = (Class<Set<TElement>>) (Class<?>) Set.class;
        return valueClass;
    }

    @Override
    public final Class<Set<TElement>> getPrimitiveValueClass() {
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
    protected final Set<TElement> newDefaultValue() {
        return this.newInstance();
    }

    @Override
    protected final Set<TElement> cloneValue(Set<TElement> value) {
        Set<TElement> clonedValue = this.newDefaultValue();
        for (TElement element : value) {
            clonedValue.add(this.elementType.cloneValue(element));
        }
        return clonedValue;
    }

    /**
     * Instantiates a new instance of this set type.
     *
     * @return new set instance
     */
    public final Set<TElement> newInstance() {
        return new HashSet<TElement>();
    }

    @Override
    protected final void serializeValue(SerializationContext context, Set<TElement> value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        final int count = value.size();
        context.writer.writeContainerBegin(count, this.elementType.getBondDataType());
        int i = 0;
        for (TElement element : value) {
            try {
                this.elementType.serializeValue(context, element);
            } catch (InvalidBondDataException e) {
                Throw.raiseListContainerElementSerializationError(false, true, this.getFullName(), i, e, null);
            }
            ++i;
        }
        context.writer.writeContainerEnd();
    }

    @Override
    protected final Set<TElement> deserializeValue(TaggedDeserializationContext context) throws IOException {
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
        final Set<TElement> value = this.newDefaultValue();
        for (int i = 0; i < count; ++i) {
            try {
                TElement element = this.elementType.deserializeValue(context);
                value.add(element);
            } catch (InvalidBondDataException e) {
                Throw.raiseListContainerElementSerializationError(true, true, this.getFullName(), i, e, null);
            }
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final Set<TElement> deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        final int count = context.reader.readContainerBegin();
        final Set<TElement> value = newDefaultValue();
        final TypeDef elementType = typeDef.element;
        for (int i = 0; i < count; ++i) {
            try {
                TElement element = this.elementType.deserializeValue(context, elementType);
                value.add(element);
            } catch (InvalidBondDataException e) {
                Throw.raiseListContainerElementSerializationError(true, true, this.getFullName(), i, e, null);
            }
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Set<TElement> value,
            StructBondType.StructField<Set<TElement>> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        final int count = value.size();
        if (!field.isDefaultNothing() && count == 0 && field.isOptional()) {
            context.writer.writeFieldOmitted(BondDataType.BT_SET, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_SET, field.getId(), field.getFieldDef().metadata);
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
    protected final Set<TElement> deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Set<TElement>> field) throws IOException {
        // a set value may be deserialized only from BT_SET
        if (context.readFieldResult.type.value != BondDataType.BT_SET.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        Set<TElement> value = null;
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
        if (obj instanceof SetBondType<?>) {
            SetBondType<?> that = (SetBondType<?>) obj;
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
