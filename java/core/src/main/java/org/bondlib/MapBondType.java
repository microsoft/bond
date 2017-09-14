// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Implements the {@link BondType} contract for "map" container data types.
 * @param <TKey> the class of the map keys
 * @param <TValue> the class of the mapped values
 */
public final class MapBondType<TKey, TValue> extends BondType<Map<TKey, TValue>> {

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "map";

    private final PrimitiveBondType<TKey> keyType;
    private final BondType<TValue> valueType;
    private final int precomputedHashCode;

    MapBondType(PrimitiveBondType<TKey> keyType, BondType<TValue> valueType) {
        this.keyType = keyType;
        this.valueType = valueType;
        this.precomputedHashCode = HashCode.computeHashCodeForMapContainer(keyType, valueType);
    }

    /**
     * Retrieves the map key type descriptor.
     *
     * @return the map key type descriptor
     */
    public final BondType<TKey> getKeyType() {
        return this.keyType;
    }

    /**
     * Retrieves the mapped value type descriptor.
     *
     * @return the mapped value type descriptor
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
        return BondDataType.BT_MAP;
    }

    @Override
    public final Class<Map<TKey, TValue>> getValueClass() {
        @SuppressWarnings("unchecked")
        Class<Map<TKey, TValue>> valueClass = (Class<Map<TKey, TValue>>) (Class<?>) Map.class;
        return valueClass;
    }

    @Override
    public final Class<Map<TKey, TValue>> getPrimitiveValueClass() {
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
        return new BondType<?>[]{this.keyType, this.valueType};
    }

    @Override
    protected final Map<TKey, TValue> newDefaultValue() {
        return this.newInstance();
    }

    @Override
    protected final Map<TKey, TValue> cloneValue(Map<TKey, TValue> value) {
        Map<TKey, TValue> clonedValue = this.newDefaultValue();
        for (Map.Entry<TKey, TValue> entry : value.entrySet()) {
            clonedValue.put(this.keyType.cloneValue(entry.getKey()), this.valueType.cloneValue(entry.getValue()));
        }
        return clonedValue;
    }

    /**
     * Instantiates a new instance of this map type.
     *
     * @return new map instance
     */
    public final Map<TKey, TValue> newInstance() {
        return new HashMap<TKey, TValue>();
    }

    @Override
    protected final void serializeValue(SerializationContext context, Map<TKey, TValue> value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        final int count = value.size();
        context.writer.writeContainerBegin(count, this.keyType.getBondDataType(), this.valueType.getBondDataType());
        int i = 0;
        for (Map.Entry<TKey, TValue> entry : value.entrySet()) {
            TKey mapEntryKey = null;
            try {
                mapEntryKey = entry.getKey();
                this.keyType.serializeValue(context, mapEntryKey);
            } catch (InvalidBondDataException e) {
                Throw.raiseMapContainerElementSerializationError(false, this.getFullName(), i, null, e, null);
            }
            TValue mapEntryValue;
            try {
                mapEntryValue = entry.getValue();
                this.valueType.serializeValue(context, mapEntryValue);
            } catch (InvalidBondDataException e) {
                Throw.raiseMapContainerElementSerializationError(false, this.getFullName(), i, mapEntryKey, e, null);
            }
        }
        context.writer.writeContainerEnd();
    }

    @Override
    protected final Map<TKey, TValue> deserializeValue(TaggedDeserializationContext context) throws IOException {
        context.reader.readMapBegin(context.readContainerResult);
        if (context.readContainerResult.keyType.value != this.keyType.getBondDataType().value) {
            // throws
            Throw.raiseContainerElementTypeIsNotCompatibleDeserializationError(
                    "map key",
                    context.readContainerResult.keyType,
                    this.keyType.getBondDataType(),
                    this.getFullName());
        }
        if (context.readContainerResult.elementType.value != this.valueType.getBondDataType().value) {
            // throws
            Throw.raiseContainerElementTypeIsNotCompatibleDeserializationError(
                    "mapped value",
                    context.readContainerResult.elementType,
                    this.valueType.getBondDataType(),
                    this.getFullName());
        }

        // store count in a local variable since readContainerResult may be modified
        // if there are nested containers and thus can't be used inside the loop
        final int count = context.readContainerResult.count;
        final Map<TKey, TValue> value = newDefaultValue();
        for (int i = 0; i < count; ++i) {
            TKey mapEntryKey = null;
            try {
                mapEntryKey = this.keyType.deserializeValue(context);
            } catch (InvalidBondDataException e) {
                Throw.raiseMapContainerElementSerializationError(true, this.getFullName(), i, null, e, null);
            }
            TValue mapEntryValue = null;
            try {
                mapEntryValue = this.valueType.deserializeValue(context);
            } catch (InvalidBondDataException e) {
                Throw.raiseMapContainerElementSerializationError(true, this.getFullName(), i, mapEntryKey, e, null);
            }
            value.put(mapEntryKey, mapEntryValue);
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final Map<TKey, TValue> deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        final int count = context.reader.readContainerBegin();
        final Map<TKey, TValue> value = newDefaultValue();
        final TypeDef keyType = typeDef.key;
        final TypeDef valueType = typeDef.element;
        for (int i = 0; i < count; ++i) {
            TKey mapEntryKey = null;
            try {
                mapEntryKey = this.keyType.deserializeValue(context, keyType);
            } catch (InvalidBondDataException e) {
                Throw.raiseMapContainerElementSerializationError(true, this.getFullName(), i, null, e, null);
            }
            TValue mapEntryValue = null;
            try {
                mapEntryValue = this.valueType.deserializeValue(context, valueType);
            } catch (InvalidBondDataException e) {
                Throw.raiseMapContainerElementSerializationError(true, this.getFullName(), i, mapEntryKey, e, null);
            }
            value.put(mapEntryKey, mapEntryValue);
        }
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Map<TKey, TValue> value,
            StructBondType.StructField<Map<TKey, TValue>> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        final int count = value.size();
        if (!field.isDefaultNothing() && count == 0 && field.isOptional()) {
            context.writer.writeFieldOmitted(BondDataType.BT_MAP, field.getId(), field.getFieldDef().metadata);
        } else {
            context.writer.writeFieldBegin(BondDataType.BT_MAP, field.getId(), field.getFieldDef().metadata);
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
    protected final Map<TKey, TValue> deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Map<TKey, TValue>> field) throws IOException {
        // a map value may be deserialized only from BT_MAP
        if (context.readFieldResult.type.value != BondDataType.BT_MAP.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        Map<TKey, TValue> value = null;
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
        if (obj instanceof MapBondType<?, ?>) {
            MapBondType<?, ?> that = (MapBondType<?, ?>) obj;
            return this.precomputedHashCode == that.precomputedHashCode &&
                    this.keyType.equals(that.keyType) && this.valueType.equals(that.valueType);
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
        typeDef.key = this.keyType.createSchemaTypeDef(structDefMap);
        return typeDef;
    }
}
