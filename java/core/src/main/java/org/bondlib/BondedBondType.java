// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;

/**
 * Implements the {@link BondType} contract for bonded container data type.
 * @param <TStruct> the class of the underlying struct value
 */
public final class BondedBondType<TStruct extends BondSerializable> extends BondType<Bonded<TStruct>> {

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "bonded";

    private final StructBondType<TStruct> valueType;
    private final int precomputedHashCode;
    private final Bonded<TStruct> defaultValue;

    BondedBondType(StructBondType<TStruct> valueType) {
        this.valueType = valueType;
        this.precomputedHashCode = HashCode.computeHashCodeForBondedContainer(valueType);
        this.defaultValue = Bonded.fromObject(this.valueType.newDefaultValue(), this.valueType);
    }

    /**
     * Retrieves the underlying value type descriptor.
     *
     * @return the underlying value type descriptor
     */
    public final StructBondType<TStruct> getValueType() {
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
        return this.valueType.getBondDataType();
    }

    @Override
    public final Class<Bonded<TStruct>> getValueClass() {
        // can't do direct cast
        @SuppressWarnings("unchecked")
        Class<Bonded<TStruct>> valueClass = (Class<Bonded<TStruct>>) (Class<?>) Bonded.class;
        return valueClass;
    }

    @Override
    public final Class<Bonded<TStruct>> getPrimitiveValueClass() {
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
        return new BondType<?>[]{this.valueType};
    }

    @Override
    protected final Bonded<TStruct> newDefaultValue() {
        // since Bonded has to API to expose the mutable struct inside it, the default value can be shared
        return this.defaultValue;
    }

    @Override
    protected final Bonded<TStruct> cloneValue(Bonded<TStruct> value) {
        // since Bonded is immutable it doesn't need to be explicitly cloned
        return value;
    }

    @Override
    protected final void serializeValue(SerializationContext context, Bonded<TStruct> value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        if (context.writer.usesMarshaledBonded()) {
            // We always marshal as Compact v1.
            final ByteArrayOutputStream stream = new ByteArrayOutputStream();
            final ProtocolWriter marshaledWriter = new CompactBinaryWriter(stream, 1);
            Marshal.marshal(value, marshaledWriter);

            // Bonded fields are always prefixed with a fixed-width length.
            context.writer.writeUInt32(stream.size());
            context.writer.writeBytes(stream.toByteArray());
        } else {
            value.serialize(context);
        }
    }

    @Override
    protected final Bonded<TStruct> deserializeValue(TaggedDeserializationContext context) throws IOException {
        // Clone the input stream (or throw an exception if not cloneable) and initialize a new Bonded instance
        // backed by the cloned stream. Then skip reading the original stream until the end of the struct, which
        // is the same number of bytes that would be read by the clone (backing the new Bonded) when deserializing.
        TaggedProtocolReader protocolReader = context.reader.cloneProtocolReader();
        Bonded<TStruct> value = Bonded.fromProtocolReader(protocolReader, this.valueType);
        context.reader.skip(BondDataType.BT_STRUCT);
        return value;
    }

    @Override
    protected final Bonded<TStruct> deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        // Bonded fields are always marshaled and prefixed with a fixed-width length.
        final int bondedLength = context.reader.readUInt32();
        final InputStream clonedInputStream = context.reader.cloneStream();
        final Bonded<TStruct> value = Unmarshal.unmarshal(clonedInputStream, this.valueType);
        context.reader.skipBytes(bondedLength);
        return value;
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Bonded<TStruct> value,
            StructBondType.StructField<Bonded<TStruct>> field) throws IOException {
        // struct (bonded) fields are never omitted
        context.writer.writeFieldBegin(BondDataType.BT_STRUCT, field.getId(), field.getFieldDef().metadata);
        try {
            this.serializeValue(context, value);
        } catch (InvalidBondDataException e) {
            // throws
            Throw.raiseStructFieldSerializationError(false, field, e, null);
        }
        context.writer.writeFieldEnd();
    }

    @Override
    protected final Bonded<TStruct> deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Bonded<TStruct>> field) throws IOException {
        // since bonded applies only to structs, a bonded value may be deserialized only from BT_STRUCT
        if (context.readFieldResult.type.value != BondDataType.BT_STRUCT.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        Bonded<TStruct> value = null;
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
        if (obj instanceof BondedBondType<?>) {
            BondedBondType<?> that = (BondedBondType<?>) obj;
            return this.precomputedHashCode == that.precomputedHashCode &&
                    this.valueType.equals(that.valueType);
        } else {
            return false;
        }
    }

    @Override
    final TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        TypeDef typeDef = this.valueType.createSchemaTypeDef(structDefMap);
        typeDef.bonded_type = true;
        return typeDef;
    }
}
