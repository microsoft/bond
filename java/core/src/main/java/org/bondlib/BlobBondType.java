// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Implements the {@link BondType} contract for the Bond "blob" data type.
 */
public final class BlobBondType extends BondType<Blob> {

    /**
     * The default of values of this type.
     * An immutable singleton value (empty).
     */
    public static final Blob DEFAULT_VALUE = new Blob();

    /**
     * The name of the type as it appears in Bond schemas.
     */
    public static final String TYPE_NAME = "blob";

    /**
     * Singleton, public access is via constants in the BondTypes class.
     */
    static final BlobBondType INSTANCE = new BlobBondType();

    private BlobBondType() {
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
        // a blob is equivalent to list<int8>
        return BondDataType.BT_LIST;
    }

    @Override
    public final Class<Blob> getValueClass() {
        return Blob.class;
    }

    @Override
    public final Class<Blob> getPrimitiveValueClass() {
        return null;
    }

    @Override
    public final boolean isNullableType() {
        return false;
    }

    @Override
    public final boolean isGenericType() {
        return false;
    }

    @Override
    public final BondType<?>[] getGenericTypeArguments() {
        return null;
    }

    @Override
    protected final Blob newDefaultValue() {
        return DEFAULT_VALUE;
    }

    @Override
    protected final Blob cloneValue(Blob value) {
        return new Blob(Arrays.copyOf(value.getData(), value.getData().length));
    }

    @Override
    protected final void serializeValue(SerializationContext context, Blob value) throws IOException {
        this.verifyNonNullableValueIsNotSetToNull(value);
        context.writer.writeContainerBegin(value.getData().length, BondDataType.BT_INT8);
        context.writer.writeBytes(value.getData());
        context.writer.writeContainerEnd();
    }

    @Override
    protected final Blob deserializeValue(TaggedDeserializationContext context) throws IOException {
        context.reader.readListBegin(context.readContainerResult);
        if (context.readContainerResult.elementType.value != BondDataType.BT_INT8.value) {
            // throws
            Throw.raiseContainerElementTypeIsNotCompatibleDeserializationError(
                    "element",
                    context.readContainerResult.elementType,
                    BondDataType.BT_INT8,
                    this.getFullName());
        }
        final Blob value = new Blob(context.reader.readBytes(context.readContainerResult.count));
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final Blob deserializeValue(
        UntaggedDeserializationContext context,
        TypeDef typeDef) throws IOException {
        final int count = context.reader.readContainerBegin();
        final Blob value = new Blob(context.reader.readBytes(count));
        context.reader.readContainerEnd();
        return value;
    }

    @Override
    protected final void serializeField(
            SerializationContext context,
            Blob value,
            StructBondType.StructField<Blob> field) throws IOException {
        this.verifySerializedNonNullableFieldIsNotSetToNull(value, field);
        if (value.getData().length == 0 && field.isOptional()) {
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
    protected final Blob deserializeField(
            TaggedDeserializationContext context,
            StructBondType.StructField<Blob> field) throws IOException {
        // a blob value may be deserialized only from BT_LIST
        if (context.readFieldResult.type.value != BondDataType.BT_LIST.value) {
            // throws
            Throw.raiseFieldTypeIsNotCompatibleDeserializationError(context.readFieldResult.type, field);
        }
        Blob value = null;
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
        // since the class is a singleton, delegate to the identity of the implementation class
        return this.getClass().hashCode();
    }

    @Override
    public final boolean equals(Object obj) {
        // since the class is a singleton, delegate to the identity of the implementation class
        return obj != null && this.getClass() == obj.getClass();
    }

    @Override
    final TypeDef createSchemaTypeDef(HashMap<StructBondType<?>, StructDefOrdinalTuple> structDefMap) {
        // initialize only with non-default values
        TypeDef typeDef = new TypeDef();
        typeDef.id = this.getBondDataType();
        typeDef.element = BondTypes.INT8.createSchemaTypeDef(structDefMap);
        return typeDef;
    }
}
