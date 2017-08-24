// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package org.bondlib;

public final class RuntimeSchema {

    private SchemaDef schemaDef;
    private TypeDef typeDef;

    public RuntimeSchema(SchemaDef schema) {
        this(schema, schema.root);
    }

    public RuntimeSchema(SchemaDef schema, TypeDef type) {
        schemaDef = schema;
        typeDef = type;
    }
    public SchemaDef getSchemaDef() {
        return schemaDef;
    }

    public TypeDef getTypeDef() {
        return typeDef;
    }

    public StructDef getStructDef() {
        return schemaDef.structs.get(typeDef.struct_def);
    }

    public boolean hasValue() {
        return schemaDef != null && typeDef != null;
    }

    public boolean isStruct()
    {
        return hasValue() && typeDef.id == BondDataType.BT_STRUCT;
    }

    public boolean isBonded() {
        return hasValue() && typeDef.bonded_type;
    }

    public boolean isBlob() {
        return hasValue() && typeDef.id == BondDataType.BT_LIST && typeDef.element.id == BondDataType.BT_INT8;
    }

    public boolean isContainer() {
        return hasValue() && typeDef.element != null;
    }

    public boolean isMap() {
        return hasValue() && typeDef.key != null;
    }

    public boolean hasBase() {
        return isStruct() && getStructDef().base_def != null;
    }

    public RuntimeSchema getBaseSchema() {
        if (!isStruct()) {
            throw new IllegalStateException();
        }
        return new RuntimeSchema(schemaDef, getStructDef().base_def);
    }

    public RuntimeSchema getElementSchema() {
        if (!hasValue()) {
            throw new IllegalStateException();
        }
	return new RuntimeSchema(schemaDef, typeDef.element);
    }

    public RuntimeSchema getKeySchema() {
        if (!hasValue()) {
            throw new IllegalStateException();
        }
        return new RuntimeSchema(schemaDef, typeDef.key);
    }

    public RuntimeSchema getFieldSchema(FieldDef field) {
        if (!hasValue()) {
            throw new IllegalStateException();
        }
        return new RuntimeSchema(schemaDef, field.type);
    }

    /**
     * Compare for equality by contents of underlying schemaDef and typeDef
     * @param other object to compare against
     * @return {@code true} if object is also a runtimeSchema and underlying schemaDef and typeDef have equivalent contents
     */
    @Override
    public boolean equals(Object other) {
        if (this == other) {
            return true;
        }
        if (!(other instanceof RuntimeSchema)) {
            return false;
        }
        RuntimeSchema otherSchema = (RuntimeSchema) other;

        return ((this.schemaDef == otherSchema.schemaDef)
                    || ((this.schemaDef != null) && this.schemaDef.equals((otherSchema.schemaDef))))
                && ((this.typeDef == otherSchema.typeDef)
                    || ((this.typeDef != null) && this.typeDef.equals(otherSchema.typeDef)));
    }

    /**
     * Generate hash code based on contents of underlying schemaDef and typeDef
     * @return hash code based on underlying schemaDef and typeDef
     */
    @Override
    public int hashCode() {
        int result = 17;
        result += schemaDef == null ? 0 : schemaDef.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        result += typeDef == null ? 0 : typeDef.hashCode();
        result *= 0xeadbeef;
        result ^= result >> 16;
        return result;
    }

}