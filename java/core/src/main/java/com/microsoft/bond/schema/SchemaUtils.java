// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

package com.microsoft.bond.schema;

import com.microsoft.bond.*;

import java.lang.reflect.Field;

/**
 * Utility functions for static schema generation.
 */
public final class SchemaUtils {
    private SchemaUtils() {}

    /**
     * Add a cloned StructDef to a SchemaDef's struct list if it isn't already
     * present. Threads a class instance through to find the Java classes of
     * fields of type BT_Struct by reflection.
     * @return the index of the new or existing StructDef in that list.
     */
    public static short add(final SchemaDef schema, final StructDef structDef, final Class<?> clazz) {
        final String structName = structDef.metadata.qualified_name;
        for (int i = 0; i < schema.structs.size(); i++) {
            if (structName.equals(schema.structs.get(i).metadata.qualified_name)) {
                return (short) i;
            }
        }

        final StructDef newStructDef = cloneStructDef(structDef, schema, clazz);
        schema.structs.add(newStructDef);
        return (short) (schema.structs.size() - 1);
    }

    /**
     * Clone a StructDef, sharing fields where possible and cloning one that are
     * specific to the closure of schema.
     */
    public static StructDef cloneStructDef(final StructDef original, final SchemaDef schema, final Class<?> clazz) {
        final StructDef cloned = new StructDef();

        cloned.metadata = original.metadata;

        // TODO:
        //cloned.base_def = cloneTypeDef(original.base_def, schema, clazz);

        for (final FieldDef field : original.fields) {
            cloned.fields.add(cloneFieldDef(field, schema, clazz));
        }

        return cloned;
    }

    /**
     * Clone a FieldDef, sharing fields where possible and cloning ones that are
     * specific to the closure of schema.
     */
    public static FieldDef cloneFieldDef(final FieldDef original, final SchemaDef schema, final Class<?> clazz) {
        if (original == null) { return null; }

        final FieldDef cloned = new FieldDef();

        cloned.id = original.id;
        cloned.metadata = original.metadata;

        cloned.type = cloneTypeDef(original.type, original.metadata.name, schema, clazz);

        return cloned;
    }

    /**
     * Clone a TypeDef, sharing fields where possible and cloning ones that are
     * specific to the closure of schema.
     */
    public static TypeDef cloneTypeDef(
        final TypeDef original, final String fieldName, final SchemaDef schema, final Class<?> clazz) {
        if (original == null) { return null; }

        final TypeDef cloned = new TypeDef();

        cloned.bonded_type = original.bonded_type;
        cloned.id = original.id;

        if (original.id.equals(BondDataType.BT_STRUCT)) {
            // If this reflection fails, something is seriously wrong with
            // codegen. There is no sensible recovery from this.
            try {
                final Field structField = (Field) clazz.getDeclaredField(fieldName).get(clazz.newInstance());
                final Class structClass = structField.getDeclaringClass();
                final StructDef structDef = ((StructDef) structClass.getDeclaredField("STRUCT_DEF").get(null));
                cloned.struct_def = add(schema, structDef, structClass);
            } catch (IllegalAccessException e) {
                throw new RuntimeException("exception while constructing schema", e);
            } catch (InstantiationException e) {
                throw new RuntimeException("exception while constructing schema", e);
            } catch (NoSuchFieldException e) {
                throw new RuntimeException("exception while constructing schema", e);
            } catch (ClassCastException e) {
                throw new RuntimeException("exception while constructing schema", e);
            }
        } else {
            cloned.struct_def = 0;
        }

        // TODO:
        //cloned.element = cloneTypeDef(original.element, schema, clazz);
        //cloned.key = cloneTypeDef(original.key, schema, clazz);

        return cloned;
    }
}
