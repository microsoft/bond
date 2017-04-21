package com.microsoft.bond.schema

import com.microsoft.bond.*

/**
 * Utility functions for static schema generation.
 */
object SchemaUtils {
    /**
     * Add a cloned StructDef to a SchemaDef's struct list if it isn't already
     * present. Threads a class instance through to find the Java classes of
     * fields of type BT_Struct by reflection.
     * @return the index of the new or existing StructDef in that list.
     */
    @JvmStatic
    fun <T> add(schema: SchemaDef, structDef: StructDef, clazz: Class<T>): Int {
        val structName = structDef.metadata.qualified_name
        for ((i, s) in (0..schema.structs.size).zip(schema.structs)) {
            if (structName == s.metadata.qualified_name) {
                return i
            }
        }

        val newStructDef = cloneStructDef(structDef, schema, clazz)
        schema.structs.add(newStructDef)
        return schema.structs.size - 1
    }

    /**
     * Clone a StructDef, sharing fields where possible and cloning one that are
     * specific to the closure of schema.
     */
    fun <T> cloneStructDef(original: StructDef, schema: SchemaDef, clazz: Class<T>): StructDef {
        val cloned = StructDef()

        cloned.metadata = original.metadata

        // TODO:
        //cloned.base_def = cloneTypeDef(original.base_def, schema, clazz)

        for (field in original.fields) {
            cloned.fields.add(cloneFieldDef(field, schema, clazz))
        }

        return cloned
    }

    /**
     * Clone a FieldDef, sharing fields where possible and cloning ones that are
     * specific to the closure of schema.
     */
    fun <T> cloneFieldDef(original: FieldDef?, schema: SchemaDef, clazz: Class<T>): FieldDef? {
        if (original == null) return null

        val cloned = FieldDef()

        cloned.id = original.id
        cloned.metadata = original.metadata

        cloned.type = cloneTypeDef(original.type, original.metadata.name, schema, clazz)

        return cloned
    }

    /**
     * Clone a TypeDef, sharing fields where possible and cloning ones that are
     * specific to the closure of schema.
     */
    fun <T> cloneTypeDef(original: TypeDef?, fieldName: String, schema: SchemaDef, clazz: Class<T>): TypeDef? {
        if (original == null) return null

        val cloned = TypeDef()

        cloned.bonded_type = original.bonded_type
        cloned.id = original.id

        if (original.id == BondDataType.BT_STRUCT) {
            val structField = clazz.getDeclaredField(fieldName).get(clazz.newInstance())
            val structClass = structField.javaClass
            val structDef = structClass.getDeclaredField("STRUCT_DEF").get(null) as StructDef
            cloned.struct_def = add<Any?>(schema, structDef, structClass)
        } else {
            cloned.struct_def = 0
        }

        // TODO:
        //cloned.element = cloneTypeDef(original.element, schema, clazz)
        //cloned.key = cloneTypeDef(original.key, schema, clazz)

        return cloned
    }
}
