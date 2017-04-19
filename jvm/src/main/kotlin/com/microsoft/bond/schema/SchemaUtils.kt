package com.microsoft.bond.schema

import com.microsoft.bond.SchemaDef
import com.microsoft.bond.StructDef

object SchemaUtils {
    @JvmStatic
    fun add(schema: SchemaDef, struct: StructDef): Int {
        val structName = struct.metadata.qualified_name
        for ((i, s) in (0..schema.structs.size).zip(schema.structs)) {
            if (structName == s.metadata.qualified_name) {
                return i
            }
        }

        // FIXME: Copy and recur.
        schema.structs.add(struct)
        return schema.structs.size - 1
    }
}
