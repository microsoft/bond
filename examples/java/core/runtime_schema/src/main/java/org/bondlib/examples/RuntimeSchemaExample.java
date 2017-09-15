package org.bondlib.examples;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import org.bondlib.*;

// See build.gradle for namespace mapping
import org.bondlib.examples.runtime_schema.Example;

public class RuntimeSchemaExample {

    public static void main(final String[] args) throws IOException {

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<SchemaDef> serializer = new Serializer<>();
        serializer.serialize(Example.BOND_TYPE.buildSchemaDef(), writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());

        final CompactBinaryReader reader = new CompactBinaryReader(input, 1);
        final Deserializer<SchemaDef> deserializer = new Deserializer<>(SchemaDef.BOND_TYPE);

        final SchemaDef schema = deserializer.deserialize(reader);

        assert schema.structs.get(0).metadata.qualified_name.equals("Examples.Example");

        // TODO: Java SchemaDef does not yet support attributes
        // assert schema.getStructDef().metadata.attributes.get("StructAttribute").equals("Value of the string");
        // assert schema.getStructDef().fields.get(0).metadata.attributes.get("FieldAttribute").equals("Value of the string");

        assert schema.structs.get(0).fields.get(0).type.key.id == BondDataType.BT_UINT32;
        assert schema.structs.get(1).fields.get(0).metadata.default_value.string_value.equals("this is a string");
    }

}
