namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    static class Program
    {
        static void Main()
        {
            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            // Get runtime schema for type Example and serialize SchemaDef
            Serialize.To(writer, Schema<Example>.RuntimeSchema.SchemaDef);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var schemaDef = Deserialize<SchemaDef>.From(reader);
            var schema = new RuntimeSchema(schemaDef);

            ThrowIfFalse(schema.IsStruct);
            ThrowIfFalse(schema.StructDef.metadata.qualified_name == "Examples.Example");
            ThrowIfFalse(schema.StructDef.metadata.attributes["StructAttribute"] == "Value of the attribute");
            ThrowIfFalse(schema.StructDef.fields[0].metadata.attributes["FieldAttribute"] == "Value of the attribute");
            ThrowIfFalse(schema.StructDef.fields[0].type.key.id == BondDataType.BT_UINT32);
            ThrowIfFalse(schema.SchemaDef.structs[1].fields[0].metadata.default_value.string_value == "this is a string");
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
