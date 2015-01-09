namespace Examples
{
    using System.Diagnostics;
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

            Debug.Assert(schema.IsStruct);
            Debug.Assert(schema.StructDef.metadata.qualified_name == "Examples.Example");
            Debug.Assert(schema.StructDef.metadata.attributes["StructAttribute"] == "Value of the attribute");
            Debug.Assert(schema.StructDef.fields[0].metadata.attributes["FieldAttribute"] == "Value of the attribute");
            Debug.Assert(schema.StructDef.fields[0].type.key.id == BondDataType.BT_UINT32);
            Debug.Assert(schema.SchemaDef.structs[1].fields[0].metadata.default_value.string_value == "this is a string");
        }
    }
}
