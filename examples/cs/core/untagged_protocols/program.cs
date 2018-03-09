namespace Examples
{
    using System;
    using System.Collections.Generic;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    static class Program
    {
        static void Main()
        {
            // Get runtime schemas for two "versions" of Example schema. 
            // When using untagged protocols the consumer must have access to runtime schema of the data.
            var schema1 = Schema<Example1>.RuntimeSchema;
            var schema2 = Schema<Example2>.RuntimeSchema;

            // Create and cache deserializers for objects of type Example2 from payloads using the two schemas
            var deserializers = new Dictionary<string, Deserializer<SimpleBinaryReader<InputBuffer>>>
            {
                {"Example1", new Deserializer<SimpleBinaryReader<InputBuffer>>(typeof(Example2), schema1)},
                {"Example2", new Deserializer<SimpleBinaryReader<InputBuffer>>(typeof(Example2), schema2)}
            };

            // Create payload serializing an instance of Example1
            var src = new Example1
            {
                Enabled = true,
                Name = "Foo"
            };

            var output = new OutputBuffer();
            var writer = new SimpleBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new SimpleBinaryReader<InputBuffer>(input);

            // Use the precreated deserializer to deserialize an instance of Example2 schema for Example1
            var dst = deserializers["Example1"].Deserialize<Example2>(reader);
            ThrowIfFalse(src.Enabled == dst.Enabled);
            ThrowIfFalse(src.Name == dst.Name);
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
