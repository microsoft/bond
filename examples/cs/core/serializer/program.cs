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
            var src = new Example
            {
                Name = "FooBar",
                Constants = { 3.14, 6.28 }
            };

            // Create de/serializer for the Example class and Compact Binary protocol.
            // This may take relatively long time so usually the objects should be cached and reused.
            var exampleSerializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Example));
            var exampleDeserializer = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Example));

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            exampleSerializer.Serialize(src, writer);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = exampleDeserializer.Deserialize<Example>(reader);
            ThrowIfFalse(Comparer.Equal(src, dst));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
