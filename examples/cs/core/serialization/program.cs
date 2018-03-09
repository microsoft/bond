namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    using examples.serialization;

    static class Program
    {
        static void Main()
        {
            var obj = new Struct
            {
                n = 0x1000,
                str = "test",
                items = { 3.14, 0 }
            };

            // OutputBuffer implements the Bond output stream interface on top
            // of a memory buffer.
            var output = new OutputBuffer();
            // Use the Compact Binary protocol for encoding; the data will be
            // written to the OutputBuffer which will allocate memory as needed.
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            // The first calls to Serialize.To and Deserialize<T>.From can take
            // a relatively long time because they generate the
            // serializer/deserializer for a given type, protocol, and stream.
            // See the serializer example for a way to control this.
            Serialize.To(writer, obj);

            // At this point the OutputBuffer contains a serialized
            // representation of the object.

            // Use the Compact Binary protocol for decoding (the same protocol
            // that was used for encoding) and InputBuffer which implements the
            // input stream interface on top of a memory blob.
            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var obj2 = Deserialize<Struct>.From(reader);
            ThrowIfFalse(Comparer.Equal(obj, obj2));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
