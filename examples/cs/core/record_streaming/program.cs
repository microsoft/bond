namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    using examples.record_streaming;

    static class Program
    {
        private const int NumObjects = 9;

        static void Main()
        {
            // Create a stream of serialized structs.
            ArraySegment<byte> output = SerializeStream();

            // Read them all back.
            var input = new InputBuffer(output);
            DeserializeStream(input);
        }

        static ArraySegment<byte> SerializeStream()
        {
            // An output stream can be used to accumulate multiple serialized
            // structs. This can be done by re-using the same writer over the same
            // stream.
            var output = new OutputBuffer();
            // Use Compact Binary protocol for encoding; the data will be written to
            // output which will allocate more memory as needed.
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            for (uint n = 1; n <= NumObjects; ++n)
            {
                var obj = new Struct
                {
                    n = n,
                    str = new string('#', (int)n)
                };

                Serialize.To(writer, obj);
            }

            // At this point output contains the serialized representation
            // of a stream of objects. OutputStream.Data can be used to get
            // the written data.
            return output.Data;
        }

        static void DeserializeStream(InputBuffer input)
        {
            var reader = new CompactBinaryReader<InputBuffer>(input);

            uint numObjectsSeen = 0;

            // Each call to Deserialize.From will advance the underlying
            // stream's Position. When there are no more bytes available,
            // all the structs have been deserialized.
            //
            // You may find that you need to pack serialized Bond structs into a
            // stream in a different way depending on your needs. The reader
            // just needs its underlying stream to be positioned at the
            // beginning of a serialized struct.
            while (input.Position < input.Length)
            {
                var obj = Deserialize<Struct>.From(reader);
                ++numObjectsSeen;

                ThrowIfFalse(numObjectsSeen == obj.n);
                ThrowIfFalse(obj.n == obj.str.Length);
            }

            ThrowIfFalse(numObjectsSeen == NumObjects);
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
