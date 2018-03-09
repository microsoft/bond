namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    using examples.protocol_versions;

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

            // Protocols may have different versions with different features.
            // When serializing/deserializing the same version needs to be used.
            //
            // Marshaling can be used to embed the protocol and version in the
            // payload so the reading side can automatically determine which
            // protocol and version to use.
            {
                // Here, we use Compact Binary v1.
                var output = new OutputBuffer();
                var writer = new CompactBinaryWriter<OutputBuffer>(output, version: 1);
                Serialize.To(writer, obj);

                var input = new InputBuffer(output.Data);
                var reader = new CompactBinaryReader<InputBuffer>(input, version: 1);

                var obj2 = Deserialize<Struct>.From(reader);
                ThrowIfFalse(Comparer.Equal(obj, obj2));
            }

            {
                // Here, we use Compact Binary v2.
                var output = new OutputBuffer();
                var writer = new CompactBinaryWriter<OutputBuffer>(output, version: 2);
                Serialize.To(writer, obj);

                var input = new InputBuffer(output.Data);
                var reader = new CompactBinaryReader<InputBuffer>(input, version: 2);

                var obj2 = Deserialize<Struct>.From(reader);
                ThrowIfFalse(Comparer.Equal(obj, obj2));
            }

            {
                // Here, we Marshal to Compact Binary v2.
                var output = new OutputBuffer();
                var writer = new CompactBinaryWriter<OutputBuffer>(output, version: 2);
                Marshal.To(writer, obj);

                var input = new InputBuffer(output.Data);
                // The protocol and version are determined from the payload
                // itself.
                var obj2 = Unmarshal<Struct>.From(input);
                ThrowIfFalse(Comparer.Equal(obj, obj2));
            }
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
