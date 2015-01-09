namespace Examples
{
    using System.Diagnostics;
    using System;
    using System.Linq;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    static class Program
    {
        static void Main()
        {
            var data = Enumerable.Range(0, 255).Select(i => (byte)i).ToArray();
            var src = new Example
            {
                ListOfBlobs =
                {
                    new ArraySegment<byte>(data, 0, 10), 
                    new ArraySegment<byte>(data, 10, 10)
                },

                NullableBlob = new ArraySegment<byte>(data, 20, 10),
                UninitializeBlob = new ArraySegment<byte>(data, 30, 70)
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Example>.From(reader);
            Debug.Assert(Comparer.Equal(src, dst));
        }
    }
}
