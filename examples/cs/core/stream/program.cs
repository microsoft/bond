namespace Examples
{
    using System;
    using System.IO;

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
                Constants = {  3.14, 6.28 }
            };

            var stream = new MemoryStream();
            var output = new OutputStream(stream);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Serialize.To(writer, src);

            output.Flush();
            stream.Position = 0;

            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input);

            var dst = Deserialize<Example>.From(reader);
            ThrowIfFalse(Comparer.Equal(src, dst));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
