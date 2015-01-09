namespace Examples
{
    using System;
    using System.Diagnostics;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    public static class BondTypeAliasConverter
    {
        public static Guid Convert(string value, Guid unused)
        {
            return new Guid(value);
        }

        public static string Convert(Guid value, string unused)
        {
            return value.ToString();
        }
    }

    static class Program
    {
        static void Main()
        {
            var src = new Example
            {
                id = new Guid("{DC37ECC5-9E39-49C9-931B-51138D648262}")
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Example>.From(reader);
            Debug.Assert(dst.id == src.id);
        }
    }
}
