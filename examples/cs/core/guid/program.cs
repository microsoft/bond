namespace Examples
{
    using System;
    using System.Diagnostics;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    public static class BondTypeAliasConverter
    {
        #region Guid-string conversion

        public static Guid Convert(string value, Guid unused)
        {
            return new Guid(value);
        }

        public static string Convert(Guid value, string unused)
        {
            return value.ToString();
        }

        #endregion

        #region Guid-blob conversion

        public static Guid Convert(ArraySegment<byte> value, Guid unused)
        {
            var bits = new byte[16];
            Buffer.BlockCopy(value.Array, value.Offset, bits, 0, 16);
            return new Guid(bits);
        }

        public static ArraySegment<byte> Convert(Guid value, ArraySegment<byte> unused)
        {
            return new ArraySegment<byte>(value.ToByteArray(), 0, 16);
        }

        #endregion
    }

    static class Program
    {
        static void Main()
        {
            var src = new Example
            {
                id = new Guid("{DC37ECC5-9E39-49C9-931B-51138D648262}"),
                id_bin = new Guid("{0F5F6768-1608-4D5C-A11D-B176D0D792B5}"),
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Example>.From(reader);
            Debug.Assert(dst.id == src.id);
            Debug.Assert(dst.id_bin == src.id_bin);
        }
    }
}
