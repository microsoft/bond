namespace Examples
{
    using System;
    using System.Diagnostics;
    using System.IO;
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
            if (value.Count != 16)
            {
                throw new InvalidDataException("value must be of length 16");
            }

            byte[] array = value.Array;
            int offset = value.Offset;

            int a =
                  ((int)array[offset + 3] << 24)
                | ((int)array[offset + 2] << 16)
                | ((int)array[offset + 1] << 8)
                | array[offset];
            short b = (short)(((int)array[offset + 5] << 8) | array[offset + 4]);
            short c = (short)(((int)array[offset + 7] << 8) | array[offset + 6]);

            return new Guid(a, b, c,
                array[offset+ 8],
                array[offset+ 9],
                array[offset+ 10],
                array[offset+ 11],
                array[offset+ 12],
                array[offset+ 13],
                array[offset+ 14],
                array[offset+ 15]);
        }

        public static ArraySegment<byte> Convert(Guid value, ArraySegment<byte> unused)
        {
            return new ArraySegment<byte>(value.ToByteArray());
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
