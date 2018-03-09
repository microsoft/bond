namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    public static class BondTypeAliasConverter
    {
        public static decimal Convert(ArraySegment<byte> value, decimal unused)
        {
            var bits = new int[value.Count / sizeof(int)];
            Buffer.BlockCopy(value.Array, value.Offset, bits, 0, bits.Length * sizeof(int));
            return new decimal(bits);
        }

        public static ArraySegment<byte> Convert(decimal value, ArraySegment<byte> unused)
        {
            var bits = decimal.GetBits(value);
            var data = new byte[bits.Length * sizeof(int)];
            Buffer.BlockCopy(bits, 0, data, 0, data.Length);
            return new ArraySegment<byte>(data);
        }
    }

    static class Program
    {
        static void Main()
        {
            var src = new Example
            {
                Price = 9999999999999999999999999999M,
                Numbers = { 79228162514264337593543950335M }
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Example>.From(reader);
            ThrowIfFalse(Comparer.Equal(src, dst));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
