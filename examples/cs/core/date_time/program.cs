namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    public static class BondTypeAliasConverter
    {
        public static long Convert(DateTime value, long unused)
        {
            return value.ToUniversalTime().Ticks;
        }

        public static DateTime Convert(long value, DateTime unused)
        {
            return new DateTime(value, DateTimeKind.Utc);
        }
    }

    static class Program
    {
        static void Main()
        {
            var src = new Example
            {
                Now = DateTime.UtcNow,
                Dates = { new DateTime(2017, 1, 29, 0, 0, 0, DateTimeKind.Utc) }
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
