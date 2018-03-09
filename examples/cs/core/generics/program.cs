namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    static class Program
    {
        static void Main()
        {
            // The Example type internally uses instances generic schemas Generic1 and Generic2
            var src = new Example
            {
                Field = { Field = new Generic2<int> { Field = 13 } }
            };

            // We can also instantiate generic schema in the C# program
            var src1 = new Generic1<Example> { Field = src };
            var src2 = new Generic2<double> {Field = 3.14};

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);
            Serialize.To(writer, src1);
            Serialize.To(writer, src2);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Example>.From(reader);
            ThrowIfFalse(Comparer.Equal(src, dst));

            var dst1 = Deserialize<Generic1<Example>>.From(reader);
            ThrowIfFalse(Comparer.Equal(src1, dst1));

            var dst2 = Deserialize<Generic2<double>>.From(reader);
            ThrowIfFalse(Comparer.Equal(src2, dst2));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
