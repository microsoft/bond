namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    using examples.schema_view;

    static class Program
    {
        static void Main()
        {
            var example = new Example
            {
                num = 42,
                str = "test",
                items = { 3.14, 0 }
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);
            Marshal.To(writer, example);

            var input = new InputBuffer(output.Data);
            ExampleView view = Unmarshal<ExampleView>.From(input);

            ThrowIfFalse(example.num == view.num);
            ThrowIfFalse(example.str.Equals(view.str, StringComparison.Ordinal));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
