namespace Examples
{
    using System.Diagnostics;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    static class Program
    {
        static void Main()
        {
            var src = new Example
            {
                Name = "foo",
                Constants = { 3.14, 6.28 }
            };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Marshal.To(writer, src);

            var input = new InputBuffer(output.Data);

            // We don't need to specify protocol for unmarshaling, 
            // it is determined from information stored in the payload.
            var dst = Unmarshal<Example>.From(input);
            Debug.Assert(Comparer.Equal(src, dst));
        }
    }
}
