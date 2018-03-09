namespace Examples
{
    using System;

    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    using Examples.Common;
    using Examples.Common.Protocol;

    static class Program
    {
        static void Main()
        {
            var src = new Message
                          {
                              Header = new Header { Origin = "contoso.com", Destination = "fabrikam.com" },
                              Priority = Priority.Normal,
                              MessagePayload = 42
                          };

            var output = new OutputBuffer();
            var writer = new CompactBinaryWriter<OutputBuffer>(output);

            Serialize.To(writer, src);

            var input = new InputBuffer(output.Data);
            var reader = new CompactBinaryReader<InputBuffer>(input);

            var dst = Deserialize<Message>.From(reader);
            ThrowIfFalse(Comparer.Equal(src, dst));
        }

        static void ThrowIfFalse(bool b)
        {
            if (!b) throw new Exception("Assertion failed");
        }
    }
}
