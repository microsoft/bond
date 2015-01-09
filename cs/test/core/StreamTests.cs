namespace UnitTest
{
    using System.IO;
    using Microsoft.VisualStudio.TestTools.UnitTesting;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    [TestClass]
    public class StreamTests
    {
        [TestMethod]
        public void StreamPositionLengthTest()
        {
            const int _50MB = 50*1024*1024;

            var from1 = Random.Init<Containers>();
            var from2 = Random.Init<Containers>();
            var stream = new MemoryStream();

            var output = new OutputStream(stream, 11);
            var writer = new CompactBinaryWriter<OutputStream>(output);

            Assert.IsTrue(output.Position == 0);

            Serialize.To(writer, from1);

            var pos = output.Position;

            output.Flush();

            Assert.IsTrue(output.Position == pos);
            Assert.IsTrue(output.Position == stream.Length);

            output.Position = _50MB;

            Serialize.To(writer, from2);
            output.Flush();

            pos = output.Position;

            stream.Position = 0;

            var input = new InputStream(stream);
            var reader = new CompactBinaryReader<InputStream>(input);

            Assert.IsTrue(input.Position == stream.Position);
            Assert.IsTrue(input.Length == stream.Length);

            var to1 = Deserialize<Containers>.From(reader);
            Assert.IsTrue(from1.IsEqual<Containers>(to1));

            Assert.IsTrue(input.Length == stream.Length);

            input.Position = _50MB;

            var to2 = Deserialize<Containers>.From(reader);
            Assert.IsTrue(from2.IsEqual<Containers>(to2));

            Assert.IsTrue(input.Position == pos);
        }
    }
}
