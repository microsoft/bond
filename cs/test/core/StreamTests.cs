namespace UnitTest
{
    using System.IO;
    using NUnit.Framework;
    using Bond;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    [TestFixture]
    public class StreamTests
    {
        [Test]
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

        [Test]
        public void StreamBufferReuseTest()
        {
            var buffer = new byte[5 * 1024];
            
            for (var i = 0; i < buffer.Length; ++i)
                buffer[i] = (byte)(i % 256);

            for (var k = 3; k < 20; ++k)
            {
                var stream = new MemoryStream(buffer, 0, buffer.Length, false, true);
                var input = new InputStream(stream, 9);
                
                while (input.Position + k + sizeof(long) < input.Length)
                {
                    var x = input.Position;
                    var bytes = input.ReadBytes(k);
                    input.ReadUInt64();
                    for (var j = 0; j < bytes.Count; ++j)
                        Assert.AreEqual(bytes.Array[bytes.Offset + j], (x + j) % 256);
                }
            }
        }


        delegate void IntTest<T>(T value);

        [Test]
        public void Varint()
        {
            IntTest<ushort> test16 = (value) =>
            {
                var output = new OutputBuffer();
                output.WriteVarUInt16(value);
                var input = new InputBuffer(output.Data);
                Assert.AreEqual(value, input.ReadVarUInt16());
            };

            IntTest<uint> test32 = (value) =>
            {
                var output = new OutputBuffer();
                output.WriteVarUInt32(value);
                var input = new InputBuffer(output.Data);
                Assert.AreEqual(value, input.ReadVarUInt32());
            };

            IntTest<ulong> test64 = (value) =>
            {
                var output = new OutputBuffer();
                output.WriteVarUInt64(value);
                var input = new InputBuffer(output.Data);
                Assert.AreEqual(value, input.ReadVarUInt64());
            };

            test16(ushort.MinValue);
            test16(ushort.MaxValue);
            test32(uint.MinValue);
            test32(uint.MaxValue);
            test64(ulong.MinValue);
            test64(ulong.MaxValue);
        }
    }
}
