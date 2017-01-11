namespace UnitTest
{
    using System;
    using System.IO;
    using System.Text;
    using System.Xml;
    using Bond;
    using Bond.IO.Unsafe;
    using Bond.Protocols;
    using NUnit.Framework;

    [TestFixture]
    public class DeserializerControlsTests
    {
        // Data that is designed to preallocate an extremely large container/blob
        // (0x7F7F7F7F elements/bytes) when used with SimpleBinary and a struct that
        // starts with a container/blob
        static readonly byte[] badData = new byte[] { 0x7F, 0x7F, 0x7F, 0x7F, 0x7f };

        // Data for a simple valid blob
        static readonly byte[] blobData = new byte[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 };

        static T DeserializeBadData<T>()
        {
            // Untagged protocol used to trigger errors more easily
            var reader = new SimpleBinaryReader<InputBuffer>(new InputBuffer(badData));
            return new Deserializer<SimpleBinaryReader<InputBuffer>>(typeof(T)).Deserialize<T>(reader);
        }

        // Restore the default settings at the start and end of each test
        [SetUp]
        [TearDown]
        public void RestoreDefaults()
        {
            DeserializerControls.Active = DeserializerControls.Default;
        }

        void RoundtripObject<T>(T obj)
        {
            // SimpleBinary
            {
                var output = new OutputBuffer();
                var writer = new SimpleBinaryWriter<OutputBuffer>(output);
                Serialize.To(writer, obj);

                var input = new InputBuffer(output.Data);
                var reader = new SimpleBinaryReader<InputBuffer>(input);
                T obj2 = Deserialize<T>.From(reader);

                Assert.True(Comparer.Equal<T>(obj, obj2));
            }

            // CompactBinary
            {
                var output = new OutputBuffer();
                var writer = new CompactBinaryWriter<OutputBuffer>(output);
                Serialize.To(writer, obj);

                var input = new InputBuffer(output.Data);
                var reader = new CompactBinaryReader<InputBuffer>(input);
                T obj2 = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(T)).Deserialize<T>(reader);

                Assert.True(Comparer.Equal<T>(obj, obj2));
            }

            // SimpleXML
            {
                var xmlString = new StringBuilder();
                var xmlWriter = new SimpleXmlWriter(XmlWriter.Create(xmlString));

                Serialize.To(xmlWriter, obj);
                xmlWriter.Flush();

                var reader = new SimpleXmlReader(XmlReader.Create(new StringReader(xmlString.ToString())));
                T obj2 = new Deserializer<SimpleXmlReader>(typeof(T)).Deserialize<T>(reader);

                Assert.True(Comparer.Equal<T>(obj, obj2));
            }
        }

        void RoundtripTests()
        {
            var containers = new SimpleContainers();
            for (int i = 0; i < 30; i++)
            {
                containers.strings.Add(i.ToString());
                containers.numbers.Add(i, i.ToString());
            }

            RoundtripObject<SimpleContainers>(containers);

            var blobs = new StructWithBlobs();
            blobs.b = new ArraySegment<byte>(blobData, 0, 30);
            blobs.lb.Add(new ArraySegment<byte>(blobData, 0, 30));
            blobs.lb.Add(new ArraySegment<byte>(blobData, 1, 30));

            RoundtripObject<StructWithBlobs>(blobs);
        }

        [Test]
        public void DeserializerControls_InvalidArguments()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => DeserializerControls.Active.MaxPreallocatedContainerElements = -1);
            Assert.Throws<ArgumentOutOfRangeException>(() => DeserializerControls.Active.MaxPreallocatedBlobBytes = -1);
        }

        [Test]
        public void DeserializerControls_InvalidData()
        {
            DeserializerControls.Active.MaxPreallocatedContainerElements = 20;
            DeserializerControls.Active.MaxPreallocatedBlobBytes = 20;

            // These tests MUST return EndOfStreamException
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<SimpleContainers>());
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<Lists>());
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<Vectors>());
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<Sets>());
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<Maps>());
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<StructWithBlobs>());
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<StructWithBonded>());
            Assert.Throws<EndOfStreamException>(() => DeserializeBadData<StructWithByteLists>());

            DeserializerControls.Active.MaxPreallocatedContainerElements = Int32.MaxValue;
            DeserializerControls.Active.MaxPreallocatedBlobBytes = Int32.MaxValue;

            // These tests may return EndOfStreamException or OutOfMemoryException depending
            // environment -- either is acceptable
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<SimpleContainers>());
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<Lists>());
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<Vectors>());
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<Sets>());
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<Maps>());
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<StructWithBlobs>());
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<StructWithBonded>());
            Assert.Throws(
                Is.InstanceOf<EndOfStreamException>().Or.InstanceOf<OutOfMemoryException>(),
                () => DeserializeBadData<StructWithByteLists>());
        }

        [Test]
        public void DeserializerControls_ValidData()
        {
            DeserializerControls.Active.MaxPreallocatedContainerElements = 80;
            DeserializerControls.Active.MaxPreallocatedBlobBytes = 80;
            RoundtripTests();
            DeserializerControls.Active.MaxPreallocatedContainerElements = 20;
            DeserializerControls.Active.MaxPreallocatedBlobBytes = 20;
            RoundtripTests();
            DeserializerControls.Active.MaxPreallocatedContainerElements = 0;
            DeserializerControls.Active.MaxPreallocatedBlobBytes = 0;
            RoundtripTests();
        }

    }
}
