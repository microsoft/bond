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
    using NUnit.Framework.Constraints;

    [TestFixture]
    public class DeserializerControlsTests
    {
        // Data that is designed to preallocate an extremely large container/blob
        // (0x7F7F7F7F elements/bytes) when used with SimpleBinary and a struct that
        // starts with a container/blob
        static readonly byte[] badData = new byte[] { 0x7F, 0x7F, 0x7F, 0x7F, 0x7f };

        // Data for a simple valid blob
        static readonly byte[] blobData = new byte[] { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 };

        // Restore the default settings at the start and end of each test
        [SetUp]
        [TearDown]
        public void RestoreDefaults()
        {
            DeserializerControls.Active = DeserializerControls.Default;
            InputStream.ActiveAllocationChunk = InputStream.DefaultAllocationChunk;
        }

        delegate T CreationDelegate<T>(byte[] data);

        InputBuffer InputBufferCreator(byte[] data)
        {
            return new InputBuffer(data);
        }

        InputStream InputStreamCreator(byte[] data)
        {
            return new InputStream(new MemoryStream(data));
        }

        static T DeserializeBadDataHelper<T, U>(CreationDelegate<U> creator) where U : Bond.IO.IInputStream, Bond.IO.ICloneable<U>
        {
            // Untagged protocol used to trigger errors more easily
            var reader = new SimpleBinaryReader<U>(creator(badData));
            return new Deserializer<SimpleBinaryReader<U>>(typeof(T)).Deserialize<T>(reader);
        }

        delegate Constraint ConstraintDelegate();

        Constraint ExceptionDelegate<T>()
        {
            return Is.InstanceOf<T>();
        }

        Constraint ExceptionDelegate<T, U>()
        {
            return Is.InstanceOf<T>().Or.InstanceOf<U>();
        }

        void DeserializeBadDataTestImpl(ConstraintDelegate constraint)
        {
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<SimpleContainers, InputBuffer>(InputBufferCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Lists, InputBuffer>(InputBufferCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Vectors, InputBuffer>(InputBufferCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Sets, InputBuffer>(InputBufferCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Maps, InputBuffer>(InputBufferCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<StructWithBlobs, InputBuffer>(InputBufferCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<StructWithBonded, InputBuffer>(InputBufferCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<StructWithByteLists, InputBuffer>(InputBufferCreator));

            Assert.Throws(constraint(), () => DeserializeBadDataHelper<SimpleContainers, InputStream>(InputStreamCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Lists, InputStream>(InputStreamCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Vectors, InputStream>(InputStreamCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Sets, InputStream>(InputStreamCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<Maps, InputStream>(InputStreamCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<StructWithBlobs, InputStream>(InputStreamCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<StructWithBonded, InputStream>(InputStreamCreator));
            Assert.Throws(constraint(), () => DeserializeBadDataHelper<StructWithByteLists, InputStream>(InputStreamCreator));
        }

        void RoundtripObjectBuffer<T>(T obj)
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

                var reader = new SimpleXmlReader(new StringReader(xmlString.ToString()));
                T obj2 = new Deserializer<SimpleXmlReader>(typeof(T)).Deserialize<T>(reader);

                Assert.True(Comparer.Equal<T>(obj, obj2));
            }
        }

        void RoundtripObjectStream<T>(T obj)
        {
            // SimpleBinary
            {
                var output = new OutputBuffer();
                var writer = new SimpleBinaryWriter<OutputBuffer>(output);
                Serialize.To(writer, obj);

                var input =
                    new InputStream(
                        new MemoryStream(
                            output.Data.Array,
                            output.Data.Offset,
                            output.Data.Count,
                            writable: false,
                            publiclyVisible: true));
                var reader = new SimpleBinaryReader<InputStream>(input);
                T obj2 = Deserialize<T>.From(reader);

                Assert.True(Comparer.Equal<T>(obj, obj2));
            }

            // CompactBinary
            {
                var output = new OutputBuffer();
                var writer = new CompactBinaryWriter<OutputBuffer>(output);
                Serialize.To(writer, obj);

                var input =
                    new InputStream(
                        new MemoryStream(
                            output.Data.Array,
                            output.Data.Offset,
                            output.Data.Count,
                            writable: false,
                            publiclyVisible: true));
                var reader = new CompactBinaryReader<InputStream>(input);
                T obj2 = new Deserializer<CompactBinaryReader<InputStream>>(typeof(T)).Deserialize<T>(reader);

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

            RoundtripObjectBuffer<SimpleContainers>(containers);
            RoundtripObjectStream<SimpleContainers>(containers);

            var blobs = new StructWithBlobs();
            blobs.b = new ArraySegment<byte>(blobData, 0, 30);
            blobs.lb.Add(new ArraySegment<byte>(blobData, 0, 30));
            blobs.lb.Add(new ArraySegment<byte>(blobData, 1, 30));

            RoundtripObjectBuffer<StructWithBlobs>(blobs);
            RoundtripObjectStream<StructWithBlobs>(blobs);
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
            // This first pass MUST return EndOfStreamException
            DeserializerControls.Active.MaxPreallocatedContainerElements = 20;
            DeserializerControls.Active.MaxPreallocatedBlobBytes = 20;
            InputStream.ActiveAllocationChunk = 20;

            DeserializeBadDataTestImpl(ExceptionDelegate<EndOfStreamException>);

            // This second pass may return EndOfStreamException or OutOfMemoryException depending
            // environment -- either is acceptable
            DeserializerControls.Active.MaxPreallocatedContainerElements = Int32.MaxValue;
            DeserializerControls.Active.MaxPreallocatedBlobBytes = Int32.MaxValue;
            InputStream.ActiveAllocationChunk = Int32.MaxValue;

            DeserializeBadDataTestImpl(ExceptionDelegate<EndOfStreamException, OutOfMemoryException>);
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
