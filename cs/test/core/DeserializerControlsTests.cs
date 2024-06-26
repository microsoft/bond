namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
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

        // Compact Binary payloads with container types that have invalid elements
        // and very large sizes.
        //
        // In all these payloads:
        // * field id is 4
        // * invalid element type is 0x8d (masked with 0x1f is BT_MAP) or 0x1e (which is alway invalid)
        // * container length is int.MaxValue
        static readonly byte[][] containersInvalidElementTypeLargeSize_CompactBinary = new byte[][]
        {
            // BT_LIST<0x1e>
            new byte[] { 0x8b, 0x1e, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_SET<0x1e>
            new byte[] { 0x8c, 0x1e, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<0x1e, BT_FLOAT>
            new byte[] { 0x8d, 0x1e, 0x07, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<BT_FLOAT, 0x1e>
            new byte[] { 0x8d, 0x07, 0x1e, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_LIST<0x8d>
            new byte[] { 0x8b, 0x8d, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_SET<0x8d>
            new byte[] { 0x8c, 0x8d, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<0x8d, BT_FLOAT>
            new byte[] { 0x8d, 0x8d, 0x07, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<BT_FLOAT, 0x8d>
            new byte[] { 0x8d, 0x07, 0x8d, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
        };

        // Fast Binary payloads with container types that have invalid elements
        // and very large sizes.
        //
        // In all these payloads:
        // * field id is 4
        // * invalid element type is 0x8d (masked with 0x1f is BT_MAP) or 0x1e (which is alway invalid)
        // * container length is int.MaxValue
        static readonly byte[][] containersInvalidElementTypeLargeSize_FastBinary = new byte[][]
        {
            // BT_LIST<0x1e>
            new byte[] { 0x0b, 0x04, 0x00, 0x1e, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_SET<0x1e>
            new byte[] { 0x0c, 0x04, 0x00, 0x1e, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<0x1e, BT_FLOAT>
            new byte[] { 0x0d, 0x04, 0x00, 0x1e, 0x07, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<BT_FLOAT, 0x1e>
            new byte[] { 0x0d, 0x04, 0x00, 0x07, 0x1e, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_LIST<0x8d>
            new byte[] { 0x0b, 0x04, 0x00, 0x8d, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_SET<0x8d>
            new byte[] { 0x0c, 0x04, 0x00, 0x8d, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<0x8d, BT_FLOAT>
            new byte[] { 0x0d, 0x04, 0x00, 0x8d, 0x07, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
            // BT_MAP<BT_FLOAT, 0x8d>
            new byte[] { 0x0d, 0x04, 0x00, 0x07, 0x8d, 0xff, 0xff, 0xff, 0xff, 0x07, 0xff, 0xff, 0xff, 0xff },
        };

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

            const int ExpectedDefaultMaxDepth = 64;

            Assert.AreEqual(ExpectedDefaultMaxDepth, DeserializerControls.Active.MaxDepth);

            Assert.Throws<ArgumentOutOfRangeException>(() => DeserializerControls.Active.MaxDepth = 0);
            Assert.AreEqual(ExpectedDefaultMaxDepth, DeserializerControls.Active.MaxDepth);
            
            Assert.Throws<ArgumentOutOfRangeException>(() => DeserializerControls.Active.MaxDepth = -1);
            Assert.AreEqual(ExpectedDefaultMaxDepth, DeserializerControls.Active.MaxDepth);

            DeserializerControls.Active.MaxDepth = 256;
            Assert.AreEqual(256, DeserializerControls.Active.MaxDepth);

            DeserializerControls.Active.MaxDepth = DeserializerControls.Default.MaxDepth;
            Assert.AreEqual(ExpectedDefaultMaxDepth, DeserializerControls.Active.MaxDepth);
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

            DeserializerControls.Active.MaxDepth = 1;
            Assert.Throws<InvalidDataException>(() => RoundtripTests());

            DeserializerControls.Active.MaxDepth = 2;
            Assert.Throws<InvalidDataException>(() => RoundtripTests());

            DeserializerControls.Active.MaxDepth = 10;
            RoundtripTests();

            DeserializerControls.Active.MaxDepth = 128;
            RoundtripTests();
        }

        [TestCase(100, (ushort)1)]
        [TestCase(100_000, (ushort)1)]
        [TestCase(100, (ushort)2)]
        [TestCase(100_000, (ushort)2)]
        public void BadPayload_RecursionViaNesting_SimpleBinary_Throws(int depth, ushort protocolVersion)
        {
            ArraySegment<byte> payload = NestedPayloadFactory.MakeWithRecursiveChild(ProtocolType.SIMPLE_PROTOCOL, protocolVersion, depth);
            DeserializeBadPayload(input => new SimpleBinaryReader<InputBuffer>(input, protocolVersion), typeof(StructWithRecursiveReference), payload, typeof(InvalidDataException));
        }

        [TestCase(100, (ushort)1)]
        [TestCase(100_000, (ushort)1)]
        [TestCase(100, (ushort)2)]
        [TestCase(100_000, (ushort)2)]
        public void BadPayload_RecursionViaNesting_CompactBinary_Throws(int depth, ushort protocolVersion)
        {
            ArraySegment<byte> payload = NestedPayloadFactory.MakeWithRecursiveChild(ProtocolType.COMPACT_PROTOCOL, protocolVersion, depth);
            DeserializeBadPayload(input => new CompactBinaryReader<InputBuffer>(input, protocolVersion), typeof(StructWithRecursiveReference), payload, typeof(InvalidDataException));
        }

        [TestCase(100, (ushort)1)]
        [TestCase(100_000, (ushort)1)]
        [TestCase(100, (ushort)2)]
        [TestCase(100_000, (ushort)2)]
        public void BadPayload_RecursionViaNesting_FastBinary_Throws(int depth, ushort protocolVersion)
        {
            ArraySegment<byte> payload = NestedPayloadFactory.MakeWithRecursiveChild(ProtocolType.FAST_PROTOCOL, protocolVersion, depth);
            DeserializeBadPayload(input => new FastBinaryReader<InputBuffer>(input, protocolVersion), typeof(StructWithRecursiveReference), payload, typeof(InvalidDataException));
        }

        [TestCaseSource(nameof(DeeplyNestedPayloadTestCases), new object[] { ProtocolType.COMPACT_PROTOCOL })]
        public void BadPayload_RecursionViaSkip_CompactBinary_Throws(int depth, BondDataType fieldType, ushort protocolVersion)
        {
            ArraySegment<byte> payload = NestedPayloadFactory.MakeWithUnknownField(ProtocolType.COMPACT_PROTOCOL, protocolVersion, depth, fieldType);
            DeserializeBadPayload(input => new CompactBinaryReader<InputBuffer>(input, protocolVersion), typeof(ClassWithStructProperties), payload, typeof(InvalidDataException));
        }

        [TestCaseSource(nameof(DeeplyNestedPayloadTestCases), new object[] { ProtocolType.FAST_PROTOCOL })]
        public void BadPayload_RecursionViaSkip_FastBinary_Throws(int depth, BondDataType fieldType, ushort protocolVersion)
        {
            ArraySegment<byte> payload = NestedPayloadFactory.MakeWithUnknownField(ProtocolType.FAST_PROTOCOL, protocolVersion, depth, fieldType);
            DeserializeBadPayload(input => new FastBinaryReader<InputBuffer>(input, protocolVersion), typeof(ClassWithStructProperties), payload, typeof(InvalidDataException));
        }

        [TestCaseSource(nameof(containersInvalidElementTypeLargeSize_CompactBinary))]
        public void BadPayload_InvalidContainerTypeLargeSize_CompactBinary_Throws(byte[] payload)
        {
            // CompactBinary.ReadContainerBegin masks the data type with 0x1f for sequence
            // containers, so some invalid data types map to valid data types. For these,
            // EndOfStreamException is expected, as the skipping code will try to read a
            // payload that does not exist.
            //
            // For maps, ReadContainerBegin does not mask. The skip code will always throw
            // InvalidDataException.
            DeserializeBadPayload(
                input => new CompactBinaryReader<InputBuffer>(input),
                typeof(ClassWithStructProperties),
                payload,
                typeof(EndOfStreamException),
                typeof(InvalidDataException));
        }

        [TestCaseSource(nameof(containersInvalidElementTypeLargeSize_FastBinary))]
        public void BadPayload_InvalidContainerTypeLargeSize_FastBinary_Throws(byte[] payload)
        {
            // Fast Binary fails to skip the invalid container element, hence InvalidDataException.
            DeserializeBadPayload(input => new FastBinaryReader<InputBuffer>(input), typeof(ClassWithStructProperties), payload, typeof(InvalidDataException));
        }

        private void DeserializeBadPayload<TReader>(Func<InputBuffer, TReader> readerFactory, Type deserializedType, byte[] payload, params Type[] expectedExceptionTypes)
        {
            DeserializeBadPayload(readerFactory, deserializedType, new ArraySegment<byte>(payload), expectedExceptionTypes);
        }

        private void DeserializeBadPayload<TReader>(Func<InputBuffer, TReader> readerFactory, Type deserializedType, ArraySegment<byte> payload, params Type[] expectedExceptionTypes)
        {
            var input = new InputBuffer(payload);
            TReader reader = readerFactory(input);
            var deserializer = new Deserializer<TReader>(deserializedType);

            var actualException = Assert.Catch<Exception>(() => deserializer.Deserialize(reader));
            Assert.True(
                expectedExceptionTypes.Any(expectedExceptionType => expectedExceptionType.IsInstanceOfType(actualException)),
                "Expected thrown exception to be {0} but it was {1}",
                    string.Join(" or ", expectedExceptionTypes.Select(eet => eet.Name)),
                    actualException.GetType().Name);
        }

        public static IEnumerable<object[]> DeeplyNestedPayloadTestCases(ProtocolType protocol)
        {
            int[] depths = new[] { 130, 4_000_000 };
            BondDataType[] types = new[] { BondDataType.BT_STRUCT, BondDataType.BT_LIST, BondDataType.BT_SET, BondDataType.BT_MAP };
            ushort[] versions = new ushort[] { 1, 2 };

            var allCombinations =
                from depth in depths
                from type in types
                from version in versions
                select new { depth, type, version };

            if (protocol == ProtocolType.COMPACT_PROTOCOL)
            {
                return
                    from c in allCombinations
                    // CBv2 skips structs by seeking not recursion, so omit that combination from the deeply nested tests
                    where !(c.version == 2 && c.type == BondDataType.BT_STRUCT)
                    select new object[] { c.depth, c.type, c.version };
            }
            else
            {
                return from c in allCombinations select new object[] { c.depth, c.type, c.version };
            }
        }
    }
}
