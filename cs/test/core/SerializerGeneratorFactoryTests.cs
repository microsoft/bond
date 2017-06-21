namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.Linq.Expressions;
    using NUnit.Framework;
    using Bond;
    using Bond.Expressions;
    using Bond.Protocols;
    using Bond.IO.Unsafe;
    using Bond.Internal.Reflection;

    [TestFixture]
    public class SerializerGeneratorFactoryTests
    {
        [Test]
        public void CustomWriterWithStandardSerializer()
        {
            SerializerTest<TestWriter>();
            TranscoderTest<TestWriter>();
            TypedTranscoderTest<TestWriter>();
        }

        [Test]
        public void CustomWriterWithCustomSerializer()
        {
            SerializerTest<WriterWithAttribute>();
            TranscoderTest<WriterWithAttribute>();
            TypedTranscoderTest<WriterWithAttribute>();
        }

        void SerializerTest<W>()
            where W : IWriterTest, new()
        {
            var serializer = new Serializer<W>(typeof(Foo));
            var writer = new W();
            serializer.Serialize(new Foo(), writer);

            Assert.IsTrue(writer.WasCalled);

            if (writer is IWriterTypeTest)
            {
                Assert.IsTrue((writer as IWriterTypeTest).Type == typeof(Foo));
            }
        }

        void TranscoderTest<W>()
            where W : IWriterTest, new()
        {
            var transcoder = new Transcoder<CompactBinaryReader<InputBuffer>, W>();
            var data = new[] { (byte)BondDataType.BT_STOP };
            var reader = new CompactBinaryReader<InputBuffer>(new InputBuffer(data));
            var writer = new W();
            transcoder.Transcode(reader, writer);

            Assert.IsTrue(writer.WasCalled);
        }

        void TypedTranscoderTest<W>()
            where W : IWriterTest, new()
        {
            var transcoder = new Transcoder<CompactBinaryReader<InputBuffer>, W>(typeof(Foo));
            var data = new[] { (byte)BondDataType.BT_STOP };
            var reader = new CompactBinaryReader<InputBuffer>(new InputBuffer(data));
            var writer = new W();
            transcoder.Transcode(reader, writer);

            Assert.IsTrue(writer.WasCalled);

            if (writer is IWriterTypeTest)
            {
                Assert.IsTrue((writer as IWriterTypeTest).Type == typeof(Foo));
            }
        }

        class TestReader
        {
        }

        interface IWriterTest
        {
            bool WasCalled { get; }
        }

        interface IWriterTypeTest
        {
            Type Type { get; }
        }

        [Reader(typeof(TestReader))]
        class TestWriter : IProtocolWriter, IWriterTest
        {
            public bool WasCalled { get; private set; }

            public void WriteVersion()
            {
            }

            public void WriteStructBegin(Metadata metadata)
            {
                WasCalled = true;
            }

            public void WriteBaseBegin(Metadata metadata)
            {
            }

            public void WriteStructEnd()
            {
            }

            public void WriteBaseEnd()
            {
            }

            public void WriteFieldBegin(BondDataType dataType, ushort id, Metadata metadata)
            {
            }

            public void WriteFieldEnd()
            {
            }

            public void WriteFieldOmitted(BondDataType dataType, ushort id, Metadata metadata)
            {
            }

            public void WriteContainerBegin(int count, BondDataType elementType)
            {
            }

            public void WriteContainerBegin(int count, BondDataType keyType, BondDataType valueType)
            {
            }

            public void WriteContainerEnd()
            {
            }

            public void WriteInt8(sbyte value)
            {
            }

            public void WriteInt16(short value)
            {
            }

            public void WriteInt32(int value)
            {
            }

            public void WriteInt64(long value)
            {
            }

            public void WriteUInt8(byte value)
            {
            }

            public void WriteUInt16(ushort value)
            {
            }

            public void WriteUInt32(uint value)
            {
            }

            public void WriteUInt64(ulong value)
            {
            }

            public void WriteFloat(float value)
            {
            }

            public void WriteDouble(double value)
            {
            }

            public void WriteBytes(ArraySegment<byte> bytes)
            {
            }

            public void WriteBool(bool value)
            {
            }

            public void WriteString(string value)
            {
            }

            public void WriteWString(string value)
            {
            }
        }

        [Serializer(typeof(TestSerializer<,>))]
        class WriterWithAttribute : IWriterTest, IWriterTypeTest
        {
            public bool WasCalled { get; set; }
            public Type Type { get; set; }
        }

        class TestSerializer<R, W> : ISerializerGenerator<R, W>
        {
            readonly ParameterExpression writer = Expression.Parameter(typeof(W), "writer");
            readonly Type type;

            public TestSerializer(Expression<Action<R, W, int>> deferred, RuntimeSchema schema)
            {
            }

            public TestSerializer(Expression<Action<R, W, int>> deferred, Type type)
            {
                this.type = type;
            }
 
            public IEnumerable<Expression<Action<R, W>>> Generate(IParser parser)
            {
                var body = Expression.Block(
                    Expression.Assign(
                        Expression.Property(writer, typeof(W).GetDeclaredProperty("WasCalled", typeof(bool))),
                        Expression.Constant(true)),
                    Expression.Assign(
                        Expression.Property(writer, typeof(W).GetDeclaredProperty("Type", typeof(Type))),
                        Expression.Constant(type, typeof(Type))));
                
                return new[] { Expression.Lambda<Action<R, W>>(body, parser.ReaderParam, writer) };
            }
        }

        [Schema]
        class Foo
        {
        }
    }
}
