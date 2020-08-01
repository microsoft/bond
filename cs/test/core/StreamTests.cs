namespace UnitTest
{
    using System;
    using System.Diagnostics;
    using System.IO;
    using System.Runtime.InteropServices;
    using NUnit.Framework;
    using Bond;
    using Bond.IO;
    using Bond.Protocols;
    using Bond.IO.Unsafe;

    /// <summary>
    /// Common test cases for IInputStream implementations. There's a
    /// concrete implementation for each IInputStream implementation we want
    /// to test.
    /// </summary>
    [TestFixture]
    public abstract class IInputStreamTestsBase<TInputStream>
        where TInputStream : IInputStream, ICloneable<TInputStream>
    {
        protected TInputStream MakeInputStream(params byte[] buffer)
        {
            return MakeInputStream(new ArraySegment<byte>(buffer));
        }

        /// <summary>
        /// Creates an <typeparamref name="TInputStream" /> over the given bytes.
        /// </summary>
        protected abstract TInputStream MakeInputStream(ArraySegment<byte> buffer);

        // Not all implementations use an internal buffer, but for those that
        // do, it needs to be this large.
        protected const int InternalBufferSize = 9;

        [Test]
        public void Skip_AdvancesPosition()
        {
            var inputStream = MakeInputStream(0, 1, 2);

            inputStream.SkipBytes(2);
            Assert.AreEqual(2, inputStream.Position);

            Assert.AreEqual(2, inputStream.ReadUInt8());
        }

        [Test]
        public void SkipBeyondInternalBufferSize_SeeksCorrectly()
        {
            var buf = MakeSequential(InternalBufferSize*3 + 1);

            var inputStream = MakeInputStream(buf);
            Assert.AreEqual(0, inputStream.ReadUInt8());
            inputStream.SkipBytes(InternalBufferSize);
            var blob = inputStream.ReadBytes(InternalBufferSize*2);
            for (int j = 0; j < blob.Count; ++j)
            {
                byte expectedValue = (byte) ((j + InternalBufferSize + 1)%256);
                Assert.AreEqual(expectedValue, blob.Array[blob.Offset + j]);
            }
        }

        [Test]
        public void SkipBeyondEndOfStream_ThrowsOnNextRead()
        {
            var buf = MakeSequential(InternalBufferSize*3);
            var inputStream = MakeInputStream(buf);
            inputStream.SkipBytes(InternalBufferSize*3 + 1);
            Assert.Throws<EndOfStreamException>(() => inputStream.ReadUInt8());
        }

        [Test]
        public void Stream_PositionLength_AsExpected()
        {
            const int _50MB = 50 * 1024 * 1024;

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

            var input = MakeInputStream(stream.ToArray());
            var reader = new CompactBinaryReader<TInputStream>(input);

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
        public void Stream_PositionLength_NotAccessedOnWriteStructBegin()
        {
            Stream_PositionLength_NotAccessedOnWriteStructBeginImplementation();
        }

        [Test]
        public void ReadBytes_DifferentSizesAndPositions_ReadCorrectly()
        {
            var buffer = MakeSequential(5 * 1024);

            for (var k = 3; k < 20; ++k)
            {
                var input = MakeInputStream(buffer);

                while (input.Position + k + sizeof(long) < input.Length)
                {
                    var x = input.Position;
                    var bytes = input.ReadBytes(k);
                    input.ReadUInt64();
                    for (var j = 0; j < bytes.Count; ++j)
                    {
                        Assert.AreEqual((x + j)%256, bytes.Array[bytes.Offset + j]);
                    }
                }
            }
        }

        [Test]
        public void VarInts_RoundTrip()
        {
            Action<ushort> test16 = value =>
            {
                var output = new OutputBuffer();
                output.WriteVarUInt16(value);
                var input = MakeInputStream(output.Data);
                Assert.AreEqual(value, input.ReadVarUInt16());
            };

            Action<uint> test32 = value =>
            {
                var output = new OutputBuffer();
                output.WriteVarUInt32(value);
                var input = MakeInputStream(output.Data);
                Assert.AreEqual(value, input.ReadVarUInt32());
            };

            Action<ulong> test64 = value =>
            {
                var output = new OutputBuffer();
                output.WriteVarUInt64(value);
                var input = MakeInputStream(output.Data);
                Assert.AreEqual(value, input.ReadVarUInt64());
            };

            test16(ushort.MinValue);
            test16(ushort.MaxValue);
            test32(uint.MinValue);
            test32(uint.MaxValue);
            test64(ulong.MinValue);
            test64(ulong.MaxValue);
        }

        protected static ArraySegment<byte> EmbedBuffer(ArraySegment<byte> buffer)
        {
            if (buffer.Offset != 0 && buffer.Count != buffer.Array.Length)
            {
                // it's already embedded in something else
                return buffer;
            }

            var largerBuffer = new byte[buffer.Count + 5];
            largerBuffer[0] = 255;
            largerBuffer[1] = 255;
            largerBuffer[2] = 255;
            Buffer.BlockCopy(
                src: buffer.Array,
                srcOffset: buffer.Offset,
                dst: largerBuffer,
                dstOffset: 3,
                count: buffer.Count);
            largerBuffer[largerBuffer.Length - 2] = 128;
            largerBuffer[largerBuffer.Length - 1] = 128;

            return new ArraySegment<byte>(largerBuffer, offset: 3, count: buffer.Count);
        }

        protected static byte[] MakeSequential(int length)
        {
            Assert.That(length >= 0);

            var buf = new byte[length];
            for (int i = 0; i < length; ++i)
            {
                buf[i] = (byte) (i%256);
            }

            return buf;
        }

        // WriteStructBegin should only access Stream.Position in the DEBUG
        // configuration, so we only execute this test in the RELEASE
        // configuration.
        [Conditional("RELEASE")]
        private void Stream_PositionLength_NotAccessedOnWriteStructBeginImplementation()
        {
            var stream = new NonSeekableStream();
            var output = new OutputStream(stream, bufferLength:11);
            var writer = new CompactBinaryWriter<OutputStream>(output, version:2);
            var firstPass = writer.GetFirstPassWriter();
            firstPass.WriteStructBegin(new Metadata());
            firstPass.WriteStructEnd();
            writer.WriteStructBegin(new Metadata());
        }

        private class NonSeekableStream : Stream
        {
            public override void Flush()
            {
            }

            public override long Seek(long offset, SeekOrigin origin)
            {
                throw new NotSupportedException();
            }

            public override void SetLength(long value)
            {
                throw new NotSupportedException();
            }

            public override int Read(byte[] buffer, int offset, int count)
            {
                return 0;
            }

            public override void Write(byte[] buffer, int offset, int count)
            {
            }

            public override bool CanRead { get; }
            public override bool CanSeek => false;
            public override bool CanWrite { get; }
            public override long Length => throw new NotSupportedException();
            public override long Position
            {
                get => throw new NotSupportedException();
                set => throw new NotSupportedException();
            }
        }
    }

    public class UnsafeInputBufferTests : IInputStreamTestsBase<Bond.IO.Unsafe.InputBuffer>
    {
        protected override Bond.IO.Unsafe.InputBuffer MakeInputStream(ArraySegment<byte> buffer)
        {
            return new Bond.IO.Unsafe.InputBuffer(buffer);
        }
    }

    public class UnsafeInputBufferOffsetTests : IInputStreamTestsBase<Bond.IO.Unsafe.InputBuffer>
    {
        protected override Bond.IO.Unsafe.InputBuffer MakeInputStream(ArraySegment<byte> buffer)
        {
            return new Bond.IO.Unsafe.InputBuffer(EmbedBuffer(buffer));
        }
    }

    public class SafeInputBufferTests : IInputStreamTestsBase<Bond.IO.Safe.InputBuffer>
    {
        protected override Bond.IO.Safe.InputBuffer MakeInputStream(ArraySegment<byte> buffer)
        {
            return new Bond.IO.Safe.InputBuffer(buffer);
        }
    }

    public class SafeInputBufferOffsetTests : IInputStreamTestsBase<Bond.IO.Safe.InputBuffer>
    {
        protected override Bond.IO.Safe.InputBuffer MakeInputStream(ArraySegment<byte> buffer)
        {
            return new Bond.IO.Safe.InputBuffer(EmbedBuffer(buffer));
        }
    }

    public class InputPointerTests : IInputStreamTestsBase<InputPointer>
    {
        GCHandle? pinnedBuffer;

        [TearDown]
        public void TearDown()
        {
            pinnedBuffer?.Free();
            pinnedBuffer = null;
        }

        protected override InputPointer MakeInputStream(ArraySegment<byte> buffer)
        {
            // there may already be a pinnedBuffer, so be sure to free it
            pinnedBuffer?.Free();

            pinnedBuffer = GCHandle.Alloc(buffer.Array, GCHandleType.Pinned);
            return new InputPointer(
                pinnedBuffer.Value.AddrOfPinnedObject() + buffer.Offset,
                buffer.Count);
        }
    }

    public abstract class InputStreamTestsBase : IInputStreamTestsBase<InputStream>
    {
        // Restore the default settings at the start and end of each test
        [SetUp]
        [TearDown]
        public void RestoreDefaults()
        {
            InputStream.ActiveAllocationChunk = InputStream.DefaultAllocationChunk;
        }

        [Test]
        public void SkipBeyondInternalBufferSize_AllocationChunk_SeeksCorrectly()
        {
            InputStream.ActiveAllocationChunk = 8;
            SkipBeyondInternalBufferSize_SeeksCorrectly();
            InputStream.ActiveAllocationChunk = 2;
            SkipBeyondInternalBufferSize_SeeksCorrectly();
        }

        [Test]
        public void SkipBeyondEndOfStream_AllocationChunk_ThrowsOnNextRead()
        {
            InputStream.ActiveAllocationChunk = 8;
            SkipBeyondEndOfStream_ThrowsOnNextRead();
            InputStream.ActiveAllocationChunk = 2;
            SkipBeyondEndOfStream_ThrowsOnNextRead();
        }

        [Test]
        public void Stream_PositionLength_WithAllocationChunk_AsExpected()
        {
            InputStream.ActiveAllocationChunk = 8;
            Stream_PositionLength_AsExpected();
            InputStream.ActiveAllocationChunk = 2;
            Stream_PositionLength_AsExpected();
        }

        [Test]
        public void ReadBytes_DifferentSizesAndOffsets_WithAllocationChunk_ReadCorrectly()
        {
            InputStream.ActiveAllocationChunk = 8;
            ReadBytes_DifferentSizesAndPositions_ReadCorrectly();
            InputStream.ActiveAllocationChunk = 2;
            ReadBytes_DifferentSizesAndPositions_ReadCorrectly();
        }

        [Test]
        public void VarInts_With_AllocationChunk_RoundTrip()
        {
            InputStream.ActiveAllocationChunk = 8;
            VarInts_RoundTrip();
            InputStream.ActiveAllocationChunk = 2;
            VarInts_RoundTrip();
        }


        [Test]
        public void BadActiveAllocationChunkValues_Throws()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => InputStream.ActiveAllocationChunk = 0);
            Assert.Throws<ArgumentOutOfRangeException>(() => InputStream.ActiveAllocationChunk = -1);
        }
    }

    public class InputStreamTests : InputStreamTestsBase
    {
        protected override InputStream MakeInputStream(ArraySegment<byte> buffer)
        {
            var ms = new MemoryStream(
                buffer.Array,
                buffer.Offset,
                buffer.Count,
                writable: false,
                publiclyVisible: true);
            return new InputStream(ms, InternalBufferSize);
        }
    }

    public class NonSeekableInputStreamTests : InputStreamTestsBase
    {
        protected override InputStream MakeInputStream(ArraySegment<byte> buffer)
        {
            var ms = new NonSeekableMemoryStream(buffer);
            return new InputStream(ms, InternalBufferSize);
        }

        private class NonSeekableMemoryStream : MemoryStream
        {
            public NonSeekableMemoryStream(ArraySegment<byte> buffer)
                : base(buffer.Array,
                    buffer.Offset,
                    buffer.Count,
                    writable: false,
                    publiclyVisible: true)
            {
            }

            public override bool CanSeek => false;

            public override long Seek(long offset, SeekOrigin loc)
            {
                throw new NotImplementedException(
                    $"{nameof(NonSeekableMemoryStream)} cannot Seek.");
            }
        }
    }
}
