// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Collections;
    using System.IO;
    using System.Linq;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;
    using UnitTest.Interfaces;

    [TestFixture]
    public class FrameTests
    {
        private const int UnknownFramelet = 0x1234;

        private readonly ArraySegment<byte> AnyContents = new ArraySegment<byte>(new byte[] { 0x62, 0x6F, 0x6E, 0x64 });

        [Test]
        public void FrameletTypes_AreAsSpecified()
        {
            var expectedFramelets = new []
            {
                new { EnumMember = FrameletType.EpoxyConfig, ExpectedValue = 0x4743 },
                new { EnumMember = FrameletType.EpoxyHeaders, ExpectedValue = 0x5248 },
                new { EnumMember = FrameletType.ErrorData, ExpectedValue = 0x4445 },
                new { EnumMember = FrameletType.LayerData, ExpectedValue = 0x594C },
                new { EnumMember = FrameletType.PayloadData, ExpectedValue = 0x4450 },
                new { EnumMember = FrameletType.ProtocolError, ExpectedValue = 0x5245 },
            };

            foreach (var ft in expectedFramelets)
            {
                Assert.AreEqual(
                    ft.ExpectedValue,
                    (int)ft.EnumMember,
                    "FrameletType {0} did not have the speced value",
                    Enum.GetName(typeof(FrameletType), ft.EnumMember));

                Assert.IsTrue(
                    Framelet.IsKnownType((UInt16)ft.EnumMember),
                    "FrameletType {0} is not known to Framelet",
                    ft.EnumMember);
            }

            var expectedEnumMembers = from ft in expectedFramelets select ft.EnumMember;
            var enumMembers = Enum.GetValues(typeof (FrameletType)).Cast<FrameletType>();
            CollectionAssert.AreEquivalent(expectedEnumMembers, enumMembers);
        }

        [Test]
        public void Framelet_ctor_UnknownFrameletType_Throws()
        {
            Assert.IsFalse(Enum.IsDefined(typeof (FrameletType), UnknownFramelet));
            Assert.Throws<ArgumentException>(() => new Framelet((FrameletType)UnknownFramelet, AnyContents));
            Assert.IsFalse(Framelet.IsKnownType((unchecked((UInt16)UnknownFramelet))));
        }

        [Test]
        public void Framelet_ctor_NullEmptyContents_Throws()
        {
            var nullArraySegment = default(ArraySegment<byte>);
            var emptyArraySegment = new ArraySegment<byte>(new byte[0]);

            Assert.Throws<ArgumentException>(() => new Framelet(FrameletType.PayloadData, nullArraySegment));
            Assert.Throws<ArgumentException>(() => new Framelet(FrameletType.PayloadData, emptyArraySegment));
        }

        [Test]
        public void Framelet_Getters_ReturnConstructorValues()
        {
            var framelet = new Framelet(FrameletType.PayloadData, AnyContents);

            Assert.AreEqual(FrameletType.PayloadData, framelet.Type);
            Assert.AreEqual(AnyContents, framelet.Contents);
        }

        [Test]
        public void Frame_ctor_NegativeCapacity_Throws()
        {
            Assert.Throws<ArgumentOutOfRangeException>(() => new Frame(-1, LoggerTests.BlackHole));
        }

        [Test]
        public void Frame_default_ctor_DoesntThrow()
        {
            var frame = new Frame(LoggerTests.BlackHole);

            Assert.AreEqual(0, frame.Count);
            CollectionAssert.IsEmpty(frame.Framelets);
        }

        [Test]
        public void Frame_Add_FrameletsCanBeRetreived()
        {
            var AnyOtherContents = new ArraySegment<byte>(new[] {(byte)0x00});

            var expectedFramelets = new[]
            {
                new Framelet(FrameletType.EpoxyConfig, AnyContents),
                new Framelet(FrameletType.EpoxyConfig, AnyOtherContents),
                new Framelet(FrameletType.LayerData, AnyContents),
                new Framelet(FrameletType.EpoxyConfig, AnyContents),
            };

            var frame = new Frame(4, LoggerTests.BlackHole);
            frame.Add(new Framelet(FrameletType.EpoxyConfig, AnyContents));
            frame.Add(new Framelet(FrameletType.EpoxyConfig, AnyOtherContents));
            frame.Add(new Framelet(FrameletType.LayerData, AnyContents));
            frame.Add(new Framelet(FrameletType.EpoxyConfig, AnyContents));

            Assert.AreEqual(4, frame.Count);
            Assert.AreEqual(frame.Framelets.Count, frame.Count);
            CollectionAssert.AreEqual(expectedFramelets, frame.Framelets);
        }

        [Test]
        public void Frame_Add_TotalCount_Updated()
        {
            var frame = new Frame(2, LoggerTests.BlackHole);

            int expectedSize = 2;
            Assert.AreEqual(expectedSize, frame.TotalSize);

            frame.Add(new Framelet(FrameletType.LayerData, AnyContents));
            expectedSize += 2 + 4 + AnyContents.Count;
            Assert.AreEqual(expectedSize, frame.TotalSize);

            frame.Add(new Framelet(FrameletType.ProtocolError, AnyContents));
            expectedSize += 2 + 4 + AnyContents.Count;
            Assert.AreEqual(expectedSize, frame.TotalSize);
        }

        [Test]
        public void Frame_Add_AddMoreThanUInt16Framelets_Throws()
        {
            var frame = new Frame((int)UInt16.MaxValue + 1, LoggerTests.BlackHole);
            for (int i = 0; i < UInt16.MaxValue; ++i)
            {
                frame.Add(new Framelet(FrameletType.LayerData, AnyContents));
            }

            Assert.That(
                () => frame.Add(new Framelet(FrameletType.PayloadData, AnyContents)),
                Throws.InvalidOperationException.With.Message.ContainsSubstring("Exceeded maximum allowed count of framelets"));
        }

        [Test]
        public void Frame_Add_AddMoreThanInt32TotalSize_Throws()
        {
            var largeContents = new ArraySegment<byte>(new byte[2 * 65535]);
            int numFramesToAdd = Int32.MaxValue/largeContents.Count;

            var frame = new Frame(numFramesToAdd, LoggerTests.BlackHole);
            for (int i = 0; i < numFramesToAdd - 1; ++i)
            {
                frame.Add(new Framelet(FrameletType.LayerData, largeContents));
            }

            Assert.That(
                () => frame.Add(new Framelet(FrameletType.LayerData, largeContents)),
                Throws.InvalidOperationException.With.Message.ContainsSubstring("Exceeded maximum size of frame"));
        }

        [Test]
        public void Frame_WriteAsync_EmptyFrame_Throws()
        {
            var frame = new Frame(LoggerTests.BlackHole);
            Assert.Throws<InvalidOperationException>(async () => await frame.WriteAsync(new MemoryStream()));
        }

        [Test]
        public async Task Frame_WriteAsync_OneFramelet_ContentsExpected()
        {
            var frame = new Frame(LoggerTests.BlackHole);
            frame.Add(new Framelet(FrameletType.EpoxyConfig, AnyContents));

            var memStream = new MemoryStream();
            await frame.WriteAsync(memStream);

            var expectedBytes = new[]
            {
                0x01, 0x00, // frame count
                0x43, 0x47, // EpoxyConfig framelet type
                0x04, 0x00, 0x00, 0x00, // framelet length
                0x62, 0x6F, 0x6E, 0x64 // AnyContents bytes
            };
            CollectionAssert.AreEqual(expectedBytes, memStream.ToArray());
        }

        [Test]
        public void Frame_ReadAsync_ZeroFramelets_Throws()
        {
            var zeroFrameletsStream = new FrameBuilder().Count(0).TakeStream();
            Assert.Throws<EpoxyProtocolErrorException>(async () => await Frame.ReadAsync(zeroFrameletsStream, CancellationToken.None, LoggerTests.BlackHole));
        }

        [Test]
        public void Frame_ReadAsync_UnknownFramelet_Throws()
        {
            var unknownFrameletStream = new FrameBuilder().Count(1).Type((FrameletType) UnknownFramelet).TakeStream();
            Assert.Throws<EpoxyProtocolErrorException>(async () => await Frame.ReadAsync(unknownFrameletStream, CancellationToken.None, LoggerTests.BlackHole));
        }

        [Test]
        public void Frame_ReadAsync_FrameletTooLarge_Throws()
        {
            var frameletTooLargeStream =
                new FrameBuilder().Count(1)
                    .Type(FrameletType.PayloadData)
                    .Size((UInt32) Int32.MaxValue + 1)
                    .TakeStream();
            Assert.Throws<EpoxyProtocolErrorException>(async () => await Frame.ReadAsync(frameletTooLargeStream, CancellationToken.None, LoggerTests.BlackHole));
        }

        [Test]
        public void Frame_ReadAsync_EndOfStreamInCount_ReturnsNull()
        {
            var tooShortStream = new FrameBuilder().Count(1).TakeTooShortStream();
            Assert.Null(Frame.ReadAsync(tooShortStream, CancellationToken.None, LoggerTests.BlackHole).Result);
        }

        [Test]
        public void Frame_ReadAsync_EndOfStreamInType_ReturnsNull()
        {
            var tooShortStream = new FrameBuilder().Count(1).Type(FrameletType.ProtocolError).TakeTooShortStream();
            Assert.Null(Frame.ReadAsync(tooShortStream, CancellationToken.None, LoggerTests.BlackHole).Result);
        }

        [Test]
        public void Frame_ReadAsync_EndOfStreamInSize_ReturnsNull()
        {
            var tooShortStream = new FrameBuilder().Count(1).Type(FrameletType.ProtocolError).Size(4).TakeTooShortStream();
            Assert.Null(Frame.ReadAsync(tooShortStream, CancellationToken.None, LoggerTests.BlackHole).Result);
        }

        [Test]
        public void Frame_ReadAsync_EndOfStreamInContent_ReturnsNull()
        {
            var tooShortStream = new FrameBuilder().Count(1).Type(FrameletType.ProtocolError).Size(4).Content(AnyContents).TakeTooShortStream();
            Assert.Null(Frame.ReadAsync(tooShortStream, CancellationToken.None, LoggerTests.BlackHole).Result);
        }

        [Test]
        public async Task Frame_ReadAsync_CancellationRequested_ReturnsNull()
        {
            var cts = new CancellationTokenSource();
            cts.Cancel();
            var goodStream = new FrameBuilder().Count(1).Type(FrameletType.ProtocolError).Size(4).Content(AnyContents).TakeStream();

            Frame frame = await Frame.ReadAsync(goodStream, cts.Token, LoggerTests.BlackHole);

            Assert.IsNull(frame);
        }

        [Test]
        public async Task Frame_RoundTrip_Works()
        {
            var expectedFramelets = new[]
            {
                new Framelet(FrameletType.EpoxyConfig, AnyContents),
                new Framelet(FrameletType.LayerData, AnyContents),
                new Framelet(FrameletType.EpoxyConfig, AnyContents),
            };

            var frame = new Frame(LoggerTests.BlackHole);
            foreach (var framelet in expectedFramelets)
            {
                frame.Add(framelet);
            }

            var memStream = new MemoryStream();
            await frame.WriteAsync(memStream);

            memStream.Seek(0, SeekOrigin.Begin);
            var resultFrame = await Frame.ReadAsync(memStream, CancellationToken.None, LoggerTests.BlackHole);

            CollectionAssert.AreEqual(expectedFramelets, resultFrame.Framelets, DeepFrameletComparer.Instance);
        }

        private class FrameBuilder
        {
            private MemoryStream stream = new MemoryStream();

            public FrameBuilder Count(UInt16 size)
            {
                byte[] bytes = BitConverter.GetBytes(size);
                stream.Write(bytes, 0, bytes.Length);
                return this;
            }

            public FrameBuilder Type(FrameletType type)
            {
                byte[] bytes = BitConverter.GetBytes(unchecked((UInt16)type));
                stream.Write(bytes, 0, bytes.Length);
                return this;
            }

            public FrameBuilder Size(UInt32 size)
            {
                byte[] bytes = BitConverter.GetBytes(size);
                stream.Write(bytes, 0, bytes.Length);
                return this;
            }

            public FrameBuilder Content(ArraySegment<byte> content)
            {
                stream.Write(content.Array, content.Offset, content.Count);
                return this;
            }

            public FrameBuilder Content(byte[] content)
            {
                return Content(new ArraySegment<byte>(content));
            }

            public MemoryStream TakeStream()
            {
                var result = stream;
                stream = null;

                result.Seek(0, SeekOrigin.Begin);
                return result;
            }

            public MemoryStream TakeTooShortStream()
            {
                var originalStream = stream;
                stream = null;

                return new MemoryStream(
                    originalStream.GetBuffer(),
                    index: 0,
                    count: (int)originalStream.Position - 1,
                    writable: false,
                    publiclyVisible: true);
            }
        }

        private class DeepFrameletComparer : IComparer
        {
            public static readonly DeepFrameletComparer Instance = new DeepFrameletComparer();

            public int Compare(object x, object y)
            {
                Assert.That(x, Is.InstanceOf<Framelet>());
                Assert.That(y, Is.InstanceOf<Framelet>());

                var left = (Framelet)x;
                var right = (Framelet)y;

                int result = left.Type.CompareTo(right.Type);
                if (result != 0)
                {
                    return result;
                }

                result = left.Contents.Count.CompareTo(right.Contents.Count);
                if (result != 0)
                {
                    return result;
                }

                if (left.Contents.Array == null && right.Contents.Array != null)
                {
                    return -1;
                }
                else if (left.Contents.Array != null && right.Contents.Array == null)
                {
                    return 1;
                }

                // at this point, both counts are the same and both arrays have the samm
                // null /not-null status. If the arrays are null, the count will be 0, and we'll
                // skip this loop.
                Assert.AreEqual(left.Contents.Count, right.Contents.Count);
                Assert.IsTrue(
                       (left.Contents.Array == null && right.Contents.Array == null)
                    || (left.Contents.Array != null && right.Contents.Array != null));

                for (int idx = 0; idx < left.Contents.Count; ++idx)
                {
                    byte leftByte = left.Contents.Array[left.Contents.Offset + idx];
                    byte rightByte = right.Contents.Array[right.Contents.Offset + idx];

                    result = leftByte.CompareTo(rightByte);
                    if (result != 0)
                    {
                        return result;
                    }
                }

                return 0;
            }
        }
    }
}
