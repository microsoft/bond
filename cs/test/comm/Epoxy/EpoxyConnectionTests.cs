// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using Bond;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;

    [TestFixture]
    public class EpoxyConnectionTests
    {
        private const ProtocolErrorCode MeaninglessErrorCode = ProtocolErrorCode.GENERIC_ERROR;

        private static readonly Error AnyDetails = new Error
        {
            error_code = (int) ErrorCode.MethodNotFound,
            message = "This is some error message"
        };

        [Test]
        public void MakeProtocolErrorFrame_JustErrorCode_MakesAFrame()
        {
            var frame = EpoxyConnection.MakeProtocolErrorFrame(MeaninglessErrorCode, null);
            Assert.NotNull(frame);
            Assert.AreEqual(1, frame.Framelets.Count);
            Assert.AreEqual(FrameletType.ProtocolError, frame.Framelets[0].Type);

            var inputBuffer = new InputBuffer(frame.Framelets[0].Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            var error = Deserialize<ProtocolError>.From(fastBinaryReader);
            Assert.AreEqual(MeaninglessErrorCode, error.error_code);
            Assert.Null(error.details);
        }

        [Test]
        public void MakeProtocolErrorFrame_WithDetails_MakesAFrame()
        {
            var frame = EpoxyConnection.MakeProtocolErrorFrame(MeaninglessErrorCode, AnyDetails);
            Assert.NotNull(frame);
            Assert.AreEqual(1, frame.Framelets.Count);
            Assert.AreEqual(FrameletType.ProtocolError, frame.Framelets[0].Type);

            var inputBuffer = new Bond.IO.Unsafe.InputBuffer(frame.Framelets[0].Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            var error = Deserialize<ProtocolError>.From(fastBinaryReader);
            Assert.AreEqual(MeaninglessErrorCode, error.error_code);
            Assert.NotNull(error.details);
            var details = error.details.Deserialize();
            Assert.IsTrue(AnyDetails.IsEqual<Error, Error>(details));
        }
    }
}
