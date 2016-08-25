// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System;
using System.Linq;

namespace UnitTest.Epoxy
{
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;
    using UnitTest.Comm;
    using UnitTest.Interfaces;

    [TestFixture]
    class EpoxyProtocolTests
    {
        private const uint GoodRequestId = 1;
        private const uint GoodResponseId = 1;
        private const string GoodMethod = "ShaveYaks";
        private const ProtocolErrorCode MeaninglessErrorCode = ProtocolErrorCode.GENERIC_ERROR;
        private static readonly IMessage<Dummy> meaninglessMessage = new Message<Dummy>(new Dummy());
        private static readonly ArraySegment<byte> emptyLayerData = new ArraySegment<byte>();
        private static ArraySegment<byte> goodLayerData;

        private static readonly EpoxyHeaders goodRequestHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Request,
            conversation_id = GoodRequestId
        };
        private static readonly EpoxyHeaders goodResponseHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Response,
            conversation_id = GoodResponseId
        };
        private static readonly EpoxyHeaders goodEventHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Event,
            conversation_id = GoodRequestId
        };
        private static readonly EpoxyHeaders unknownTypeHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = (PayloadType)(-100),
            conversation_id = GoodRequestId
        };
        private static readonly Dummy dummyObject = new Dummy
        {
            int_value = 0x1234
        };

        private static Frame goodRequestFrame;
        private static Frame goodRequestLayerDataFrame;
        private static Frame goodResponseFrame;
        private static Frame goodEventFrame;
        private static Frame shortRequestFrame;         // a request frame with EpoxyHeaders but no PayloadData
        private static Frame doubleHeadersRequestFrame; // a request frame with duplicate EpoxyHeaders
        private static Frame headersConfigRequestFrame; // a request frame with EpoxyHeaders, then an EpoxyConfig
        private static Frame doublePayloadRequestFrame; // a request frame with duplicate PayloadData
        private static Frame backwardsRequestFrame;     // a request frame with PayloadData before EpoxyHeaders
        private static Frame configFrame;               // a frame with a well-formed EpoxyConfig
        private static Frame configFrameExtra;          // a frame with a well-formed EpoxyConfig and extra stuff
        private static Frame configFrameBadConfigData;  // a frame that fits that shape of a config frame, but with a payload that can't be deserialized
        private static Frame protocolErrorFrame;        // a frame with a well-formed ProtocolError
        private static Frame doubleProtocolErrorFrame;  // a frame with two ProtocolError frames
        private static readonly Frame emptyFrame = new Frame(0, LoggerTests.BlackHole);

        [TestFixtureSetUp]
        public static void CreateFrames()
        {
            // Set up the non-empty layer data we'll use.
            Bond.IBonded goodLayerObject = new Bond.Bonded<Dummy>(dummyObject);
            var outputBuffer = new OutputBuffer();
            var compactWriter = new CompactBinaryWriter<OutputBuffer>(outputBuffer);
            compactWriter.WriteVersion();
            goodLayerObject.Serialize(compactWriter);
            goodLayerData = outputBuffer.Data;

            // Good frames, from which we can pull good framelets to build bad frames.
            goodRequestFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Request, meaninglessMessage, null, LoggerTests.BlackHole);
            goodRequestLayerDataFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Request, meaninglessMessage, goodLayerObject, LoggerTests.BlackHole);

            goodResponseFrame = EpoxyConnection.MessageToFrame(
                GoodResponseId, GoodMethod, PayloadType.Response, meaninglessMessage, null, LoggerTests.BlackHole);
            goodEventFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Event, meaninglessMessage, null, LoggerTests.BlackHole);

            configFrame = EpoxyConnection.MakeConfigFrame(LoggerTests.BlackHole);
            protocolErrorFrame = EpoxyConnection.MakeProtocolErrorFrame(MeaninglessErrorCode, null, LoggerTests.BlackHole);

            var goodFrameletCount = goodRequestFrame.Count;

            // Bad frames made of good framelets.
            shortRequestFrame = new Frame(goodFrameletCount - 1, LoggerTests.BlackHole);
            for (var i = 0; i < goodFrameletCount - 1; i++)
            {
                shortRequestFrame.Add(goodRequestFrame.Framelets[i]);
            }

            doubleHeadersRequestFrame = new Frame(goodFrameletCount + 1, LoggerTests.BlackHole);
            doubleHeadersRequestFrame.Add(goodRequestFrame.Framelets[0]);
            for (var i = 0; i < goodFrameletCount; i++)
            {
                doubleHeadersRequestFrame.Add(goodRequestFrame.Framelets[i]);
            }

            headersConfigRequestFrame = new Frame(2, LoggerTests.BlackHole);
            headersConfigRequestFrame.Add(goodRequestFrame.Framelets[0]);
            headersConfigRequestFrame.Add(configFrame.Framelets[0]);

            doublePayloadRequestFrame = new Frame(goodFrameletCount + 1, LoggerTests.BlackHole);
            for (var i = 0; i < goodFrameletCount; i++)
            {
                doublePayloadRequestFrame.Add(goodRequestFrame.Framelets[i]);
            }
            doublePayloadRequestFrame.Add(goodRequestFrame.Framelets[goodFrameletCount - 1]);

            backwardsRequestFrame = new Frame(goodFrameletCount, LoggerTests.BlackHole);
            foreach (var framelet in goodRequestFrame.Framelets.Reverse())
            {
                backwardsRequestFrame.Add(framelet);
            }

            doubleProtocolErrorFrame = EpoxyConnection.MakeProtocolErrorFrame(MeaninglessErrorCode, null, LoggerTests.BlackHole);
            doubleProtocolErrorFrame.Add(doubleProtocolErrorFrame.Framelets[0]);

            configFrameExtra = EpoxyConnection.MakeConfigFrame(LoggerTests.BlackHole);
            configFrameExtra.Add(goodRequestFrame.Framelets[0]);

            // Bad frames made of bad framelets.
            var invalidConfigData = new ArraySegment<byte>(new byte[] { 0x01 });
            configFrameBadConfigData = new Frame(1, LoggerTests.BlackHole);
            configFrameBadConfigData.Add(new Framelet(FrameletType.EpoxyConfig, invalidConfigData));
        }

        // For each state that is implemented as a function, test:
        //  * that it works in the happy path
        //  * that it transitions to an internal error if it's coming from any unexpected state
        //  * that it transitions to an internal error if any state left by previous transitions is unacceptable
        //  * that it transitions to an expected error state if the frame is malformed

        [Test]
        public void TransitionExpectFirstFramelet_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, goodRequestFrame, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, after);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, protocolErrorFrame, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectProtocolError, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectFirstFramelet_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, emptyFrame, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            errorCode = null;

            after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, backwardsRequestFrame, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectEpoxyHeaders_Valid()
        {
            EpoxyHeaders headers = null;
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, goodRequestFrame, ref headers, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, after);
            Assert.NotNull(headers);
            Assert.AreEqual(GoodRequestId, headers.conversation_id);
            Assert.AreEqual(0, headers.error_code);
            Assert.AreEqual(GoodMethod, headers.method_name);
            Assert.AreEqual(PayloadType.Request, headers.payload_type);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, goodRequestLayerDataFrame,
                ref headers, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, after);
            Assert.NotNull(headers);
            Assert.AreEqual(GoodRequestId, headers.conversation_id);
            Assert.AreEqual(0, headers.error_code);
            Assert.AreEqual(GoodMethod, headers.method_name);
            Assert.AreEqual(PayloadType.Request, headers.payload_type);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEpoxyHeaders_InvalidPreconditions()
        {
            EpoxyHeaders headers = null;
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, emptyFrame, ref headers, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, backwardsRequestFrame, ref headers, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectOptionalLayerData_Valid()
        {
            var layerData = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectOptionalLayerData(
                EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, goodRequestFrame, goodRequestHeaders,
                ref layerData, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectPayload, after);
            Assert.Null(layerData.Array);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectOptionalLayerData(
                EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, goodRequestLayerDataFrame, goodRequestHeaders,
                ref layerData, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectPayload, after);
            CollectionAssert.AreEqual(goodLayerData, layerData);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectOptionalLayerData_InvalidPreconditions()
        {
            var layerData = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectOptionalLayerData(
                EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, goodRequestFrame, null,
                ref layerData, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(layerData.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectOptionalLayerData_MalformedFrame()
        {
            var layerData = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectOptionalLayerData(
                EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, shortRequestFrame, goodRequestHeaders,
                ref layerData, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(layerData.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectPayload_Valid()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectEndOfFrame, after);
            Assert.NotNull(payload.Array);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestLayerDataFrame, goodRequestHeaders, goodLayerData,
                ref payload, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectEndOfFrame, after);
            Assert.NotNull(payload.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectPayload_InvalidPreconditions()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestFrame, null, emptyLayerData,
                ref payload, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(payload.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectPayload_MalformedFrame()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, emptyFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, shortRequestFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestLayerDataFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestFrame, goodRequestHeaders, goodLayerData,
                ref payload, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, goodRequestFrame, emptyLayerData, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.FrameComplete, after);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, goodRequestLayerDataFrame, goodLayerData, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.FrameComplete, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, doublePayloadRequestFrame, emptyLayerData, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionFrameComplete_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, goodRequestHeaders, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ValidFrame, after);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, goodResponseHeaders, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ValidFrame, after);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, goodEventHeaders, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ValidFrame, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionFrameComplete_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, unknownTypeHeaders, ref errorCode, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.NOT_SUPPORTED, errorCode);
        }

        [Test]
        public void TransitionValidFrame_Valid()
        {
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionValidFrame(
                EpoxyProtocol.ClassifyState.ValidFrame, goodRequestHeaders, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverRequestToService, disposition);

            disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            after = EpoxyProtocol.TransitionValidFrame(
                EpoxyProtocol.ClassifyState.ValidFrame, goodResponseHeaders, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverResponseToProxy, disposition);

            disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            after = EpoxyProtocol.TransitionValidFrame(
                EpoxyProtocol.ClassifyState.ValidFrame, goodEventHeaders, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverEventToService, disposition);
        }

        [Test]
        public void TransitionValidFrame_InvalidPreconditions()
        {
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionValidFrame(EpoxyProtocol.ClassifyState.ValidFrame, null, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
        }

        [Test]
        public void TransitionExpectConfig_Valid()
        {
            ProtocolErrorCode? errorCode = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectConfig(
                EpoxyProtocol.ClassifyState.ExpectConfig, configFrame, ref errorCode, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.ProcessConfig, disposition);
            Assert.IsNull(errorCode);
        }

        [Test]
        public void TransitionExpectConfig_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectConfig(
                EpoxyProtocol.ClassifyState.ExpectConfig, configFrameExtra, ref errorCode, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
        }

        [Test]
        public void TransitionExpectConfig_MalformedConfigData()
        {
            ProtocolErrorCode? errorCode = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectConfig(
                EpoxyProtocol.ClassifyState.ExpectConfig, configFrameBadConfigData, ref errorCode, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
        }

        [Test]
        public void TransitionExpectConfig_InvalidPreconditions()
        {
            ProtocolErrorCode? errorCode = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectConfig(
                EpoxyProtocol.ClassifyState.ExpectConfig, emptyFrame, ref errorCode, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
            Assert.IsNull(errorCode);

            // Non-empty, non-config frame.
            after = EpoxyProtocol.TransitionExpectConfig(
                EpoxyProtocol.ClassifyState.ExpectConfig, protocolErrorFrame, ref errorCode, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
            Assert.IsNull(errorCode);
        }

        [Test]
        public void TransitionExpectProtocolError_Valid()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, protocolErrorFrame, ref error, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(MeaninglessErrorCode, error.error_code);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HandleProtocolError, disposition);
        }

        [Test]
        public void TransitionExpectProtocolError_ErrorInError()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, doubleProtocolErrorFrame, ref error, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ErrorInErrorFrame, after);
            Assert.Null(error);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
        }

        [Test]
        public void TransitionExpectProtocolError_InvalidPreconditions()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, goodRequestFrame, ref error, ref disposition, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(error);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
        }

        // These end-to-end tests cover states that don't fit in functions.

        private static void AssertHeadersEqual(EpoxyHeaders expected, EpoxyHeaders actual)
        {
            Assert.AreEqual(expected.error_code, actual.error_code);
            Assert.AreEqual(expected.method_name, actual.method_name);
            Assert.AreEqual(expected.payload_type, actual.payload_type);
            Assert.AreEqual(expected.conversation_id, actual.conversation_id);
        }

        [Test]
        public void Classify_Valid()
        {
            var requestResult = EpoxyProtocol.Classify(goodRequestFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverRequestToService, requestResult.Disposition);
            Assert.Null(requestResult.LayerData.Array);
            AssertHeadersEqual(goodRequestHeaders, requestResult.Headers);
            Assert.AreEqual(goodRequestFrame.Framelets[goodRequestFrame.Count - 1].Contents, requestResult.Payload);
            Assert.Null(requestResult.Error);
            Assert.Null(requestResult.ErrorCode);

            var requestLayerResult = EpoxyProtocol.Classify(goodRequestLayerDataFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverRequestToService, requestLayerResult.Disposition);
            AssertHeadersEqual(goodRequestHeaders, requestLayerResult.Headers);
            CollectionAssert.AreEqual(goodLayerData, requestLayerResult.LayerData);
            Assert.AreEqual(goodRequestLayerDataFrame.Framelets[goodRequestLayerDataFrame.Count - 1].Contents, requestLayerResult.Payload);
            Assert.Null(requestLayerResult.Error);
            Assert.Null(requestLayerResult.ErrorCode);

            var responseResult = EpoxyProtocol.Classify(goodResponseFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverResponseToProxy, responseResult.Disposition);
            AssertHeadersEqual(goodResponseHeaders, responseResult.Headers);
            Assert.Null(requestResult.LayerData.Array);
            Assert.AreEqual(goodResponseFrame.Framelets[goodResponseFrame.Count - 1].Contents, responseResult.Payload);
            Assert.Null(requestResult.Error);
            Assert.Null(requestResult.ErrorCode);

            var eventResult = EpoxyProtocol.Classify(goodEventFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverEventToService, eventResult.Disposition);
            Assert.Null(eventResult.LayerData.Array);
            AssertHeadersEqual(goodEventHeaders, eventResult.Headers);
            Assert.AreEqual(goodEventFrame.Framelets[goodEventFrame.Count - 1].Contents, eventResult.Payload);
            Assert.Null(eventResult.Error);
            Assert.Null(eventResult.ErrorCode);

            var configResult = EpoxyProtocol.Classify(configFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.ProcessConfig, configResult.Disposition);
            Assert.Null(configResult.Headers);
            Assert.Null(configResult.LayerData.Array);
            Assert.Null(configResult.Payload.Array);
            Assert.Null(configResult.Error);
            Assert.Null(configResult.ErrorCode);

            var protocolErrorResult = EpoxyProtocol.Classify(protocolErrorFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HandleProtocolError, protocolErrorResult.Disposition);
            Assert.Null(protocolErrorResult.Headers);
            Assert.Null(protocolErrorResult.LayerData.Array);
            Assert.Null(protocolErrorResult.Payload.Array);
            Assert.AreEqual(MeaninglessErrorCode, protocolErrorResult.Error.error_code);
            Assert.Null(protocolErrorResult.ErrorCode);

            var doubleProtocolErrorResult = EpoxyProtocol.Classify(doubleProtocolErrorFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HangUp, doubleProtocolErrorResult.Disposition);
            Assert.Null(doubleProtocolErrorResult.Headers);
            Assert.Null(doubleProtocolErrorResult.LayerData.Array);
            Assert.Null(doubleProtocolErrorResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.ERROR_IN_ERROR, doubleProtocolErrorResult.Error.error_code);
            Assert.Null(doubleProtocolErrorResult.ErrorCode);
        }

        [Test]
        public void Classify_InvalidPreconditions()
        {
            var nullResult = EpoxyProtocol.Classify(null, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, nullResult.Disposition);
            Assert.Null(nullResult.Headers);
            Assert.Null(nullResult.LayerData.Array);
            Assert.Null(nullResult.Payload.Array);
            Assert.Null(nullResult.Error);
            Assert.Null(nullResult.ErrorCode);
        }

        [Test]
        public void Classify_MalformedFrame()
        {
            var emptyResult = EpoxyProtocol.Classify(emptyFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, emptyResult.Disposition);
            Assert.Null(emptyResult.Headers);
            Assert.Null(emptyResult.LayerData.Array);
            Assert.Null(emptyResult.Payload.Array);
            Assert.Null(emptyResult.Error);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, emptyResult.ErrorCode);

            var shortResult = EpoxyProtocol.Classify(shortRequestFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, shortResult.Disposition);
            Assert.Null(shortResult.Headers);
            Assert.Null(shortResult.LayerData.Array);
            Assert.Null(shortResult.Payload.Array);
            Assert.Null(shortResult.Error);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, shortResult.ErrorCode);

            var doubleHeadersResult = EpoxyProtocol.Classify(doubleHeadersRequestFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, doubleHeadersResult.Disposition);
            Assert.Null(doubleHeadersResult.Headers);
            Assert.Null(doubleHeadersResult.LayerData.Array);
            Assert.Null(doubleHeadersResult.Payload.Array);
            Assert.Null(doubleHeadersResult.Error);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, doubleHeadersResult.ErrorCode);

            var headersConfigRequestResult = EpoxyProtocol.Classify(headersConfigRequestFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, headersConfigRequestResult.Disposition);
            Assert.Null(headersConfigRequestResult.Headers);
            Assert.Null(headersConfigRequestResult.LayerData.Array);
            Assert.Null(headersConfigRequestResult.Payload.Array);
            Assert.Null(headersConfigRequestResult.Error);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, headersConfigRequestResult.ErrorCode);

            var doublePayloadResult = EpoxyProtocol.Classify(doublePayloadRequestFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, doublePayloadResult.Disposition);
            Assert.Null(doublePayloadResult.Headers);
            Assert.Null(doublePayloadResult.LayerData.Array);
            Assert.Null(doublePayloadResult.Payload.Array);
            Assert.Null(doublePayloadResult.Error);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, doublePayloadResult.ErrorCode);

            var backwardsResult = EpoxyProtocol.Classify(backwardsRequestFrame, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, backwardsResult.Disposition);
            Assert.Null(backwardsResult.Headers);
            Assert.Null(backwardsResult.LayerData.Array);
            Assert.Null(backwardsResult.Payload.Array);
            Assert.Null(backwardsResult.Error);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, backwardsResult.ErrorCode);

            var configExtraResult = EpoxyProtocol.Classify(configFrameExtra, LoggerTests.BlackHole);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, configExtraResult.Disposition);
            Assert.Null(configExtraResult.Headers);
            Assert.Null(configExtraResult.LayerData.Array);
            Assert.Null(configExtraResult.Payload.Array);
            Assert.Null(configExtraResult.Error);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, configExtraResult.ErrorCode);
        }
    }
}
