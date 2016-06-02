// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System;
using System.Linq;

namespace UnitTest.Epoxy
{
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;
    using UnitTest.Comm;

    [TestFixture]
    class EpoxyProtocolTests
    {
        private const uint GoodRequestId = 1;
        private const uint GoodResponseId = 1;
        private const string GoodMethod = "ShaveYaks";
        private static readonly Error goodPayload = new Error();
        private static readonly IMessage<Error> meaninglessMessage = new Message<Error>(goodPayload);
        private static readonly ProtocolErrorCode meaninglessErrorCode = ProtocolErrorCode.GENERIC_ERROR;
        private static readonly ArraySegment<byte> emptyLayerData = new ArraySegment<byte>();
        private static readonly ArraySegment<byte> nonEmptyLayerData = new ArraySegment<byte>(new byte[] { 3 });

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
        private static readonly Dummy DummyObject = new Dummy
        {
            int_value = 0x1234
        };
        private static readonly Bond.IBonded GoodLayerData = new Bond.Bonded<Dummy>(DummyObject);

        private static Frame goodRequestFrame;
        private static Frame goodRequestLayerDataFrame;
        private static Frame goodResponseFrame;
        private static Frame goodEventFrame;
        private static Frame shortRequestFrame;         // a request frame with EpoxyHeaders but no PayloadData
        private static Frame doubleHeadersRequestFrame; // a request frame with duplicate EpoxyHeaders
        private static Frame doublePayloadRequestFrame; // a request frame with duplicate PayloadData
        private static Frame backwardsRequestFrame;     // a request frame with PayloadData before EpoxyHeaders
        private static Frame configFrame;               // a frame with a well-formed EpoxyConfig
        private static Frame configFrameExtra;          // a frame with a well-formed EpoxyConfig and extra stuff
        private static Frame configFrameBadConfigData;  // a frame that fits that shape of a config frame, but with a payload that can't be deserialized
        private static Frame protocolErrorFrame;        // a frame with a well-formed ProtocolError
        private static Frame doubleProtocolErrorFrame;  // a frame with two ProtocolError frames
        private static readonly Frame emptyFrame = new Frame(0);

        [TestFixtureSetUp]
        public static void CreateFrames()
        {
            goodRequestFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Request, meaninglessMessage, null);
            goodRequestLayerDataFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Request, meaninglessMessage, GoodLayerData);

            goodResponseFrame = EpoxyConnection.MessageToFrame(
                GoodResponseId, GoodMethod, PayloadType.Response, meaninglessMessage, null);
            goodEventFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Event, meaninglessMessage, null);
            var goodFrameletCount = goodRequestFrame.Count;

            shortRequestFrame = new Frame(goodFrameletCount - 1);
            for (var i = 0; i < goodFrameletCount - 1; i++)
            {
                shortRequestFrame.Add(goodRequestFrame.Framelets[i]);
            }

            doubleHeadersRequestFrame = new Frame(goodFrameletCount + 1);
            doubleHeadersRequestFrame.Add(goodRequestFrame.Framelets[0]);
            for (var i = 0; i < goodFrameletCount; i++)
            {
                doubleHeadersRequestFrame.Add(goodRequestFrame.Framelets[i]);
            }

            doublePayloadRequestFrame = new Frame(goodFrameletCount + 1);
            for (var i = 0; i < goodFrameletCount; i++)
            {
                doublePayloadRequestFrame.Add(goodRequestFrame.Framelets[i]);
            }
            doublePayloadRequestFrame.Add(goodRequestFrame.Framelets[goodFrameletCount - 1]);

            backwardsRequestFrame = new Frame(goodFrameletCount);
            foreach (var framelet in goodRequestFrame.Framelets.Reverse())
            {
                backwardsRequestFrame.Add(framelet);
            }

            protocolErrorFrame = EpoxyConnection.MakeProtocolErrorFrame(meaninglessErrorCode, null);

            doubleProtocolErrorFrame = EpoxyConnection.MakeProtocolErrorFrame(meaninglessErrorCode, null);
            doubleProtocolErrorFrame.Add(doubleProtocolErrorFrame.Framelets[0]);

            configFrame = EpoxyConnection.MakeConfigFrame();

            configFrameExtra = EpoxyConnection.MakeConfigFrame();
            configFrameExtra.Add(goodRequestFrame.Framelets[0]);

            var invalidConfigData = new ArraySegment<byte>(new byte[] { 0x01 });
            configFrameBadConfigData = new Frame(1);
            configFrameBadConfigData.Add(new Framelet(FrameletType.EpoxyConfig, invalidConfigData));

            protocolErrorFrame = EpoxyConnection.MakeProtocolErrorFrame(meaninglessErrorCode, null);
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
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, goodRequestFrame, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, after);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, protocolErrorFrame, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectProtocolError, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectFirstFramelet_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, emptyFrame, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            errorCode = null;

            after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, backwardsRequestFrame, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectEpoxyHeaders_Valid()
        {
            EpoxyHeaders headers = null;
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, goodRequestFrame, ref headers, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, after);
            Assert.NotNull(headers);
            Assert.AreEqual(GoodRequestId, headers.conversation_id);
            Assert.AreEqual(0, headers.error_code);
            Assert.AreEqual(GoodMethod, headers.method_name);
            Assert.AreEqual(PayloadType.Request, headers.payload_type);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, goodRequestLayerDataFrame,
                ref headers, ref errorCode);
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
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, emptyFrame, ref headers, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, backwardsRequestFrame, ref headers, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionOptionalExpectLayerData_Valid()
        {
            var layerData = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectOptionalLayerData(
                EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, goodRequestFrame, goodRequestHeaders,
                ref layerData, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectPayload, after);
            Assert.Null(layerData.Array);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectOptionalLayerData(
                EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, goodRequestLayerDataFrame, goodRequestHeaders,
                ref layerData, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectPayload, after);
            Assert.NotNull(layerData.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionOptionalExpectLayerData_InvalidPreconditions()
        {
            var layerData = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectOptionalLayerData(
                EpoxyProtocol.ClassifyState.ExpectOptionalLayerData, goodRequestFrame, null,
                ref layerData, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(layerData.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionOptionalExpectLayerData_MalformedFrame()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, emptyFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, shortRequestFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectPayload_Valid()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectEndOfFrame, after);
            Assert.NotNull(payload.Array);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestLayerDataFrame, goodRequestHeaders, nonEmptyLayerData,
                ref payload, ref errorCode);
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
                ref payload, ref errorCode);
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
                ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, shortRequestFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestLayerDataFrame, goodRequestHeaders, emptyLayerData,
                ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestFrame, goodRequestHeaders, nonEmptyLayerData,
                ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, goodRequestFrame, emptyLayerData, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.FrameComplete, after);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, goodRequestLayerDataFrame, nonEmptyLayerData, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.FrameComplete, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, doublePayloadRequestFrame, emptyLayerData, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionFrameComplete_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, goodRequestHeaders, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ValidFrame, after);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, goodResponseHeaders, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ValidFrame, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionFrameComplete_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, unknownTypeHeaders, ref errorCode);
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
                EpoxyProtocol.ClassifyState.ExpectConfig, configFrame, ref errorCode, ref disposition);
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
                EpoxyProtocol.ClassifyState.ExpectConfig, configFrameExtra, ref errorCode, ref disposition);
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
                EpoxyProtocol.ClassifyState.ExpectConfig, configFrameBadConfigData, ref errorCode, ref disposition);
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
                EpoxyProtocol.ClassifyState.ExpectConfig, emptyFrame, ref errorCode, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);

            // not a config frame
            after = EpoxyProtocol.TransitionExpectConfig(
                EpoxyProtocol.ClassifyState.ExpectConfig, protocolErrorFrame, ref errorCode, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
        }

        [Test]
        public void TransitionExpectProtocolError_Valid()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, protocolErrorFrame, ref error, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(meaninglessErrorCode, error.error_code);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HandleProtocolError, disposition);
        }

        [Test]
        public void TransitionExpectProtocolError_Error()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, doubleProtocolErrorFrame, ref error, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ErrorInErrorFrame, after);
        }

        [Test]
        public void TransitionExpectProtocolError_InvalidPreconditions()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, backwardsRequestFrame, ref error, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(error);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
        }

        // These end-to-end tests cover states that don't fit in functions.

        [Test]
        public void Classify_Valid()
        {
            var requestResult = EpoxyProtocol.Classify(goodRequestFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverRequestToService, requestResult.Disposition);
            Assert.AreEqual(goodRequestHeaders.error_code, requestResult.Headers.error_code);
            Assert.AreEqual(goodRequestHeaders.method_name, requestResult.Headers.method_name);
            Assert.AreEqual(goodRequestHeaders.payload_type, requestResult.Headers.payload_type);
            Assert.AreEqual(goodRequestHeaders.conversation_id, requestResult.Headers.conversation_id);
            Assert.AreEqual(goodRequestFrame.Framelets[goodRequestFrame.Count - 1].Contents, requestResult.Payload);
            Assert.Null(requestResult.ErrorCode);

            var responseResult = EpoxyProtocol.Classify(goodResponseFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverResponseToProxy, responseResult.Disposition);
            Assert.AreEqual(goodResponseHeaders.error_code, responseResult.Headers.error_code);
            Assert.AreEqual(goodResponseHeaders.method_name, responseResult.Headers.method_name);
            Assert.AreEqual(goodResponseHeaders.payload_type, responseResult.Headers.payload_type);
            Assert.AreEqual(goodResponseHeaders.conversation_id, responseResult.Headers.conversation_id);
            Assert.AreEqual(goodResponseFrame.Framelets[goodResponseFrame.Count - 1].Contents, responseResult.Payload);
            Assert.Null(requestResult.ErrorCode);

            var configResult = EpoxyProtocol.Classify(configFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.ProcessConfig, configResult.Disposition);
            Assert.Null(configResult.Headers);
            Assert.Null(configResult.ErrorCode);

            var protocolErrorResult = EpoxyProtocol.Classify(protocolErrorFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HandleProtocolError, protocolErrorResult.Disposition);
            Assert.Null(protocolErrorResult.Headers);
            Assert.Null(protocolErrorResult.Payload.Array);
            Assert.AreEqual(meaninglessErrorCode, protocolErrorResult.Error.error_code);

            var doubleProtocolErrorResult = EpoxyProtocol.Classify(doubleProtocolErrorFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HangUp, doubleProtocolErrorResult.Disposition);
            Assert.Null(doubleProtocolErrorResult.Headers);
            Assert.Null(doubleProtocolErrorResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.ERROR_IN_ERROR, doubleProtocolErrorResult.Error.error_code);

            var eventResult = EpoxyProtocol.Classify(goodEventFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverEventToService, eventResult.Disposition);
            Assert.AreEqual(goodEventHeaders.error_code, eventResult.Headers.error_code);
            Assert.AreEqual(goodEventHeaders.method_name, eventResult.Headers.method_name);
            Assert.AreEqual(goodEventHeaders.payload_type, eventResult.Headers.payload_type);
            Assert.AreEqual(goodEventHeaders.conversation_id, eventResult.Headers.conversation_id);
            Assert.AreEqual(goodEventFrame.Framelets[goodEventFrame.Count - 1].Contents, eventResult.Payload);
        }

        [Test]
        public void Classify_InvalidPreconditions()
        {
            var nullResult = EpoxyProtocol.Classify(null);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, nullResult.Disposition);
            Assert.Null(nullResult.Headers);
            Assert.Null(nullResult.Payload.Array);
            Assert.Null(nullResult.ErrorCode);
        }

        [Test]
        public void Classify_MalformedFrame()
        {
            var emptyResult = EpoxyProtocol.Classify(emptyFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, emptyResult.Disposition);
            Assert.Null(emptyResult.Headers);
            Assert.Null(emptyResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, emptyResult.ErrorCode);

            var shortResult = EpoxyProtocol.Classify(shortRequestFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, shortResult.Disposition);
            Assert.Null(shortResult.Headers);
            Assert.Null(shortResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, shortResult.ErrorCode);

            var doubleHeadersResult = EpoxyProtocol.Classify(doubleHeadersRequestFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, doubleHeadersResult.Disposition);
            Assert.Null(doubleHeadersResult.Headers);
            Assert.Null(doubleHeadersResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, doubleHeadersResult.ErrorCode);

            var doublePayloadResult = EpoxyProtocol.Classify(doublePayloadRequestFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, doublePayloadResult.Disposition);
            Assert.Null(doublePayloadResult.Headers);
            Assert.Null(doublePayloadResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, doublePayloadResult.ErrorCode);

            var backwardsResult = EpoxyProtocol.Classify(backwardsRequestFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, backwardsResult.Disposition);
            Assert.Null(backwardsResult.Headers);
            Assert.Null(backwardsResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, backwardsResult.ErrorCode);

            var configExtraResult = EpoxyProtocol.Classify(configFrameExtra);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.SendProtocolError, configExtraResult.Disposition);
            Assert.Null(configExtraResult.Headers);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, configExtraResult.ErrorCode);
        }
    }
}
