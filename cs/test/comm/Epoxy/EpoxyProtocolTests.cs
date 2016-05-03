// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

using System;
using System.Collections.Generic;
using System.Linq;

namespace UnitTest.Epoxy
{
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;

    [TestFixture]
    class EpoxyProtocolTests
    {
        private static readonly IEnumerable<EpoxyProtocol.ClassifyState> allStates =
                Enum.GetValues(typeof (EpoxyProtocol.ClassifyState)).Cast<EpoxyProtocol.ClassifyState>();

        private const uint GoodRequestId = 1;
        private const uint GoodResponseId = 1;
        private const string GoodMethod = "ShaveYaks";
        private static readonly Error goodPayload = new Error();
        private static readonly IMessage<Error> meaninglessMessage = new Message<Error>(goodPayload);
        private static readonly ProtocolErrorCode meaninglessErrorCode = ProtocolErrorCode.GENERIC_ERROR;

        private static readonly EpoxyHeaders goodRequestHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Request,
            request_id = GoodRequestId
        };
        private static readonly EpoxyHeaders goodResponseHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Response,
            request_id = GoodResponseId
        };
        private static readonly EpoxyHeaders goodEventHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Event,
            request_id = GoodRequestId
        };
        private static readonly EpoxyHeaders unknownTypeHeaders = new EpoxyHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = (PayloadType)(-100),
            request_id = GoodRequestId
        };

        private static Frame goodRequestFrame;
        private static Frame goodResponseFrame;
        private static Frame goodEventFrame;
        private static Frame shortRequestFrame;         // a request frame with EpoxyHeaders but no PayloadData
        private static Frame doubleHeadersRequestFrame; // a request frame with duplicate EpoxyHeaders
        private static Frame doublePayloadRequestFrame; // a request frame with duplicate PayloadData
        private static Frame backwardsRequestFrame;     // a request frame with PayloadData before EpoxyHeaders
        private static Frame protocolErrorFrame;        // a frame with a well-formed ProtocolError
        private static readonly Frame emptyFrame = new Frame(0);


        private static IEnumerable<EpoxyProtocol.ClassifyState> StatesExcept(params EpoxyProtocol.ClassifyState[] excludedStates)
        {
            return allStates.Where(state => !excludedStates.Contains(state));
        }

        [TestFixtureSetUp]
        public static void CreateFrames()
        {
            goodRequestFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Request, meaninglessMessage);
            goodResponseFrame = EpoxyConnection.MessageToFrame(
                GoodResponseId, GoodMethod, PayloadType.Response, meaninglessMessage);
            goodEventFrame = EpoxyConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Event, meaninglessMessage);
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

            protocolErrorFrame = EpoxyConnection.MakeProtocolErrorFrame(meaninglessErrorCode);
        }



        // For each state that is implemented as a function, test:
        //  * that it works in the happy path
        //  * that it transitions to an internal error if it's coming from any unexpected state
        //  * that it transitions to an internal error if any state left by previous transitions is unacceptable
        //  * that it transitions to an expected error state if the frame is malformed

        [Test]
        public void TransitionExpectFrame_Valid()
        {
            var after = EpoxyProtocol.TransitionExpectFrame(EpoxyProtocol.ClassifyState.ExpectFrame, goodRequestFrame);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectFirstFramelet, after);
        }

        [Test]
        public void TransitionExpectFrame_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.ExpectFrame))
            {
                var after = EpoxyProtocol.TransitionExpectFrame(invalid, goodRequestFrame);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            }
        }

        [Test]
        public void TransitionExpectFrame_InvalidPreconditions()
        {
            var after = EpoxyProtocol.TransitionExpectFrame(EpoxyProtocol.ClassifyState.ExpectFrame, null);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
        }

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
        public void TransitionExpectFirstFramelet_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.ExpectFirstFramelet))
            {
                ProtocolErrorCode? errorCode = null;

                var after = EpoxyProtocol.TransitionExpectFirstFramelet(invalid, goodRequestFrame, ref errorCode);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectFirstFramelet_InvalidPreconditions()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectFirstFramelet(
                EpoxyProtocol.ClassifyState.ExpectFirstFramelet, null, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
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
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectPayload, after);
            Assert.NotNull(headers);
            Assert.AreEqual(GoodRequestId, headers.request_id);
            Assert.AreEqual(0, headers.error_code);
            Assert.AreEqual(GoodMethod, headers.method_name);
            Assert.AreEqual(PayloadType.Request, headers.payload_type);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEpoxyHeaders_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders))
            {
                EpoxyHeaders headers = null;
                ProtocolErrorCode? errorCode = null;

                var after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                    invalid, goodRequestFrame, ref headers, ref errorCode);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(headers);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectEpoxyHeaders_InvalidPreconditions()
        {
            EpoxyHeaders headers = null;
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
                EpoxyProtocol.ClassifyState.ExpectEpoxyHeaders, null, ref headers, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectEpoxyHeaders(
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
        public void TransitionExpectPayload_Valid()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestFrame, goodRequestHeaders, ref payload,
                ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ExpectEndOfFrame, after);
            Assert.NotNull(payload.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectPayload_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.ExpectPayload))
            {
                var payload = new ArraySegment<byte>();
                ProtocolErrorCode? errorCode = null;

                var after = EpoxyProtocol.TransitionExpectPayload(
                    invalid, goodRequestFrame, goodRequestHeaders, ref payload, ref errorCode);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(payload.Array);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectPayload_InvalidPreconditions()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, null, goodRequestHeaders, ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(payload.Array);
            Assert.Null(errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, goodRequestFrame, null, ref payload, ref errorCode);
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
                EpoxyProtocol.ClassifyState.ExpectPayload, emptyFrame, goodRequestHeaders, ref payload, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = EpoxyProtocol.TransitionExpectPayload(
                EpoxyProtocol.ClassifyState.ExpectPayload, shortRequestFrame, goodRequestHeaders, ref payload,
                ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, goodRequestFrame, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.FrameComplete, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.ExpectEndOfFrame))
            {
                ProtocolErrorCode? errorCode = null;

                var after = EpoxyProtocol.TransitionExpectEndOfFrame(invalid, goodRequestFrame, ref errorCode);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectEndOfFrame_InvalidPrecondition()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, null, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionExpectEndOfFrame(
                EpoxyProtocol.ClassifyState.ExpectEndOfFrame, doublePayloadRequestFrame, ref errorCode);
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
        public void TransitionFrameComplete_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.FrameComplete))
            {
                ProtocolErrorCode? errorCode = null;

                var after = EpoxyProtocol.TransitionFrameComplete(invalid, goodRequestHeaders, ref errorCode);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionFrameComplete_InvalidPreconditions()
        {
            ProtocolErrorCode? errorCode = null;

            var after = EpoxyProtocol.TransitionFrameComplete(
                EpoxyProtocol.ClassifyState.FrameComplete, null, ref errorCode);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
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
        public void TransitionValidFrame_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.ValidFrame))
            {
                var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

                var after = EpoxyProtocol.TransitionValidFrame(invalid, goodRequestHeaders, ref disposition);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
                Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
            }
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
        public void TransitionExpectProtocolError_Valid()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, protocolErrorFrame, ref error, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(meaninglessErrorCode, error.error_code);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HangUp, disposition);
        }

        [Test]
        public void TransitionExpectProtocolError_InvalidStates()
        {
            foreach (var invalid in StatesExcept(EpoxyProtocol.ClassifyState.ExpectProtocolError))
            {
                ProtocolError error = null;
                var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

                var after = EpoxyProtocol.TransitionExpectProtocolError(
                    invalid, protocolErrorFrame, ref error, ref disposition);
                Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(error);
                Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);
            }
        }

        [Test]
        public void TransitionExpectProtocolError_InvalidPreconditions()
        {
            ProtocolError error = null;
            var disposition = EpoxyProtocol.FrameDisposition.Indeterminate;

            var after = EpoxyProtocol.TransitionExpectProtocolError(
                EpoxyProtocol.ClassifyState.ExpectProtocolError, null, ref error, ref disposition);
            Assert.AreEqual(EpoxyProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(error);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.Indeterminate, disposition);

            after = EpoxyProtocol.TransitionExpectProtocolError(
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
            Assert.AreEqual(goodRequestHeaders.request_id, requestResult.Headers.request_id);
            Assert.AreEqual(goodRequestFrame.Framelets[goodRequestFrame.Count - 1].Contents, requestResult.Payload);
            Assert.Null(requestResult.ErrorCode);

            var responseResult = EpoxyProtocol.Classify(goodResponseFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverResponseToProxy, responseResult.Disposition);
            Assert.AreEqual(goodResponseHeaders.error_code, responseResult.Headers.error_code);
            Assert.AreEqual(goodResponseHeaders.method_name, responseResult.Headers.method_name);
            Assert.AreEqual(goodResponseHeaders.payload_type, responseResult.Headers.payload_type);
            Assert.AreEqual(goodResponseHeaders.request_id, responseResult.Headers.request_id);
            Assert.AreEqual(goodResponseFrame.Framelets[goodResponseFrame.Count - 1].Contents, responseResult.Payload);
            Assert.Null(requestResult.ErrorCode);

            var protocolErrorResult = EpoxyProtocol.Classify(protocolErrorFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.HangUp, protocolErrorResult.Disposition);
            Assert.Null(protocolErrorResult.Headers);
            Assert.Null(protocolErrorResult.Payload.Array);
            Assert.AreEqual(meaninglessErrorCode, protocolErrorResult.Error.error_code);

            var eventResult = EpoxyProtocol.Classify(goodEventFrame);
            Assert.AreEqual(EpoxyProtocol.FrameDisposition.DeliverEventToService, eventResult.Disposition);
            Assert.AreEqual(goodEventHeaders.error_code, eventResult.Headers.error_code);
            Assert.AreEqual(goodEventHeaders.method_name, eventResult.Headers.method_name);
            Assert.AreEqual(goodEventHeaders.payload_type, eventResult.Headers.payload_type);
            Assert.AreEqual(goodEventHeaders.request_id, eventResult.Headers.request_id);
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
        }
    }
}
