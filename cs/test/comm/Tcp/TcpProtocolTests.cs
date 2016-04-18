using System;
using System.Collections.Generic;
using System.Linq;

namespace UnitTest.Tcp
{
    using Bond;
    using Bond.Comm;
    using Bond.Comm.Tcp;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;

    [TestFixture]
    class TcpProtocolTests
    {
        private static readonly IEnumerable<TcpProtocol.ClassifyState> allStates =
                Enum.GetValues(typeof (TcpProtocol.ClassifyState)).Cast<TcpProtocol.ClassifyState>();

        private const uint GoodRequestId = 1;
        private const uint GoodResponseId = 1;
        private const string GoodMethod = "ShaveYaks";
        private static readonly Error goodPayload = new Error();
        private static readonly IMessage<Error> meaninglessMessage = new Message<Error>(goodPayload);
        private static readonly ProtocolErrorCode meaninglessErrorCode = ProtocolErrorCode.GENERIC_ERROR;

        private static readonly TcpHeaders goodRequestHeaders = new TcpHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Request,
            request_id = GoodRequestId
        };
        private static readonly TcpHeaders goodResponseHeaders = new TcpHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Response,
            request_id = GoodResponseId
        };
        private static readonly TcpHeaders goodEventHeaders = new TcpHeaders
        {
            error_code = 0,
            method_name = GoodMethod,
            payload_type = PayloadType.Event,
            request_id = GoodRequestId
        };

        private static Frame goodRequestFrame;
        private static Frame goodResponseFrame;
        private static Frame goodEventFrame;
        private static Frame shortRequestFrame;         // a request frame with TcpHeaders but no PayloadData
        private static Frame doubleHeadersRequestFrame; // a request frame with duplicate TcpHeaders
        private static Frame doublePayloadRequestFrame; // a request frame with duplicate PayloadData
        private static Frame backwardsRequestFrame;     // a request frame with PayloadData before TcpHeaders
        private static Frame protocolErrorFrame;        // a frame with a well-formed ProtocolError
        private static readonly Frame emptyFrame = new Frame(0);


        private static IEnumerable<TcpProtocol.ClassifyState> StatesExcept(params TcpProtocol.ClassifyState[] excludedStates)
        {
            return allStates.Where(state => !excludedStates.Contains(state));
        }

        [TestFixtureSetUp]
        public static void CreateFrames()
        {
            goodRequestFrame = TcpConnection.MessageToFrame(
                GoodRequestId, GoodMethod, PayloadType.Request, meaninglessMessage);
            goodResponseFrame = TcpConnection.MessageToFrame(
                GoodResponseId, GoodMethod, PayloadType.Response, meaninglessMessage);
            goodEventFrame = TcpConnection.MessageToFrame(
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

            protocolErrorFrame = TcpConnection.MakeProtocolErrorFrame(meaninglessErrorCode);
        }



        // For each state that is implemented as a function, test:
        //  * that it works in the happy path
        //  * that it transitions to an internal error if it's coming from any unexpected state
        //  * that it transitions to an internal error if any state left by previous transitions is unacceptable
        //  * that it transitions to an expected error state if the frame is malformed

        [Test]
        public void TransitionExpectFrame_Valid()
        {
            var after = TcpProtocol.TransitionExpectFrame(TcpProtocol.ClassifyState.ExpectFrame, goodRequestFrame);
            Assert.AreEqual(TcpProtocol.ClassifyState.ExpectFirstFramelet, after);
        }

        [Test]
        public void TransitionExpectFrame_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.ExpectFrame))
            {
                var after = TcpProtocol.TransitionExpectFrame(invalid, goodRequestFrame);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            }
        }

        [Test]
        public void TransitionExpectFrame_InvalidPreconditions()
        {
            var after = TcpProtocol.TransitionExpectFrame(TcpProtocol.ClassifyState.ExpectFrame, null);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
        }

        [Test]
        public void TransitionExpectFirstFramelet_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectFirstFramelet(
                TcpProtocol.ClassifyState.ExpectFirstFramelet, goodRequestFrame, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.ExpectTcpHeaders, after);
            Assert.Null(errorCode);

            after = TcpProtocol.TransitionExpectFirstFramelet(
                TcpProtocol.ClassifyState.ExpectFirstFramelet, protocolErrorFrame, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.ExpectProtocolError, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectFirstFramelet_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.ExpectFirstFramelet))
            {
                ProtocolErrorCode? errorCode = null;

                var after = TcpProtocol.TransitionExpectFirstFramelet(invalid, goodRequestFrame, ref errorCode);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectFirstFramelet_InvalidPreconditions()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectFirstFramelet(
                TcpProtocol.ClassifyState.ExpectFirstFramelet, null, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectFirstFramelet_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectFirstFramelet(
                TcpProtocol.ClassifyState.ExpectFirstFramelet, emptyFrame, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            errorCode = null;

            after = TcpProtocol.TransitionExpectFirstFramelet(
                TcpProtocol.ClassifyState.ExpectFirstFramelet, backwardsRequestFrame, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectTcpHeaders_Valid()
        {
            TcpHeaders headers = null;
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectTcpHeaders(
                TcpProtocol.ClassifyState.ExpectTcpHeaders, goodRequestFrame, ref headers, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.ExpectPayload, after);
            Assert.NotNull(headers);
            Assert.AreEqual(GoodRequestId, headers.request_id);
            Assert.AreEqual(0, headers.error_code);
            Assert.AreEqual(GoodMethod, headers.method_name);
            Assert.AreEqual(PayloadType.Request, headers.payload_type);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectTcpHeaders_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.ExpectTcpHeaders))
            {
                TcpHeaders headers = null;
                ProtocolErrorCode? errorCode = null;

                var after = TcpProtocol.TransitionExpectTcpHeaders(
                    invalid, goodRequestFrame, ref headers, ref errorCode);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(headers);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectTcpHeaders_InvalidPreconditions()
        {
            TcpHeaders headers = null;
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectTcpHeaders(
                TcpProtocol.ClassifyState.ExpectTcpHeaders, null, ref headers, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);

            after = TcpProtocol.TransitionExpectTcpHeaders(
                TcpProtocol.ClassifyState.ExpectTcpHeaders, emptyFrame, ref headers, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);

            after = TcpProtocol.TransitionExpectTcpHeaders(
                TcpProtocol.ClassifyState.ExpectTcpHeaders, backwardsRequestFrame, ref headers, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(headers);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectPayload_Valid()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectPayload(
                TcpProtocol.ClassifyState.ExpectPayload, goodRequestFrame, goodRequestHeaders, ref payload,
                ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.ExpectEndOfFrame, after);
            Assert.NotNull(payload.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectPayload_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.ExpectPayload))
            {
                var payload = new ArraySegment<byte>();
                ProtocolErrorCode? errorCode = null;

                var after = TcpProtocol.TransitionExpectPayload(
                    invalid, goodRequestFrame, goodRequestHeaders, ref payload, ref errorCode);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(payload.Array);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectPayload_InvalidPreconditions()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectPayload(
                TcpProtocol.ClassifyState.ExpectPayload, null, goodRequestHeaders, ref payload, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(payload.Array);
            Assert.Null(errorCode);

            after = TcpProtocol.TransitionExpectPayload(
                TcpProtocol.ClassifyState.ExpectPayload, goodRequestFrame, null, ref payload, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(payload.Array);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectPayload_MalformedFrame()
        {
            var payload = new ArraySegment<byte>();
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectPayload(
                TcpProtocol.ClassifyState.ExpectPayload, emptyFrame, goodRequestHeaders, ref payload, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);

            after = TcpProtocol.TransitionExpectPayload(
                TcpProtocol.ClassifyState.ExpectPayload, shortRequestFrame, goodRequestHeaders, ref payload,
                ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.MalformedFrame, after);
            Assert.Null(payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectEndOfFrame(
                TcpProtocol.ClassifyState.ExpectEndOfFrame, goodRequestFrame, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.FrameComplete, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.ExpectEndOfFrame))
            {
                ProtocolErrorCode? errorCode = null;

                var after = TcpProtocol.TransitionExpectEndOfFrame(invalid, goodRequestFrame, ref errorCode);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionExpectEndOfFrame_InvalidPrecondition()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectEndOfFrame(
                TcpProtocol.ClassifyState.ExpectEndOfFrame, null, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionExpectEndOfFrame_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionExpectEndOfFrame(
                TcpProtocol.ClassifyState.ExpectEndOfFrame, doublePayloadRequestFrame, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, errorCode);
        }

        [Test]
        public void TransitionFrameComplete_Valid()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionFrameComplete(
                TcpProtocol.ClassifyState.FrameComplete, goodRequestHeaders, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.ValidFrame, after);
            Assert.Null(errorCode);

            after = TcpProtocol.TransitionFrameComplete(
                TcpProtocol.ClassifyState.FrameComplete, goodResponseHeaders, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.ValidFrame, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionFrameComplete_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.FrameComplete))
            {
                ProtocolErrorCode? errorCode = null;

                var after = TcpProtocol.TransitionFrameComplete(invalid, goodRequestHeaders, ref errorCode);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(errorCode);
            }
        }

        [Test]
        public void TransitionFrameComplete_InvalidPreconditions()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionFrameComplete(
                TcpProtocol.ClassifyState.FrameComplete, null, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(errorCode);
        }

        [Test]
        public void TransitionFrameComplete_MalformedFrame()
        {
            ProtocolErrorCode? errorCode = null;

            var after = TcpProtocol.TransitionFrameComplete(
                TcpProtocol.ClassifyState.FrameComplete, goodEventHeaders, ref errorCode);
            Assert.AreEqual(TcpProtocol.ClassifyState.MalformedFrame, after);
            Assert.AreEqual(ProtocolErrorCode.NOT_SUPPORTED, errorCode);
        }

        [Test]
        public void TransitionValidFrame_Valid()
        {
            var disposition = TcpProtocol.FrameDisposition.Indeterminate;

            var after = TcpProtocol.TransitionValidFrame(
                TcpProtocol.ClassifyState.ValidFrame, goodRequestHeaders, ref disposition);
            Assert.AreEqual(TcpProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(TcpProtocol.FrameDisposition.DeliverRequestToService, disposition);

            disposition = TcpProtocol.FrameDisposition.Indeterminate;

            after = TcpProtocol.TransitionValidFrame(
                TcpProtocol.ClassifyState.ValidFrame, goodResponseHeaders, ref disposition);
            Assert.AreEqual(TcpProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(TcpProtocol.FrameDisposition.DeliverResponseToProxy, disposition);
        }

        [Test]
        public void TransitionValidFrame_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.ValidFrame))
            {
                var disposition = TcpProtocol.FrameDisposition.Indeterminate;

                var after = TcpProtocol.TransitionValidFrame(invalid, goodRequestHeaders, ref disposition);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
                Assert.AreEqual(TcpProtocol.FrameDisposition.Indeterminate, disposition);
            }
        }

        [Test]
        public void TransitionValidFrame_InvalidPreconditions()
        {
            var disposition = TcpProtocol.FrameDisposition.Indeterminate;

            var after = TcpProtocol.TransitionValidFrame(TcpProtocol.ClassifyState.ValidFrame, null, ref disposition);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.AreEqual(TcpProtocol.FrameDisposition.Indeterminate, disposition);

            after = TcpProtocol.TransitionValidFrame(
                TcpProtocol.ClassifyState.ValidFrame, goodEventHeaders, ref disposition);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.AreEqual(TcpProtocol.FrameDisposition.Indeterminate, disposition);
        }

        [Test]
        public void TransitionExpectProtocolError_Valid()
        {
            ProtocolError error = null;
            var disposition = TcpProtocol.FrameDisposition.Indeterminate;

            var after = TcpProtocol.TransitionExpectProtocolError(
                TcpProtocol.ClassifyState.ExpectProtocolError, protocolErrorFrame, ref error, ref disposition);
            Assert.AreEqual(TcpProtocol.ClassifyState.ClassifiedValidFrame, after);
            Assert.AreEqual(meaninglessErrorCode, error.error_code);
            Assert.AreEqual(TcpProtocol.FrameDisposition.HangUp, disposition);
        }

        [Test]
        public void TransitionExpectProtocolError_InvalidStates()
        {
            foreach (var invalid in StatesExcept(TcpProtocol.ClassifyState.ExpectProtocolError))
            {
                ProtocolError error = null;
                var disposition = TcpProtocol.FrameDisposition.Indeterminate;

                var after = TcpProtocol.TransitionExpectProtocolError(
                    invalid, protocolErrorFrame, ref error, ref disposition);
                Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
                Assert.Null(error);
                Assert.AreEqual(TcpProtocol.FrameDisposition.Indeterminate, disposition);
            }
        }

        [Test]
        public void TransitionExpectProtocolError_InvalidPreconditions()
        {
            ProtocolError error = null;
            var disposition = TcpProtocol.FrameDisposition.Indeterminate;

            var after = TcpProtocol.TransitionExpectProtocolError(
                TcpProtocol.ClassifyState.ExpectProtocolError, null, ref error, ref disposition);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(error);
            Assert.AreEqual(TcpProtocol.FrameDisposition.Indeterminate, disposition);

            after = TcpProtocol.TransitionExpectProtocolError(
                TcpProtocol.ClassifyState.ExpectProtocolError, backwardsRequestFrame, ref error, ref disposition);
            Assert.AreEqual(TcpProtocol.ClassifyState.InternalStateError, after);
            Assert.Null(error);
            Assert.AreEqual(TcpProtocol.FrameDisposition.Indeterminate, disposition);
        }

        // These end-to-end tests cover states that don't fit in functions.

        [Test]
        public void Classify_Valid()
        {
            var requestResult = TcpProtocol.Classify(goodRequestFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.DeliverRequestToService, requestResult.Disposition);
            Assert.AreEqual(goodRequestHeaders.error_code, requestResult.Headers.error_code);
            Assert.AreEqual(goodRequestHeaders.method_name, requestResult.Headers.method_name);
            Assert.AreEqual(goodRequestHeaders.payload_type, requestResult.Headers.payload_type);
            Assert.AreEqual(goodRequestHeaders.request_id, requestResult.Headers.request_id);
            Assert.AreEqual(goodRequestFrame.Framelets[goodRequestFrame.Count - 1].Contents, requestResult.Payload);
            Assert.Null(requestResult.ErrorCode);

            var responseResult = TcpProtocol.Classify(goodResponseFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.DeliverResponseToProxy, responseResult.Disposition);
            Assert.AreEqual(goodResponseHeaders.error_code, responseResult.Headers.error_code);
            Assert.AreEqual(goodResponseHeaders.method_name, responseResult.Headers.method_name);
            Assert.AreEqual(goodResponseHeaders.payload_type, responseResult.Headers.payload_type);
            Assert.AreEqual(goodResponseHeaders.request_id, responseResult.Headers.request_id);
            Assert.AreEqual(goodResponseFrame.Framelets[goodResponseFrame.Count - 1].Contents, responseResult.Payload);
            Assert.Null(requestResult.ErrorCode);

            var protocolErrorResult = TcpProtocol.Classify(protocolErrorFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.HangUp, protocolErrorResult.Disposition);
            Assert.Null(protocolErrorResult.Headers);
            Assert.Null(protocolErrorResult.Payload.Array);
            Assert.AreEqual(meaninglessErrorCode, protocolErrorResult.Error.error_code);
        }

        [Test]
        public void Classify_InvalidPreconditions()
        {
            var nullResult = TcpProtocol.Classify(null);
            Assert.AreEqual(TcpProtocol.FrameDisposition.Indeterminate, nullResult.Disposition);
            Assert.Null(nullResult.Headers);
            Assert.Null(nullResult.Payload.Array);
            Assert.Null(nullResult.ErrorCode);
        }

        [Test]
        public void Classify_MalformedFrame()
        {
            var eventResult = TcpProtocol.Classify(goodEventFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.SendProtocolError, eventResult.Disposition);
            Assert.Null(eventResult.Headers);
            Assert.Null(eventResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.NOT_SUPPORTED, eventResult.ErrorCode);

            var emptyResult = TcpProtocol.Classify(emptyFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.SendProtocolError, emptyResult.Disposition);
            Assert.Null(emptyResult.Headers);
            Assert.Null(emptyResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, emptyResult.ErrorCode);

            var shortResult = TcpProtocol.Classify(shortRequestFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.SendProtocolError, shortResult.Disposition);
            Assert.Null(shortResult.Headers);
            Assert.Null(shortResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, shortResult.ErrorCode);

            var doubleHeadersResult = TcpProtocol.Classify(doubleHeadersRequestFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.SendProtocolError, doubleHeadersResult.Disposition);
            Assert.Null(doubleHeadersResult.Headers);
            Assert.Null(doubleHeadersResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, doubleHeadersResult.ErrorCode);

            var doublePayloadResult = TcpProtocol.Classify(doublePayloadRequestFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.SendProtocolError, doublePayloadResult.Disposition);
            Assert.Null(doublePayloadResult.Headers);
            Assert.Null(doublePayloadResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, doublePayloadResult.ErrorCode);

            var backwardsResult = TcpProtocol.Classify(backwardsRequestFrame);
            Assert.AreEqual(TcpProtocol.FrameDisposition.SendProtocolError, backwardsResult.Disposition);
            Assert.Null(backwardsResult.Headers);
            Assert.Null(backwardsResult.Payload.Array);
            Assert.AreEqual(ProtocolErrorCode.MALFORMED_DATA, backwardsResult.ErrorCode);
        }
    }
}
