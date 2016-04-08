using System;

namespace Bond.Comm.Tcp
{
    using System.IO;
    using Bond.IO.Safe;
    using Bond.Protocols;

    internal enum FrameDisposition
    {
        Indeterminate,
        DeliverRequestToService,
        DeliverResponseToProxy,
        SendProtocolError,
        HangUp
    }

    internal struct ClassifyResult
    {
        public FrameDisposition Disposition;
        public TcpHeaders Headers;
        public ArraySegment<byte> Payload;
    }

    internal enum ClassifyState
    {
        ExpectFrame,
        ExpectTcpHeaders,
        ExpectPayload,
        ExpectEndOfFrame,
        FrameComplete,

        ValidFrame,
        ReturnDisposition,
        MalformedFrame,

        InternalStateError      // Indicates a bug in the state machine.
    }

    internal class TcpProtocol
    {
        private static readonly uint maximumTransitions = (uint) Enum.GetNames(typeof(ClassifyState)).Length;

        private const int TcpHeadersIndex = 0;
        private const int PayloadDataIndex = 1;
        private const int FrameSize = 2;

        internal static ClassifyResult Classify(Frame frame)
        {
            if (frame == null)
            {
                return new ClassifyResult
                {
                    Disposition = FrameDisposition.SendProtocolError
                };
            }

            var state = ClassifyState.ExpectFrame;
            TcpHeaders headers = null;
            ArraySegment<byte>? payload = null;
            var disposition = FrameDisposition.Indeterminate;
            uint transitions = 0;
            while (true)
            {
                if (transitions++ > maximumTransitions)
                {
                    return new ClassifyResult
                    {
                        Disposition = FrameDisposition.SendProtocolError
                    };
                }

                switch (state)
                {
                    case ClassifyState.ExpectFrame:
                        state = TransitionExpectFrame(state, frame);
                        continue;

                    case ClassifyState.ExpectTcpHeaders:
                        state = TransitionExpectTcpHeaders(state, frame, ref headers);
                        continue;

                    case ClassifyState.ExpectPayload:
                        state = TransitionExpectPayload(state, frame, headers, ref payload);
                        continue;

                    case ClassifyState.ExpectEndOfFrame:
                        state = TransitionExpectEndOfFrame(state, frame);
                        continue;

                    case ClassifyState.FrameComplete:
                        state = TransitionFrameComplete(state, headers);
                        continue;

                    case ClassifyState.ValidFrame:
                        state = TransitionValidFrame(state, headers, ref disposition);
                        continue;

                    case ClassifyState.ReturnDisposition:
                        if (disposition == FrameDisposition.Indeterminate || headers == null || payload == null)
                        {
                            state = ClassifyState.InternalStateError;
                            continue;
                        }

                        return new ClassifyResult
                        {
                            Disposition = disposition,
                            Headers = headers,
                            Payload = payload.Value
                        };

                    case ClassifyState.MalformedFrame:
                        return new ClassifyResult
                        {
                            Disposition = FrameDisposition.SendProtocolError
                        };

                    case ClassifyState.InternalStateError:
                        return new ClassifyResult
                        {
                            Disposition = FrameDisposition.SendProtocolError
                        };

                    default:
                        Log.Error("{0}.{1}: Unhandled state {2}. Dropping frame.",
                            nameof(TcpProtocol), nameof(Classify), state);
                        return new ClassifyResult
                        {
                            Disposition = FrameDisposition.SendProtocolError
                        };
                }
            }
        }

        internal static ClassifyState TransitionExpectFrame(ClassifyState state, Frame frame)
        {
            if (state != ClassifyState.ExpectFrame || frame == null)
            {
                return ClassifyState.InternalStateError;
            }

            Log.Debug("{0}.{1}: Processing {2} framelets.",
                nameof(TcpProtocol), nameof(TransitionExpectFrame), frame.Count);
            state = ClassifyState.ExpectTcpHeaders;
            return state;
        }

        internal static ClassifyState TransitionExpectTcpHeaders(
            ClassifyState state, Frame frame, ref TcpHeaders headers)
        {
            if (state != ClassifyState.ExpectTcpHeaders || frame == null)
            {
                return ClassifyState.InternalStateError;
            }

            if (TcpHeadersIndex >= frame.Count)
            {
                return ClassifyState.MalformedFrame;
            }

            var framelet = frame.Framelets[TcpHeadersIndex];
            if (framelet.Type != FrameletType.TcpHeaders)
            {
                Log.Error("{0}.{1}: Frame did not begin with TcpHeaders.",
                    nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders));
                return ClassifyState.MalformedFrame;
            }

            var inputBuffer = new InputBuffer(framelet.Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            try
            {
                headers = Deserialize<TcpHeaders>.From(fastBinaryReader);
            }
            catch (Exception ex) when (ex is InvalidDataException || ex is IOException)
            {
                Log.Error(ex, "{0}.{1}: Failed to parse TcpHeaders: {2}.",
                    nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders), ex.Message);
                return ClassifyState.MalformedFrame;
            }

            Log.Debug("{0}.{1}: Extracted TcpHeaders with request ID {2} and payload type {3}.",
                nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders), headers.request_id, headers.payload_type);
            return ClassifyState.ExpectPayload;
        }

        internal static ClassifyState TransitionExpectPayload(
            ClassifyState state, Frame frame, TcpHeaders headers, ref ArraySegment<byte>? payload)
        {
            if (state != ClassifyState.ExpectPayload || frame == null || headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            if (PayloadDataIndex >= frame.Count)
            {
                return ClassifyState.MalformedFrame;
            }

            var framelet = frame.Framelets[PayloadDataIndex];
            if (framelet.Type != FrameletType.PayloadData)
            {
                Log.Error("{0}.{1}: Frame did not continue with PayloadData.",
                    nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders));
                return ClassifyState.MalformedFrame;
            }

            payload = framelet.Contents;
            Log.Debug("{0}.{1}: Extracted {2}-byte payload in request ID {3}.",
                nameof(TcpProtocol), nameof(TransitionExpectPayload), payload.Value.Count, headers.request_id);
            return ClassifyState.ExpectEndOfFrame;
        }

        internal static ClassifyState TransitionExpectEndOfFrame(ClassifyState state, Frame frame)
        {
            if (state != ClassifyState.ExpectEndOfFrame || frame == null)
            {
                return ClassifyState.InternalStateError;
            }

            return frame.Count == FrameSize ? ClassifyState.FrameComplete : ClassifyState.MalformedFrame;
        }

        internal static ClassifyState TransitionFrameComplete(ClassifyState state, TcpHeaders headers)
        {
            if (state != ClassifyState.FrameComplete || headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            switch (headers.payload_type)
            {
                case PayloadType.Request:
                case PayloadType.Response:
                    return ClassifyState.ValidFrame;

                case PayloadType.Event:
                    Log.Warning("{0}.{1}: Received unimplemented payload type {2}.",
                        nameof(TcpProtocol), nameof(TransitionFrameComplete), headers.payload_type);
                    return ClassifyState.MalformedFrame;

                default:
                    Log.Warning("{0}.{1}: Received unrecognized payload type {2}.",
                        nameof(TcpProtocol), nameof(TransitionFrameComplete), headers.payload_type);
                    return ClassifyState.MalformedFrame;
            }
        }

        internal static ClassifyState TransitionValidFrame(
            ClassifyState state, TcpHeaders headers, ref FrameDisposition disposition)
        {
            if (state != ClassifyState.ValidFrame || headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            switch (headers.payload_type)
            {
                case PayloadType.Request:
                    disposition = FrameDisposition.DeliverRequestToService;
                    return ClassifyState.ReturnDisposition;

                case PayloadType.Response:
                    disposition = FrameDisposition.DeliverResponseToProxy;
                    return ClassifyState.ReturnDisposition;

                default:
                    return ClassifyState.InternalStateError;
            }
        }
    }
}
