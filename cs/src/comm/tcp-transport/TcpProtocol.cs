using System;
using System.IO;
using Bond.IO.Safe;
using Bond.Protocols;

namespace Bond.Comm.Tcp
{
    internal static class TcpProtocol
    {
        /// <summary>
        /// Indicates what action should be taken in response to a Frame.
        /// </summary>
        internal enum FrameDisposition
        {
            /// <summary>
            /// The disposition of a Frame about which we know nothing. If this is ever returned by Classify(), it
            /// indicates that Classify() detected a bug and refused to continue.
            /// </summary>
            Indeterminate,

            /// <summary>
            /// The frame was a valid Request.
            /// </summary>
            DeliverRequestToService,

            /// <summary>
            /// The frame was a valid Response.
            /// </summary>
            DeliverResponseToProxy,

            /// <summary>
            /// The frame was not valid, and the caller should send an error to the remote host.
            /// </summary>
            SendProtocolError,

            /// <summary>
            /// The caller should silently close the connection.
            /// </summary>
            HangUp
        }

        /// <summary>
        /// Encapsulates an action that should be taken in response to a Frame and its contents, if it was determined
        /// to be a valid Frame.
        /// </summary>
        internal struct ClassifyResult
        {
            public FrameDisposition Disposition;
            public TcpHeaders Headers;
            public ArraySegment<byte> Payload;
        }

        /// <summary>
        /// States for the state machine inside <see cref="TcpProtocol.Classify"/>. <c>internal</c> for testing.
        /// </summary>
        internal enum ClassifyState
        {
            // These states are the happy path from no knowledge to proving a frame is good. Each is implemented in its
            // own function below.

            // Do we have a frame at all?
            ExpectFrame,

            // Does the frame begin with a valid TcpHeaders?
            ExpectTcpHeaders,

            // Does the frame have a Payload immediately after the TcpHeaders?
            ExpectPayload,

            // The frame has all required framelets. Does it have any trailing ones?
            ExpectEndOfFrame,

            // Do we know what to do with frames with this PayloadType?
            FrameComplete,

            // There are no problems with this frame. What should we do with it?
            ValidFrame,

            // Terminal states. Each of these indicates that we have classified the frame and need to return something.
            // Because these frames always mean the next step is to return, their implementations are inlined in
            // Classify().

            // There are no problems with this frame, and we know what to do with it. Return from Classify().
            ReturnDisposition,

            // We could not interpret the frame, or it violated the protocol. Return from Classify().
            MalformedFrame,

            // We detected a bug in the state machine. Return from Classify() with a FrameDisposition of Indeterminate.
            // This should never happen.
            InternalStateError
        }

        // This needs to be larger than the longest valid path through the state machine.
        private static readonly uint maximumTransitions = (uint) Enum.GetNames(typeof(ClassifyState)).Length;

        // These values are hard-coded because the protocol, as implemented, does not permit any framelet sequences
        // other than [TcpHeaders, PayloadData]. We will need to get rid of these indices and track the index of the
        // next unexamined Framelet once we implement Layers.
        private const int TcpHeadersIndex = 0;
        private const int PayloadDataIndex = 1;
        private const int FrameSize = 2;

        internal static ClassifyResult Classify(Frame frame)
        {
            if (frame == null)
            {
                return new ClassifyResult
                {
                    Disposition = FrameDisposition.Indeterminate
                };
            }

            var state = ClassifyState.ExpectFrame;
            TcpHeaders headers = null;
            ArraySegment<byte>? payload = null;
            var disposition = FrameDisposition.Indeterminate;
            uint transitions = 0;
            while (true)
            {
                // If it looks like we have a bug and are looping forever, bail out of the state machine.
                if (transitions++ > maximumTransitions)
                {
                    return new ClassifyResult
                    {
                        Disposition = FrameDisposition.Indeterminate
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
                            Disposition = FrameDisposition.Indeterminate
                        };

                    default:
                        Log.Error("{0}.{1}: Unhandled state {2}. Dropping frame.",
                            nameof(TcpProtocol), nameof(Classify), state);
                        return new ClassifyResult
                        {
                            Disposition = FrameDisposition.Indeterminate
                        };
                }
            }
        }

        // Terminal states need to be inlined in Classify() so they can return, but everything else fits nicely in its
        // own function. These are internal for testing.

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
