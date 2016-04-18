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
            /// <summary>
            /// What <see cref="Classify"/> thinks its caller should do with the <see cref="Frame"/> it was given.
            /// </summary>
            public FrameDisposition Disposition;

            /// <summary>
            /// The <see cref="TcpHeaders"/> from the <see cref="Frame"/> given to
            /// <see cref="Classify"/>. If <c>Classify</c> was unable to find <c>TcpHeaders</c>, was unable to
            /// understand the <c>TcpHeaders</c> it found, or returned before it
            /// reached the <c>TcpHeaders</c>, this will be <c>null</c>.
            /// </summary>
            public TcpHeaders Headers;

            /// <summary>
            /// The payload from the <see cref="Frame"/> given to <see cref="Classify"/>. If <c>Classify</c> was unable
            /// to find a payload or returned before it reached the payload, <c>Payload.Array</c> will be <c>null</c>.
            /// </summary>
            public ArraySegment<byte> Payload;

            /// <summary>
            /// If the <see cref="Frame"/> given to <see cref="Classify"/> contained a <see cref="ProtocolError"/>,
            /// this will point to it. Otherwise, this will be null.
            /// </summary>
            public ProtocolError Error;

            /// <summary>
            /// If <see cref="Disposition"/> is <see cref="FrameDisposition.SendProtocolError"/>, this will contain the
            /// relevant <see cref="ProtocolErrorCode"/>. For all other dispositions, this will be <c>null</c>.
            /// </summary>
            public ProtocolErrorCode? ErrorCode;
        }

        /// <summary>
        /// States for the state machine inside <see cref="TcpProtocol.Classify"/>. <c>internal</c> for testing.
        /// </summary>
        internal enum ClassifyState
        {
            // These states are the happy path from no knowledge to proving a frame is good and knowing what to do with
            // its contents. Each is implemented in its own function below.

            // Do we have a frame at all?
            ExpectFrame,

            // Does the frame have at least one framelet? Is the first one valid?
            ExpectFirstFramelet,

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

            // This state is the happy path from believing the frame represents an error to proving the frame is good
            // and knowing what to do with that error.
            ExpectProtocolError,

            // Terminal states. Each of these indicates that we have classified the frame and need to return something.
            // Because these frames always mean the next step is to return, their implementations are inlined in
            // Classify().

            // There are no problems with this frame, and we know what to do with it. Return from Classify().
            ClassifiedValidFrame,

            // We could not interpret the frame, or it violated the protocol. Return from Classify().
            MalformedFrame,

            // We got an error, but couldn't interpret it.
            ErrorInErrorFrame,

            // We detected a bug in the state machine. Return from Classify() with a FrameDisposition of Indeterminate.
            // This should never happen.
            InternalStateError
        }

        // This needs to be larger than the longest valid path through the state machine.
        private static readonly uint maximumTransitions = (uint) Enum.GetNames(typeof(ClassifyState)).Length;

        // These values are hard-coded because the protocol, as implemented, does not permit any framelet sequences
        // other than [TcpHeaders, PayloadData] and [ProtocolError]. We will need to get rid of these indices and track
        // the index of the next unexamined Framelet once we implement Layers.
        private const int PayloadDataIndex = 1;
        private const int ValidFrameSize = 2;

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
            var payload = new ArraySegment<byte>();
            ProtocolError error = null;
            var disposition = FrameDisposition.Indeterminate;
            ProtocolErrorCode? errorCode = null;
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

                    case ClassifyState.ExpectFirstFramelet:
                        state = TransitionExpectFirstFramelet(state, frame, ref errorCode);
                        continue;

                    case ClassifyState.ExpectTcpHeaders:
                        state = TransitionExpectTcpHeaders(state, frame, ref headers, ref errorCode);
                        continue;

                    case ClassifyState.ExpectPayload:
                        state = TransitionExpectPayload(state, frame, headers, ref payload, ref errorCode);
                        continue;

                    case ClassifyState.ExpectEndOfFrame:
                        state = TransitionExpectEndOfFrame(state, frame, ref errorCode);
                        continue;

                    case ClassifyState.FrameComplete:
                        state = TransitionFrameComplete(state, headers, ref errorCode);
                        continue;

                    case ClassifyState.ValidFrame:
                        state = TransitionValidFrame(state, headers, ref disposition);
                        continue;

                    case ClassifyState.ExpectProtocolError:
                        state = TransitionExpectProtocolError(state, frame, ref error, ref disposition);
                        continue;

                    case ClassifyState.ClassifiedValidFrame:
                        if (disposition == FrameDisposition.Indeterminate)
                        {
                            state = ClassifyState.InternalStateError;
                            continue;
                        }

                        return new ClassifyResult
                        {
                            Disposition = disposition,
                            Headers = headers,
                            Payload = payload,
                            Error = error
                        };

                    case ClassifyState.MalformedFrame:
                        if (errorCode == null)
                        {
                            state = ClassifyState.InternalStateError;
                            continue;
                        }

                        return new ClassifyResult
                        {
                            Disposition = FrameDisposition.SendProtocolError,
                            ErrorCode = errorCode
                        };

                    case ClassifyState.ErrorInErrorFrame:
                        return new ClassifyResult
                        {
                            Disposition = FrameDisposition.HangUp,
                            Error = new ProtocolError
                            {
                                error_code = ProtocolErrorCode.ERROR_IN_ERROR
                            }
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
            state = ClassifyState.ExpectFirstFramelet;
            return state;
        }

        internal static ClassifyState TransitionExpectFirstFramelet(
            ClassifyState state, Frame frame, ref ProtocolErrorCode? errorCode)
        {
            if (state != ClassifyState.ExpectFirstFramelet || frame == null)
            {
                return ClassifyState.InternalStateError;
            }

            if (frame.Framelets.Count == 0)
            {
                Log.Error("{0}.{1}: Frame was empty.", nameof(TcpProtocol), nameof(TransitionExpectFirstFramelet));
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            switch (frame.Framelets[0].Type)
            {
                case FrameletType.TcpHeaders:
                    return ClassifyState.ExpectTcpHeaders;

                case FrameletType.ProtocolError:
                    return ClassifyState.ExpectProtocolError;

                default:
                    Log.Error("{0}.{1}: Frame began with invalid FrameletType {2}.",
                        nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders), frame.Framelets[0].Type);
                    errorCode = ProtocolErrorCode.MALFORMED_DATA;
                    return ClassifyState.MalformedFrame;
            }
        }

        internal static ClassifyState TransitionExpectTcpHeaders(
            ClassifyState state, Frame frame, ref TcpHeaders headers, ref ProtocolErrorCode? errorCode)
        {
            if (state != ClassifyState.ExpectTcpHeaders || frame == null || frame.Count == 0
                || frame.Framelets[0].Type != FrameletType.TcpHeaders)
            {
                return ClassifyState.InternalStateError;
            }

            var framelet = frame.Framelets[0];

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
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            Log.Debug("{0}.{1}: Extracted TcpHeaders with request ID {2} and payload type {3}.",
                nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders), headers.request_id, headers.payload_type);
            return ClassifyState.ExpectPayload;
        }

        internal static ClassifyState TransitionExpectPayload(
            ClassifyState state, Frame frame, TcpHeaders headers, ref ArraySegment<byte> payload,
            ref ProtocolErrorCode? errorCode)
        {
            if (state != ClassifyState.ExpectPayload || frame == null || headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            if (PayloadDataIndex >= frame.Count)
            {
                Log.Error("{0}.{1}: Frame did not continue with PayloadData.",
                    nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders));
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            var framelet = frame.Framelets[PayloadDataIndex];
            if (framelet.Type != FrameletType.PayloadData)
            {
                Log.Error("{0}.{1}: Frame did not continue with PayloadData.",
                    nameof(TcpProtocol), nameof(TransitionExpectTcpHeaders));
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            payload = framelet.Contents;
            Log.Debug("{0}.{1}: Extracted {2}-byte payload in request ID {3}.",
                nameof(TcpProtocol), nameof(TransitionExpectPayload), payload.Count, headers.request_id);
            return ClassifyState.ExpectEndOfFrame;
        }

        internal static ClassifyState TransitionExpectEndOfFrame(
            ClassifyState state, Frame frame, ref ProtocolErrorCode? errorCode)
        {
            if (state != ClassifyState.ExpectEndOfFrame || frame == null)
            {
                return ClassifyState.InternalStateError;
            }

            if (frame.Count == ValidFrameSize)
            {
                return ClassifyState.FrameComplete;
            }
            else
            {
                Log.Error("{0}.{1}: Frame had trailing framelets.",
                    nameof(TcpProtocol), nameof(TransitionExpectEndOfFrame));
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }
        }

        internal static ClassifyState TransitionFrameComplete(
            ClassifyState state, TcpHeaders headers, ref ProtocolErrorCode? errorCode)
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
                    errorCode = ProtocolErrorCode.NOT_SUPPORTED;
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
                    return ClassifyState.ClassifiedValidFrame;

                case PayloadType.Response:
                    disposition = FrameDisposition.DeliverResponseToProxy;
                    return ClassifyState.ClassifiedValidFrame;

                default:
                    return ClassifyState.InternalStateError;
            }
        }

        internal static ClassifyState TransitionExpectProtocolError(
            ClassifyState state, Frame frame, ref ProtocolError error, ref FrameDisposition disposition)
        {
            if (state != ClassifyState.ExpectProtocolError || frame == null || frame.Count == 0
                || frame.Framelets[0].Type != FrameletType.ProtocolError)
            {
                return ClassifyState.InternalStateError;
            }

            var framelet = frame.Framelets[0];

            var inputBuffer = new InputBuffer(framelet.Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            try
            {
                error = Deserialize<ProtocolError>.From(fastBinaryReader);
            }
            catch (Exception ex) when (ex is InvalidDataException || ex is IOException)
            {
                Log.Error(ex, "{0}.{1}: Failed to parse ProtocolError: {2}.",
                    nameof(TcpProtocol), nameof(TransitionExpectProtocolError), ex.Message);
                return ClassifyState.ErrorInErrorFrame;
            }

            Log.Debug("{0}.{1}: Extracted ProtocolError with code {2}.",
                nameof(TcpProtocol), nameof(TransitionExpectProtocolError), error.error_code);
            disposition = FrameDisposition.HangUp;
            return ClassifyState.ClassifiedValidFrame;
        }
    }
}
