// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Diagnostics;
    using Bond.IO.Safe;
    using Bond.Protocols;

    internal static class EpoxyProtocol
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
            /// The frame was a valid Event.
            /// </summary>
            DeliverEventToService,

            /// <summary>
            /// The frame is a valid Config frame and needs to be handled.
            /// </summary>
            ProcessConfig,

            /// <summary>
            /// The frame is a valid ProtocolError frame and needs to be handled.
            /// </summary>
            HandleProtocolError,

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
        /// Struct to hold message data and indicate whether it is an Error or not.
        /// </summary>
        internal struct MessageData
        {
            public MessageData(bool isError, ArraySegment<byte> data)
            {
                IsError = isError;
                Data = data;
            }

            public bool IsError { get; }
            public ArraySegment<byte> Data { get; }
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
            /// The <see cref="EpoxyHeaders"/> from the <see cref="Frame"/> given to
            /// <see cref="Classify"/>. If <c>Classify</c> was unable to find <c>EpoxyHeaders</c>, was unable to
            /// understand the <c>EpoxyHeaders</c> it found, or returned before it
            /// reached the <c>EpoxyHeaders</c>, this will be <c>null</c>.
            /// </summary>
            public EpoxyHeaders Headers;

            /// <summary>
            /// The layer data from the <see cref="Frame"/> given to <see cref="Classify"/>. If <c>Classify</c> was unable
            /// to find layer data or returned before it reached the layer data, <c>LayerData.Array</c> will be <c>null</c>.
            /// </summary>
            public ArraySegment<byte> LayerData;

            /// <summary>
            /// The representation of the message from the <see cref="Frame"/> given to
            /// <see cref="Classify"/>. If <c>Classify</c> was unable to find a message or returned
            /// before it reached the message, <c>MessageData.Data.Array</c> will be <c>null</c>.
            /// </summary>
            public MessageData MessageData;

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
        /// States for the state machine inside <see cref="EpoxyProtocol.Classify"/>. <c>internal</c> for testing.
        /// </summary>
        internal enum ClassifyState
        {
            // These states are the happy path from no knowledge to proving a frame is good and knowing what to do with
            // its contents. Each is implemented in its own function below.

            // Do we have a frame at all?
            ExpectFrame,

            // Does the frame have at least one framelet? Is the first one valid?
            ExpectFirstFramelet,

            // Does the frame begin with a valid EpoxyHeaders?
            ExpectEpoxyHeaders,

            // Does the frame have (optional) LayerData after the EpoxyHeaders?
            ExpectOptionalLayerData,

            // Does the frame have message data immediately after the EpoxyHeaders (or LayerData if
            // it was present)?
            ExpectMessageData,

            // The frame has all required framelets. Does it have any trailing ones?
            ExpectEndOfFrame,

            // Do we know what to do with frames with this EpoxyMessageType?
            FrameComplete,

            // There are no problems with this frame. What should we do with it?
            ValidFrame,

            // This state is the happy path from believing the frame represents config to proving the frame is good
            // and knowing what to do with that config.
            ExpectConfig,

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

        private static readonly Deserializer<FastBinaryReader<InputBuffer>> configDeserializer =
            new Deserializer<FastBinaryReader<InputBuffer>>(typeof(EpoxyConfig));
        private static readonly Deserializer<FastBinaryReader<InputBuffer>> headersDeserializer =
            new Deserializer<FastBinaryReader<InputBuffer>>(typeof(EpoxyHeaders));
        private static readonly Deserializer<FastBinaryReader<InputBuffer>> errorDeserializer =
            new Deserializer<FastBinaryReader<InputBuffer>>(typeof(ProtocolError));

        internal static ClassifyResult Classify(Frame frame, Logger logger)
        {
            if (frame == null)
            {
                return new ClassifyResult
                {
                    Disposition = FrameDisposition.Indeterminate
                };
            }

            logger.Site().Debug("Processing {0} framelets.", frame.Count);

            var state = ClassifyState.ExpectFirstFramelet;
            var disposition = FrameDisposition.Indeterminate;
            EpoxyHeaders headers = null;
            var layerData = new ArraySegment<byte>();
            var messageData = default(MessageData);
            ProtocolError error = null;
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
                    case ClassifyState.ExpectFirstFramelet:
                        state = TransitionExpectFirstFramelet(state, frame, ref errorCode, logger);
                        continue;

                    case ClassifyState.ExpectEpoxyHeaders:
                        state = TransitionExpectEpoxyHeaders(state, frame, ref headers, ref errorCode, logger);
                        continue;

                    case ClassifyState.ExpectOptionalLayerData:
                        state = TransitionExpectOptionalLayerData(state, frame, headers, ref layerData, ref errorCode, logger);
                        continue;

                    case ClassifyState.ExpectMessageData:
                        state = TransitionExpectMessageData(state, frame, headers, layerData, ref messageData, ref errorCode, logger);
                        continue;

                    case ClassifyState.ExpectEndOfFrame:
                        state = TransitionExpectEndOfFrame(state, frame, layerData, ref errorCode, logger);
                        continue;

                    case ClassifyState.FrameComplete:
                        state = TransitionFrameComplete(state, headers, ref errorCode, logger);
                        continue;

                    case ClassifyState.ValidFrame:
                        state = TransitionValidFrame(state, headers, ref disposition);
                        continue;

                    case ClassifyState.ExpectConfig:
                        state = TransitionExpectConfig(state, frame, ref errorCode, ref disposition, logger);
                        continue;

                    case ClassifyState.ExpectProtocolError:
                        state = TransitionExpectProtocolError(state, frame, ref error, ref disposition, logger);
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
                            LayerData = layerData,
                            MessageData = messageData,
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
                        logger.Site().Error("Unhandled state {0}. Dropping frame.", state);
                        return new ClassifyResult
                        {
                            Disposition = FrameDisposition.Indeterminate
                        };
                }
            }
        }

        internal static ClassifyState TransitionExpectFirstFramelet(
            ClassifyState state, Frame frame, ref ProtocolErrorCode? errorCode, Logger logger)
        {
            Debug.Assert(state == ClassifyState.ExpectFirstFramelet);
            Debug.Assert(frame != null);

            if (frame.Framelets.Count == 0)
            {
                logger.Site().Error("Frame was empty.");
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            switch (frame.Framelets[0].Type)
            {
                case FrameletType.EpoxyHeaders:
                    return ClassifyState.ExpectEpoxyHeaders;

                case FrameletType.EpoxyConfig:
                    return ClassifyState.ExpectConfig;

                case FrameletType.ProtocolError:
                    return ClassifyState.ExpectProtocolError;

                default:
                    logger.Site().Error("Frame began with invalid FrameletType {0}.", frame.Framelets[0].Type);
                    errorCode = ProtocolErrorCode.MALFORMED_DATA;
                    return ClassifyState.MalformedFrame;
            }
        }

        internal static ClassifyState TransitionExpectEpoxyHeaders(
            ClassifyState state, Frame frame, ref EpoxyHeaders headers, ref ProtocolErrorCode? errorCode, Logger logger)
        {
            Debug.Assert(state == ClassifyState.ExpectEpoxyHeaders);
            Debug.Assert(frame != null);

            if (frame.Count == 0 || frame.Framelets[0].Type != FrameletType.EpoxyHeaders)
            {
                return ClassifyState.InternalStateError;
            }

            var framelet = frame.Framelets[0];
            var inputBuffer = new InputBuffer(framelet.Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            switch (headersDeserializer.TryDeserialize(fastBinaryReader, out headers))
            {
                case Deserialize.Result.Success:
                    break;

                default:
                    logger.Site().Error("Didn't get a valid {0}.", nameof(EpoxyHeaders));
                    errorCode = ProtocolErrorCode.MALFORMED_DATA;
                    return ClassifyState.MalformedFrame;
            }

            logger.Site().Debug("Deserialized {0} with conversation ID {1} and message type {2}.",
                nameof(EpoxyHeaders), headers.conversation_id, headers.message_type);
            return ClassifyState.ExpectOptionalLayerData;
        }

        internal static ClassifyState TransitionExpectOptionalLayerData(
            ClassifyState state, Frame frame, EpoxyHeaders headers, ref ArraySegment<byte> layerData,
            ref ProtocolErrorCode? errorCode, Logger logger)
        {
            Debug.Assert(state == ClassifyState.ExpectOptionalLayerData);
            Debug.Assert(frame != null);

            if (headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            if (frame.Count < 2)
            {
                logger.Site().Error("Frame had headers but no message data.");
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            var framelet = frame.Framelets[1];
            if (framelet.Type == FrameletType.LayerData)
            {
                layerData = framelet.Contents;
                logger.Site().Debug("Extracted {0}-byte layer data in conversation ID {1}.",
                    layerData.Count, headers.conversation_id);
            }

            return ClassifyState.ExpectMessageData;
        }

        internal static ClassifyState TransitionExpectMessageData(
            ClassifyState state, Frame frame, EpoxyHeaders headers, ArraySegment<byte> layerData,
            ref MessageData messageData, ref ProtocolErrorCode? errorCode, Logger logger)
        {
            Debug.Assert(state == ClassifyState.ExpectMessageData);
            Debug.Assert(frame != null);

            if (headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            int messageDataIndex = (layerData.Array == null ? 1 : 2);
            if (messageDataIndex >= frame.Count)
            {
                logger.Site().Error("Frame had headers but no message data.");
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            var framelet = frame.Framelets[messageDataIndex];
            if (framelet.Type != FrameletType.PayloadData && framelet.Type != FrameletType.ErrorData)
            {
                logger.Site().Error("Frame had headers but no message data. Unexpected framelet type {0}", (int)framelet.Type);
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            messageData = new MessageData(
                isError: framelet.Type == FrameletType.ErrorData,
                data: framelet.Contents);

            logger.Site().Debug("Extracted {0}-byte message in conversation ID {1}.",
                messageData.Data.Count, headers.conversation_id);
            return ClassifyState.ExpectEndOfFrame;
        }

        internal static ClassifyState TransitionExpectEndOfFrame(
            ClassifyState state, Frame frame, ArraySegment<byte> layerData, ref ProtocolErrorCode? errorCode,
            Logger logger)
        {
            // FIXME: Change all of these to asserts.
            if (state != ClassifyState.ExpectEndOfFrame || frame == null)
            {
                return ClassifyState.InternalStateError;
            }

            var validFrameSize = (layerData.Array == null ? 2 : 3);
            if (frame.Count == validFrameSize)
            {
                return ClassifyState.FrameComplete;
            }
            else
            {
                logger.Site().Error("Frame had trailing framelets.");
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }
        }

        internal static ClassifyState TransitionFrameComplete(
            ClassifyState state, EpoxyHeaders headers, ref ProtocolErrorCode? errorCode, Logger logger)
        {
            if (state != ClassifyState.FrameComplete || headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            switch (headers.message_type)
            {
                case EpoxyMessageType.REQUEST:
                case EpoxyMessageType.RESPONSE:
                case EpoxyMessageType.EVENT:
                    return ClassifyState.ValidFrame;

                default:
                    logger.Site().Warning("Received unrecognized message type {0}.", headers.message_type);
                    errorCode = ProtocolErrorCode.NOT_SUPPORTED;
                    return ClassifyState.MalformedFrame;
            }
        }

        internal static ClassifyState TransitionValidFrame(
            ClassifyState state, EpoxyHeaders headers, ref FrameDisposition disposition)
        {
            if (state != ClassifyState.ValidFrame || headers == null)
            {
                return ClassifyState.InternalStateError;
            }

            switch (headers.message_type)
            {
                case EpoxyMessageType.REQUEST:
                    disposition = FrameDisposition.DeliverRequestToService;
                    return ClassifyState.ClassifiedValidFrame;

                case EpoxyMessageType.RESPONSE:
                    disposition = FrameDisposition.DeliverResponseToProxy;
                    return ClassifyState.ClassifiedValidFrame;

                case EpoxyMessageType.EVENT:
                    disposition = FrameDisposition.DeliverEventToService;
                    return ClassifyState.ClassifiedValidFrame;

                default:
                    return ClassifyState.InternalStateError;
            }
        }

        internal static ClassifyState TransitionExpectConfig(
            ClassifyState state, Frame frame, ref ProtocolErrorCode? errorCode, ref FrameDisposition disposition,
            Logger logger)
        {
            Debug.Assert(state == ClassifyState.ExpectConfig);
            Debug.Assert(frame != null);

            if (frame.Count == 0 || frame.Framelets[0].Type != FrameletType.EpoxyConfig)
            {
                return ClassifyState.InternalStateError;
            }

            if (frame.Count != 1)
            {
                logger.Site().Error("Config frame had trailing framelets.");
                errorCode = ProtocolErrorCode.MALFORMED_DATA;
                return ClassifyState.MalformedFrame;
            }

            var framelet = frame.Framelets[0];

            var inputBuffer = new InputBuffer(framelet.Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);

            // We don't currently do anything with the config aside from try to deserialize it.
            EpoxyConfig config;
            switch (configDeserializer.TryDeserialize(fastBinaryReader, out config))
            {
                case Deserialize.Result.Success:
                    break;

                default:
                    logger.Site().Error("Didn't get a valid {0}.", nameof(EpoxyConfig));
                    errorCode = ProtocolErrorCode.MALFORMED_DATA;
                    return ClassifyState.MalformedFrame;
            }

            disposition = FrameDisposition.ProcessConfig;
            return ClassifyState.ClassifiedValidFrame;
        }

        internal static ClassifyState TransitionExpectProtocolError(
            ClassifyState state, Frame frame, ref ProtocolError error, ref FrameDisposition disposition, Logger logger)
        {
            if (state != ClassifyState.ExpectProtocolError || frame == null || frame.Count == 0
                || frame.Framelets[0].Type != FrameletType.ProtocolError)
            {
                return ClassifyState.InternalStateError;
            }

            if (frame.Count > 1)
            {
                logger.Site().Error("Protocol error frame had trailing framelets.");
                return ClassifyState.ErrorInErrorFrame;
            }

            var framelet = frame.Framelets[0];

            var inputBuffer = new InputBuffer(framelet.Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            switch (errorDeserializer.TryDeserialize(fastBinaryReader, out error))
            {
                case Deserialize.Result.Success:
                    break;

                default:
                    logger.Site().Error("Didn't get a valid {0}.", nameof(ProtocolError));
                    return ClassifyState.ErrorInErrorFrame;
            }

            logger.Site().Debug("Deserialized {0} with code {1}.", nameof(ProtocolError), error.error_code);
            disposition = FrameDisposition.HandleProtocolError;
            return ClassifyState.ClassifiedValidFrame;
        }
    }
}
