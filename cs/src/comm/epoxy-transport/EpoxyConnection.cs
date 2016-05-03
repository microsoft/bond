// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Net;
    using System.Net.Sockets;
    using System.Text;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Service;
    using Bond.IO.Safe;
    using Bond.Protocols;

    public enum ConnectionType
    {
        Client,
        Server
    }

    public class EpoxyConnection : Connection, IRequestResponseConnection, IEventConnection
    {
        private readonly EpoxyTransport m_parentTransport;
        private readonly Socket m_socket;
        private readonly Stream m_networkStream;

        ServiceHost m_serviceHost;

        object m_requestsLock;
        Dictionary<uint, TaskCompletionSource<IMessage>> m_outstandingRequests;

        long m_requestId;

        internal EpoxyConnection(
            EpoxyTransport parentTransport,
            Socket socket,
            ConnectionType connectionType)
            : this (
                  parentTransport,
                  socket,
                  new ServiceHost(parentTransport),
                  connectionType)
        {
        }

        internal EpoxyConnection(
            EpoxyTransport parentTransport,
            Socket socket,
            ServiceHost serviceHost,
            ConnectionType connectionType)
        {
            m_parentTransport = parentTransport;
            m_socket = socket;
            m_networkStream = new NetworkStream(socket, ownsSocket: false);

            m_serviceHost = serviceHost;
            m_requestsLock = new object();
            m_outstandingRequests = new Dictionary<uint, TaskCompletionSource<IMessage>>();

            // start at -1 or 0 so the first request ID is 1 or 2.
            m_requestId = connectionType == ConnectionType.Client ? -1 : 0;
        }

        /// <summary>
        /// Get this connection's local endpoint.
        /// </summary>
        public IPEndPoint LocalEndPoint => (IPEndPoint) m_socket.LocalEndPoint;

        /// <summary>
        /// Get this connection's remote endpoint.
        /// </summary>
        public IPEndPoint RemoteEndPoint => (IPEndPoint) m_socket.RemoteEndPoint;

        public override string ToString()
        {
            return $"{nameof(EpoxyConnection)}(local: {LocalEndPoint}, remote: {RemoteEndPoint})";
        }

        internal static Frame MessageToFrame(uint requestId, string methodName, PayloadType type, IMessage payload)
        {
            var frame = new Frame();

            {
                var headers = new EpoxyHeaders
                {
                    request_id = requestId,
                    payload_type = type,
                    method_name = methodName ?? string.Empty, // method_name is not nullable
                };

                if (payload.IsError)
                {
                    headers.error_code = payload.Error.Deserialize<Error>().error_code;
                }
                else
                {
                    headers.error_code = (int)ErrorCode.OK;
                }

                var outputBuffer = new OutputBuffer(150);
                var fastWriter = new FastBinaryWriter<OutputBuffer>(outputBuffer);
                Serialize.To(fastWriter, headers);

                frame.Add(new Framelet(FrameletType.EpoxyHeaders, outputBuffer.Data));
            }

            {
                var userData = payload.IsError ? (IBonded)payload.Error : (IBonded)payload.RawPayload;

                var outputBuffer = new OutputBuffer(1024);
                var compactWriter = new CompactBinaryWriter<OutputBuffer>(outputBuffer);
                // TODO: marshal dies on IBonded Marshal.To(compactWriter, request)
                // understand more deeply why and consider fixing
                compactWriter.WriteVersion();
                userData.Serialize(compactWriter);

                frame.Add(new Framelet(FrameletType.PayloadData, outputBuffer.Data));
            }

            return frame;
        }

        internal static Frame MakeProtocolErrorFrame(ProtocolErrorCode errorCode, Error details)
        {
            var protocolError = new ProtocolError
            {
                error_code = errorCode,
                details = details == null ? null : new Bonded<Error>(details)
            };

            var outputBuffer = new OutputBuffer(16);
            var fastWriter = new FastBinaryWriter<OutputBuffer>(outputBuffer);
            Serialize.To(fastWriter, protocolError);

            var frame = new Frame(1);
            frame.Add(new Framelet(FrameletType.ProtocolError, outputBuffer.Data));
            return frame;
        }

        // TODO: make async for real
        internal async Task<IMessage> SendRequestAsync<TPayload>(string methodName, IMessage<TPayload> request)
        {
            uint requestId = AllocateNextRequestId();
            var frame = MessageToFrame(requestId, methodName, PayloadType.Request, request);

            Log.Debug("{0}.{1}: Sending request {2}/{3}.", this, nameof(SendRequestAsync), requestId, methodName);
            var responseCompletionSource = new TaskCompletionSource<IMessage>();
            lock (m_requestsLock)
            {
                m_outstandingRequests.Add(requestId, responseCompletionSource);
            }

            await SendFrameAsync(frame, responseCompletionSource);
            Log.Debug("{0}.{1}: Sent request {2}/{3}.", this, nameof(SendRequestAsync), requestId, methodName);
            return await responseCompletionSource.Task;
        }

        internal async Task SendReplyAsync(uint requestId, IMessage response)
        {
            var frame = MessageToFrame(requestId, null, PayloadType.Response, response);
            Log.Debug("{0}.{1}: Sending reply for request ID {2}.", this, nameof(SendReplyAsync), requestId);

            await SendFrameAsync(frame);
            Log.Debug("{0}.{1}: Sent reply for request ID {2}.", this, nameof(SendReplyAsync), requestId);
        }

        internal async Task SendProtocolErrorAsync(ProtocolErrorCode errorCode, Error details = null)
        {
            var frame = MakeProtocolErrorFrame(errorCode, details);
            Log.Debug("{0}.{1}: Sending protocol error with code {2} and details {3}.",
                this, nameof(SendProtocolErrorAsync), errorCode, details == null ? "<null>" : details.error_code + details.message);

            await SendFrameAsync(frame);
            Log.Debug("{0}.{1}: Sent protocol error with code {2}.", this, nameof(SendProtocolErrorAsync), errorCode);
        }

        internal async Task SendFrameAsync(Frame frame, TaskCompletionSource<IMessage> responseCompletionSource = null)
        {
            try
            {
                using (var binWriter = new BinaryWriter(m_networkStream, encoding: Encoding.UTF8, leaveOpen: true))
                {
                    lock (m_networkStream)
                    {
                        frame.Write(binWriter);
                        binWriter.Flush();
                    }
                }

                await m_networkStream.FlushAsync();
            }
            catch (IOException ex)
            {
                Log.Error(ex, "{0}.{1}: While writing a Frame to the network: {2}", this, nameof(SendFrameAsync),
                    ex.Message);
                responseCompletionSource?.TrySetException(ex);
            }
        }

        internal async Task SendEventAsync(string methodName, IMessage message)
        {
            uint requestId = AllocateNextRequestId();
            var frame = MessageToFrame(requestId, methodName, PayloadType.Event, message);

            Log.Debug("{0}.{1}: Sending event {2}/{3}.", this, nameof(SendEventAsync), requestId, methodName);
            using (var binWriter = new BinaryWriter(m_networkStream, encoding: Encoding.UTF8, leaveOpen: true))
            {
                lock (m_networkStream)
                {
                    frame.Write(binWriter);
                    binWriter.Flush();
                }
            }

            await m_networkStream.FlushAsync();
            Log.Debug("{0}.{1}: Sent event {2}/{3}.", this, nameof(SendEventAsync), requestId, methodName);
        }

        internal void Start()
        {
            Task.Run(() => ProcessFramesAsync(m_networkStream));
        }

        private UInt32 AllocateNextRequestId()
        {
            var requestIdLong = Interlocked.Add(ref m_requestId, 2);
            if (requestIdLong > UInt32.MaxValue)
            {
                throw new EpoxyProtocolErrorException("Exhausted request IDs");
            }

            return unchecked((UInt32)requestIdLong);
        }

        private async Task ProcessFramesAsync(Stream stream)
        {
            // TODO: shutdown
            while (true)
            {
                var frame = await Frame.ReadAsync(stream);
                var result = EpoxyProtocol.Classify(frame);
                switch (result.Disposition)
                {
                    case EpoxyProtocol.FrameDisposition.DeliverRequestToService:
                        DispatchRequest(result.Headers, result.Payload);
                        break;

                    case EpoxyProtocol.FrameDisposition.DeliverResponseToProxy:
                        DispatchResponse(result.Headers, result.Payload);
                        break;

                    case EpoxyProtocol.FrameDisposition.DeliverEventToService:
                        DispatchEvent(result.Headers, result.Payload);
                        break;

                    case EpoxyProtocol.FrameDisposition.SendProtocolError:
                        await SendProtocolErrorAsync(result.ErrorCode ?? ProtocolErrorCode.INTERNAL_ERROR);
                        break;

                    default:
                        var message = LogUtil.ErrorAndReturnFormatted("{0}.{1}: Unsupported FrameDisposition {2}",
                            this, nameof(ProcessFramesAsync), result.Disposition);
                        throw new NotImplementedException(message);
                }
            }
        }

        private void DispatchRequest(EpoxyHeaders headers, ArraySegment<byte> payload)
        {
            if (headers.error_code != (int)ErrorCode.OK)
            {
                throw new EpoxyProtocolErrorException("Received a request with non-zero error code. Request ID " + headers.request_id);
            }

            IMessage request = Message.FromPayload(Unmarshal.From(payload));

            Task.Run(async () =>
            {
                IMessage result = await m_serviceHost.DispatchRequest(headers.method_name, new EpoxyReceiveContext(this), request);
                await SendReplyAsync(headers.request_id, result);
            });
        }

        private void DispatchResponse(EpoxyHeaders headers, ArraySegment<byte> payload)
        {
            TaskCompletionSource<IMessage> responseCompletionSource;

            lock (m_requestsLock)
            {
                if (!m_outstandingRequests.TryGetValue(headers.request_id, out responseCompletionSource))
                {
                    Log.Error("{0}.{1}: Response for unmatched request {2}.",
                        this, nameof(DispatchResponse), headers.request_id);
                    throw new EpoxyProtocolErrorException($"{this}.{nameof(DispatchResponse)}: Response for unmatched request {headers.request_id}.");
                }

                m_outstandingRequests.Remove(headers.request_id);
            }

            IMessage response;
            if (headers.error_code != (int) ErrorCode.OK)
            {
                response = Message.FromError(Unmarshal<Error>.From(payload));
            }
            else
            {
                response = Message.FromPayload(Unmarshal.From(payload));
            }

            responseCompletionSource.SetResult(response);
        }

        private void DispatchEvent(EpoxyHeaders headers, ArraySegment<byte> payload)
        {
            if (headers.error_code != (int)ErrorCode.OK)
            {
                throw new EpoxyProtocolErrorException("Received a request with non-zero error code. Request ID " + headers.request_id);
            }

            IMessage request = Message.FromPayload(Unmarshal.From(payload));

            Task.Run(async () =>
            {
                await m_serviceHost.DispatchEvent(headers.method_name, new EpoxyReceiveContext(this), request);
            });
        }

        public override Task StopAsync()
        {
            Log.Debug("{0}.{1}: Shutting down.", this, nameof(StopAsync));
            m_networkStream.Close();
            m_socket.Close();
            return TaskExt.CompletedTask;
        }

        public async Task<IMessage<TResponse>> RequestResponseAsync<TRequest, TResponse>(string methodName, IMessage<TRequest> message, CancellationToken ct)
        {
            // TODO: cancellation
            IMessage response = await SendRequestAsync(methodName, message);
            return response.Convert<TResponse>();
        }

        public Task FireEventAsync<TPayload>(string methodName, IMessage<TPayload> message)
        {
            return SendEventAsync(methodName, message);
        }
    }
}
