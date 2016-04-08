// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    using System;
    using System.Collections.Generic;
    using System.IO;
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

    public class TcpConnection : Connection, IRequestResponseConnection
    {
        private TcpTransport m_parentTransport;

        TcpClient m_tcpClient;
        NetworkStream m_networkStream;

        ServiceHost m_serviceHost;

        object m_requestsLock;
        Dictionary<uint, TaskCompletionSource<IMessage>> m_outstandingRequests;

        long m_requestId;

        public TcpConnection(
            TcpTransport parentTransport,
            TcpClient tcpClient,
            ConnectionType connectionType)
            : this (
                  parentTransport,
                  tcpClient,
                  new ServiceHost(parentTransport),
                  connectionType)
        {
        }

        internal TcpConnection(
            TcpTransport parentTransport,
            TcpClient tcpClient,
            ServiceHost serviceHost,
            ConnectionType connectionType)
        {
            m_parentTransport = parentTransport;
            m_tcpClient = tcpClient;
            m_networkStream = tcpClient.GetStream();
            m_serviceHost = serviceHost;
            m_requestsLock = new object();
            m_outstandingRequests = new Dictionary<uint, TaskCompletionSource<IMessage>>();

            // start at -1 or 0 so the first request ID is 1 or 2.
            m_requestId = connectionType == ConnectionType.Client ? -1 : 0;
        }

        public override string ToString()
        {
            return $"{nameof(TcpConnection)}(local: {m_tcpClient.Client.LocalEndPoint}, remote: {m_tcpClient.Client.RemoteEndPoint})";
        }

        internal static Frame MessageToFrame(uint requestId, string methodName, PayloadType type, IMessage payload)
        {
            var frame = new Frame();

            {
                var tcpHeaders = new TcpHeaders
                {
                    request_id = requestId,
                    payload_type = type,
                    method_name = methodName ?? string.Empty, // method_name is not nullable
                };

                if (payload.IsError)
                {
                    tcpHeaders.error_code = payload.Error.Deserialize<Error>().error_code;
                }
                else
                {
                    tcpHeaders.error_code = (int)ErrorCode.OK;
                }

                var outputBuffer = new OutputBuffer(150);
                var fastWriter = new FastBinaryWriter<OutputBuffer>(outputBuffer);
                Serialize.To(fastWriter, tcpHeaders);

                frame.Add(new Framelet(FrameletType.TcpHeaders, outputBuffer.Data));
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

            try
            {
                using (var binWriter = new BinaryWriter(m_networkStream, encoding: Encoding.UTF8, leaveOpen: true))
                {
                    frame.Write(binWriter);
                    binWriter.Flush();
                }

                await m_networkStream.FlushAsync();
            }
            catch (IOException ex)
            {
                responseCompletionSource.TrySetException(ex);
            }

            Log.Debug("{0}.{1}: Sent request {2}/{3}.", this, nameof(SendRequestAsync), requestId, methodName);
            return await responseCompletionSource.Task;
        }

        internal async Task SendReplyAsync(uint requestId, IMessage response)
        {
            var frame = MessageToFrame(requestId, null, PayloadType.Response, response);

            Log.Debug("{0}.{1}: Sending reply for request ID {1}.", this, nameof(SendReplyAsync), requestId);
            using (var binWriter = new BinaryWriter(m_networkStream, encoding: Encoding.UTF8, leaveOpen: true))
            {
                frame.Write(binWriter);
                binWriter.Flush();
            }

            await m_networkStream.FlushAsync();
            Log.Debug("{0}.{1}: Sent reply for request ID {1}.", this, nameof(SendReplyAsync), requestId);
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
                throw new TcpProtocolErrorException("Exhausted request IDs");
            }

            return unchecked((UInt32)requestIdLong);
        }

        private async Task ProcessFramesAsync(NetworkStream stream)
        {
            // TODO: shutdown
            while (true)
            {
                var frame = await Frame.ReadAsync(stream);
                var result = TcpProtocol.Classify(frame);
                switch (result.Disposition)
                {
                    case FrameDisposition.DeliverRequestToService:
                        DispatchRequest(result.Headers, result.Payload);
                        break;

                    case FrameDisposition.DeliverResponseToProxy:
                        DispatchResponse(result.Headers, result.Payload);
                        break;

                    default:
                        var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Unsupported FrameDisposition",
                            this, nameof(ProcessFramesAsync), result.Disposition);
                        throw new NotImplementedException(message);
                }
            }
        }

        private void DispatchRequest(TcpHeaders headers, ArraySegment<byte> payload)
        {
            if (headers.error_code != (int)ErrorCode.OK)
            {
                throw new TcpProtocolErrorException("Received a request with non-zero error code. Request ID " + headers.request_id);
            }

            IMessage request = Message.FromPayload(Unmarshal.From(payload));

            Task.Run(async () =>
            {
                IMessage result = await m_serviceHost.DispatchRequest(headers.method_name, new TcpReceiveContext(this), request);
                await SendReplyAsync(headers.request_id, result);
            });
        }

        private void DispatchResponse(TcpHeaders headers, ArraySegment<byte> payload)
        {
            TaskCompletionSource<IMessage> responseCompletionSource;

            lock (m_requestsLock)
            {
                if (!m_outstandingRequests.TryGetValue(headers.request_id, out responseCompletionSource))
                {
                    Log.Error("{0}.{1}: Response for unmatched request {2}.",
                        this, nameof(DispatchResponse), headers.request_id);
                    throw new TcpProtocolErrorException($"{this}.{nameof(DispatchResponse)}: Response for unmatched request {headers.request_id}.");
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

        public override Task StopAsync()
        {
            Log.Debug("{0}.{1}: Shutting down.", this, nameof(StopAsync));
            m_tcpClient.Close();
            return TaskExt.CompletedTask;
        }

        public override void AddService<T>(T service)
        {
            // TODO: re-work when we use reflection to find service methods
            m_serviceHost.Register(service);
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public async Task<IMessage<TResponse>> RequestResponseAsync<TRequest, TResponse>(string methodName, IMessage<TRequest> message, CancellationToken ct)
        {
            // TODO: cancellation
            IMessage response = await SendRequestAsync(methodName, message);
            return response.Convert<TResponse>();
        }
    }
}
