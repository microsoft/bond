namespace Bond.Comm.Tcp
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Net.Sockets;
    using System.Threading;
    using System.Threading.Tasks;
    using System.Text;

    using Bond;
    using Bond.IO.Safe;
    using Bond.Protocols;
    
    public enum ConnectionType
    {
        Client,
        Server
    }

    public class TcpConnection : Connection, IRequestResponseConnection
    {
        TcpClient m_tcpClient;
        NetworkStream m_networkStream;
        ConnectionType m_connectionType;

        TcpServiceHost m_serviceHost;

        object m_requestsLock;
        Dictionary<uint, TaskCompletionSource<IBonded>> m_outstandingRequests;

        long m_requestId;

        public TcpConnection(TcpClient tcpClient, ConnectionType connectionType) : this (tcpClient, new TcpServiceHost(), connectionType)
        {
        }

        internal TcpConnection(TcpClient tcpClient, TcpServiceHost serviceHost, ConnectionType connectionType)
        {
            m_tcpClient = tcpClient;
            m_networkStream = tcpClient.GetStream();
            m_connectionType = connectionType;
            m_serviceHost = serviceHost;
            m_requestsLock = new object();
            m_outstandingRequests = new Dictionary<uint, TaskCompletionSource<IBonded>>();

            // start at -1 or 0 so the first request ID is 1 or 2.
            m_requestId = connectionType == ConnectionType.Client ? -1 : 0;
        }

        // TODO: make async for real
        internal async Task<IBonded> SendRequestAsync(string methodName, IBonded request)
        {
            uint requestId = AllocateNextRequestId();
            var frame = new Frame();

            {
                var tcpHeaders = new TcpHeaders
                {
                    request_id = requestId,
                    payload_type = PayloadType.Request,
                    method_name = methodName
                };

                var outputBuffer = new OutputBuffer(150);
                var fastWriter = new FastBinaryWriter<OutputBuffer>(outputBuffer);
                Serialize.To(fastWriter, tcpHeaders);

                frame.Add(new Framelet(FrameletType.TcpHeaders, outputBuffer.Data));
            }

            {
                var outputBuffer = new OutputBuffer(1024);
                var compactWriter = new CompactBinaryWriter<OutputBuffer>(outputBuffer);
                // TODO: marshal dies on IBonded Marshal.To(compactWriter, request)
                // understand more deeply why and consider fixing
                compactWriter.WriteVersion();
                request.Serialize(compactWriter);

                frame.Add(new Framelet(FrameletType.PayloadData, outputBuffer.Data));
            }

            var responseCompletionSource = new TaskCompletionSource<IBonded>();
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
            
            return await responseCompletionSource.Task;
        }

        internal async Task SendReplyAsync(uint requestId, IBonded response)
        {
            var frame = new Frame();

            {
                var tcpHeaders = new TcpHeaders
                {
                    request_id = requestId,
                    payload_type = PayloadType.Response,
                };

                var outputBuffer = new OutputBuffer(150);
                var fastWriter = new FastBinaryWriter<OutputBuffer>(outputBuffer);
                Serialize.To(fastWriter, tcpHeaders);

                frame.Add(new Framelet(FrameletType.TcpHeaders, outputBuffer.Data));
            }

            {
                var outputBuffer = new OutputBuffer(1024);
                var compactWriter = new CompactBinaryWriter<OutputBuffer>(outputBuffer);
                // TODO: see above about Marshal.
                compactWriter.WriteVersion();
                response.Serialize(compactWriter);

                frame.Add(new Framelet(FrameletType.PayloadData, outputBuffer.Data));
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
            catch (IOException)
            {
                // TODO: convert to an Error?
                throw;
            }
        }

        internal void Start()
        {
            Task.Run(() => ProcessFrameAsync(m_networkStream));
        }

        private UInt32 AllocateNextRequestId()
        {
            var requestIdLong = Interlocked.Add(ref m_requestId, 2);
            if (requestIdLong > UInt32.MaxValue)
            {
                throw new ProtocolErrorException("Exhausted request IDs");
            }

            return unchecked((UInt32)requestIdLong);
        }

        private async Task ProcessFrameAsync(NetworkStream stream)
        {
            // TODO: shutdown
            for (;;)
            {
                var frame = await Frame.ReadAsync(stream);

                var payload = default(ArraySegment<byte>);
                var headers = default(TcpHeaders);

                foreach(var framelet in frame.Framelets)
                {
                    switch (framelet.Type)
                    {
                        case FrameletType.TcpHeaders:
                            var inputBuffer = new InputBuffer(framelet.Contents);
                            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
                            headers = Deserialize<TcpHeaders>.From(fastBinaryReader);
                            break;

                        case FrameletType.PayloadData:
                            payload = framelet.Contents;
                            break;

                        default:
                            System.Diagnostics.Debug.Write("Ignoring frame of type " + framelet.Type);
                            break;
                    }
                }

                if (headers == null)
                {
                    throw new ProtocolErrorException("Missing headers");
                }
                else if (payload.Array == null)
                {
                    throw new ProtocolErrorException("Missing payload");
                }

                switch (headers.payload_type)
                {
                    case PayloadType.Request:
                        DispatchRequest(headers, payload);
                        break;

                    case PayloadType.Response:
                        DispatchResponse(headers, payload);
                        break;

                    case PayloadType.Event:
                    default:
                        throw new NotImplementedException(headers.payload_type.ToString());
                }
            }
        }

        private void DispatchRequest(TcpHeaders headers, ArraySegment<byte> payload)
        {
            var bondedRequest = Unmarshal.From(payload);
            m_serviceHost.DispatchRequest(headers, this, bondedRequest);
        }

        private void DispatchResponse(TcpHeaders headers, ArraySegment<byte> payload)
        {
            TaskCompletionSource<IBonded> responseCompletionSource;

            lock (m_requestsLock)
            {
                if (!m_outstandingRequests.TryGetValue(headers.request_id, out responseCompletionSource))
                {
                    // TODO: this should be something we just log
                    throw new ProtocolErrorException("Response for unmatched request " + headers.request_id);
                }

                m_outstandingRequests.Remove(headers.request_id);
            }

            var bondedResopnse = Unmarshal.From(payload);
            responseCompletionSource.SetResult(bondedResopnse);
        }

        public override Task StopAsync()
        {
            m_tcpClient.Close();
            return TaskExt.CompletedTask;
        }

        public override void AddService<T>(T server)
        {
            // TODO: re-work when we use reflection to find service methods
            m_serviceHost.Register((IService)server);
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public async Task<Message<TResponse>> RequestResponseAsync<TRequest, TResponse>(string methodName, Message<TRequest> message, CancellationToken ct)
        {
            // TODO: cancellation
            IBonded response = await SendRequestAsync(methodName, message.Payload);
            // TODO: handle error responses
            return new Message<TResponse>(response.Convert<TResponse>());
        }
    }
}
