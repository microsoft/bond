// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.IO;
    using System.Net;
    using System.Net.Sockets;
    using System.Runtime.CompilerServices;
    using System.Text;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Service;
    using Bond.IO.Safe;
    using Bond.Protocols;

    public class EpoxyConnection : Connection, IRequestResponseConnection, IEventConnection
    {
        private enum ConnectionType
        {
            Client,
            Server
        }

        [Flags]
        private enum State
        {
            None = 0,
            Created = 0x01,
            Connected = 0x02,
            SendProtocolError = 0x04,
            Disconnecting = 0x08,
            Disconnected = 0x10,
            All = Created | Connected | SendProtocolError | Disconnecting | Disconnected,
        }

        readonly ConnectionType m_connectionType;

        readonly EpoxyTransport m_parentTransport;
        readonly EpoxyListener m_parentListener;
        readonly ServiceHost m_serviceHost;

        readonly EpoxySocket netSocket;

        readonly object m_requestsLock;
        readonly Dictionary<ulong, TaskCompletionSource<IMessage>> m_outstandingRequests;

        State m_state;
        readonly TaskCompletionSource<bool> m_startTask;
        readonly TaskCompletionSource<bool> m_stopTask;
        readonly CancellationTokenSource m_shutdownTokenSource;

        long prevConversationId;

        ProtocolErrorCode m_protocolError;
        Error m_errorDetails;

        readonly private ConnectionMetrics connectionMetrics = new ConnectionMetrics();
        private Stopwatch duration;

        private EpoxyConnection(
            ConnectionType connectionType,
            EpoxyTransport parentTransport,
            EpoxyListener parentListener,
            ServiceHost serviceHost,
            Socket socket)
        {
            Debug.Assert(parentTransport != null);
            Debug.Assert(connectionType != ConnectionType.Server || parentListener != null, "Server connections must have a listener");
            Debug.Assert(serviceHost != null);
            Debug.Assert(socket != null);

            m_connectionType = connectionType;

            m_parentTransport = parentTransport;
            m_parentListener = parentListener;
            m_serviceHost = serviceHost;

            netSocket = new EpoxySocket(socket);

            // cache these so we can use them after the socket has been shutdown
            LocalEndPoint = (IPEndPoint) socket.LocalEndPoint;
            RemoteEndPoint = (IPEndPoint) socket.RemoteEndPoint;

            m_requestsLock = new object();
            m_outstandingRequests = new Dictionary<ulong, TaskCompletionSource<IMessage>>();

            m_state = State.Created;
            m_startTask = new TaskCompletionSource<bool>();
            m_stopTask = new TaskCompletionSource<bool>();
            m_shutdownTokenSource = new CancellationTokenSource();

            // start at -1 or 0 so the first conversation ID is 1 or 2.
            prevConversationId = (connectionType == ConnectionType.Client) ? -1 : 0;

            connectionMetrics.connection_id = Guid.NewGuid().ToString();
            connectionMetrics.local_endpoint = LocalEndPoint.ToString();
            connectionMetrics.remote_endpoint = RemoteEndPoint.ToString();
        }

        internal static EpoxyConnection MakeClientConnection(
            EpoxyTransport parentTransport,
            Socket clientSocket)
        {
            const EpoxyListener parentListener = null;

            return new EpoxyConnection(
                ConnectionType.Client,
                parentTransport,
                parentListener,
                new ServiceHost(parentTransport),
                clientSocket);
        }

        internal static EpoxyConnection MakeServerConnection(
            EpoxyTransport parentTransport,
            EpoxyListener parentListener,
            ServiceHost serviceHost,
            Socket socket)
        {
            return new EpoxyConnection(
                ConnectionType.Server,
                parentTransport,
                parentListener,
                serviceHost,
                socket);
        }

        /// <summary>
        /// Get this connection's local endpoint.
        /// </summary>
        public IPEndPoint LocalEndPoint { get; private set; }

        /// <summary>
        /// Get this connection's remote endpoint.
        /// </summary>
        public IPEndPoint RemoteEndPoint { get; private set; }

        public override string ToString()
        {
            return $"{nameof(EpoxyConnection)}(local: {LocalEndPoint}, remote: {RemoteEndPoint})";
        }

        internal static Frame MessageToFrame(ulong conversationId, string methodName, PayloadType type, IMessage payload, IBonded layerData)
        {
            var frame = new Frame();

            {
                var headers = new EpoxyHeaders
                {
                    conversation_id = conversationId,
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

            if (layerData != null)
            {
                var outputBuffer = new OutputBuffer(150);
                var compactWriter = new CompactBinaryWriter<OutputBuffer>(outputBuffer);
                // TODO: See TODO below about issues with IBonded Marshal.TO(...)
                compactWriter.WriteVersion();
                layerData.Serialize(compactWriter);
                frame.Add(new Framelet(FrameletType.LayerData, outputBuffer.Data));
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

        private async Task<IMessage> SendRequestAsync<TPayload>(string methodName, IMessage<TPayload> request)
        {
            var conversationId = AllocateNextConversationId();

            var sendContext = new EpoxySendContext(this);
            IBonded layerData;
            Error layerError = ProcessLayerStackOnSend(MessageType.Request, sendContext, out layerData);

            if (layerError != null)
            {
                Log.Debug("{0}.{1}: Sending request {2}/{3} failed due to layer error (Code: {4}, Message: {5}).",
                            this, nameof(SendRequestAsync), conversationId, methodName, layerError.error_code,
                            layerError.message);
                return Message.FromError(layerError);
            }

            var frame = MessageToFrame(conversationId, methodName, PayloadType.Request, request, layerData);

            Log.Debug("{0}.{1}: Sending request {2}/{3}.", this, nameof(SendRequestAsync), conversationId, methodName);
            var responseCompletionSource = new TaskCompletionSource<IMessage>();
            lock (m_requestsLock)
            {
                m_outstandingRequests.Add(conversationId, responseCompletionSource);
            }

            await SendFrameAsync(frame, responseCompletionSource);
            Log.Debug("{0}.{1}: Sent request {2}/{3}.", this, nameof(SendRequestAsync), conversationId, methodName);
            return await responseCompletionSource.Task;
        }

        private async Task SendReplyAsync(ulong conversationId, IMessage response)
        {
            var sendContext = new EpoxySendContext(this);
            IBonded layerData;
            Error layerError = ProcessLayerStackOnSend(MessageType.Response, sendContext, out layerData);

            // If there was a layer error, replace the response with the layer error
            if (layerError != null)
            {
                Log.Debug("{0}.{1}: Sending reply for conversation ID {2} failed due to layer error (Code: {3}, Message: {4}).",
                            this, nameof(SendReplyAsync), conversationId, layerError.error_code, layerError.message);
                response = Message.FromError(layerError);
            }

            var frame = MessageToFrame(conversationId, null, PayloadType.Response, response, layerData);
            Log.Debug("{0}.{1}: Sending reply for conversation ID {2}.", this, nameof(SendReplyAsync), conversationId);

            await SendFrameAsync(frame);
            Log.Debug("{0}.{1}: Sent reply for conversation ID {2}.", this, nameof(SendReplyAsync), conversationId);
        }

        private async Task SendFrameAsync(Frame frame, TaskCompletionSource<IMessage> responseCompletionSource = null)
        {
            try
            {
                Stream networkStream = netSocket.NetworkStream;

                await netSocket.WriteLock.WaitAsync();
                try
                {
                    await frame.WriteAsync(networkStream);
                }
                finally
                {
                    netSocket.WriteLock.Release();
                }

                await networkStream.FlushAsync();
            }
            catch (Exception ex) when (ex is IOException || ex is ObjectDisposedException || ex is SocketException)
            {
                Log.Error(ex, "{0}.{1}: While writing a Frame to the network: {2}", this, nameof(SendFrameAsync),
                    ex.Message);
                responseCompletionSource?.TrySetException(ex);
            }
        }

        internal async Task SendEventAsync(string methodName, IMessage message)
        {
            var conversationId = AllocateNextConversationId();

            var sendContext = new EpoxySendContext(this);
            IBonded layerData;
            Error layerError = ProcessLayerStackOnSend(MessageType.Event, sendContext, out layerData);

            if (layerError != null)
            {
                Log.Debug("{0}.{1}: Sending event {2}/{3} failed due to layer error (Code: {4}, Message: {5}).",
                            this, nameof(SendEventAsync), conversationId, methodName, layerError.error_code, layerError.message);
                return;
            }

            var frame = MessageToFrame(conversationId, methodName, PayloadType.Event, message, layerData);

            Log.Debug("{0}.{1}: Sending event {2}/{3}.", this, nameof(SendEventAsync), conversationId, methodName);

            await SendFrameAsync(frame);
            Log.Debug("{0}.{1}: Sent event {2}/{3}.", this, nameof(SendEventAsync), conversationId, methodName);
        }

        internal Task StartAsync()
        {
            EnsureCorrectState(State.Created);
            duration = Stopwatch.StartNew();
            Task.Run((Func<Task>)ConnectionLoop);
            return m_startTask.Task;
        }

        private void EnsureCorrectState(State allowedStates, [CallerMemberName] string methodName = "<unknown>")
        {
            if ((m_state & allowedStates) == 0)
            {
                var message = $"Connection ({this}) is not in the correct state for the requested operation ({methodName}). Current state: {m_state} Allowed states: {allowedStates}";
                throw new InvalidOperationException(message);
            }
        }

        private ulong AllocateNextConversationId()
        {
            // Interlocked.Add() handles overflow by wrapping, not throwing.
            var newConversationId = Interlocked.Add(ref prevConversationId, 2);
            if (newConversationId < 0)
            {
                throw new EpoxyProtocolErrorException("Exhausted conversation IDs");
            }
            return unchecked((ulong)newConversationId);
        }

        private async Task ConnectionLoop()
        {
            while (true)
            {
                State nextState;

                try
                {
                    switch (m_state)
                    {
                        case State.Created:
                            nextState = DoCreated();
                            break;

                        case State.Connected:
                            // signal after state change to prevent races with
                            // EnsureCorrectState
                            m_startTask.SetResult(true);
                            nextState = await DoConnectedAsync();
                            break;

                        case State.SendProtocolError:
                            nextState = await DoSendProtocolErrorAsync();
                            break;

                        case State.Disconnecting:
                            nextState = DoDisconnect();
                            break;

                        case State.Disconnected:
                            DoDisconnected();
                            return;

                        default:
                            Log.Error("Unknown connection state: {0}", m_state);
                            m_protocolError = ProtocolErrorCode.INTERNAL_ERROR;
                            nextState = State.SendProtocolError;
                            break;
                    }
                }
                catch (Exception ex)
                {
                    Log.Error(ex, "{0}.{1} Unhandled exception. CurrentState: {2}", this, nameof(ConnectionLoop), m_state);

                    if (m_state != State.Disconnecting && m_state != State.Disconnected)
                    {
                        // we're in a state where we can attempt to disconnect
                        nextState = State.Disconnecting;
                    }
                    else
                    {
                        // some part of disconnecting threw. Just give up and be done.
                        m_startTask.TrySetResult(true);
                        m_stopTask.TrySetResult(true);
                        return;
                    }
                }

                m_state = nextState;
            }
        }

        private State DoCreated()
        {
            State result;

            if (m_connectionType == ConnectionType.Server)
            {
                var args = new ConnectedEventArgs(this);
                Error disconnectError = m_parentListener.InvokeOnConnected(args);

                if (disconnectError == null)
                {
                    result = State.Connected;
                }
                else
                {
                    Log.Information("Rejecting connection {0} because {1}:{2}",
                        this, disconnectError.error_code, disconnectError.message);

                    m_protocolError = ProtocolErrorCode.CONNECTION_REJECTED;
                    m_errorDetails = disconnectError;
                    result = State.SendProtocolError;
                }
            }
            else
            {
                result = State.Connected;
            }

            return result;
        }

        private async Task<State> DoConnectedAsync()
        {
            while (!m_shutdownTokenSource.IsCancellationRequested)
            {
                Frame frame;

                try
                {
                    Stream networkStream = netSocket.NetworkStream;
                    frame = await Frame.ReadAsync(networkStream, m_shutdownTokenSource.Token);
                    if (frame == null)
                    {
                        Log.Information("{0}.{1} EOS encountered, so disconnecting.", this,
                            nameof(DoConnectedAsync));
                        return State.Disconnecting;
                    }
                }
                catch (EpoxyProtocolErrorException pex)
                {
                    Log.Error(pex, "{0}.{1} Protocol error encountered.", this,
                        nameof(DoConnectedAsync));
                    m_protocolError = ProtocolErrorCode.PROTOCOL_VIOLATED;
                    return State.SendProtocolError;
                }
                catch (Exception ex) when (ex is IOException || ex is ObjectDisposedException || ex is SocketException)
                {
                    Log.Error(ex, "{0}.{1} IO error encountered.", this, nameof(DoConnectedAsync));
                    return State.Disconnecting;
                }

                var result = EpoxyProtocol.Classify(frame);
                switch (result.Disposition)
                {
                    case EpoxyProtocol.FrameDisposition.DeliverRequestToService:
                    {
                        State? nextState = DispatchRequest(result.Headers, result.Payload, result.LayerData);
                        if (nextState.HasValue)
                        {
                            return nextState.Value;
                        }
                        else
                        {
                            // continue the read loop
                            break;
                        }
                    }

                    case EpoxyProtocol.FrameDisposition.DeliverResponseToProxy:
                        DispatchResponse(result.Headers, result.Payload, result.LayerData);
                        break;

                    case EpoxyProtocol.FrameDisposition.DeliverEventToService:
                        DispatchEvent(result.Headers, result.Payload, result.LayerData);
                        break;

                    case EpoxyProtocol.FrameDisposition.SendProtocolError:
                        m_protocolError = result.ErrorCode ?? ProtocolErrorCode.INTERNAL_ERROR;
                        return State.SendProtocolError;

                    case EpoxyProtocol.FrameDisposition.HangUp:
                        return State.Disconnecting;

                    default:
                        Log.Error("{0}.{1}: Unsupported FrameDisposition {2}", this, nameof(DoConnectedAsync), result.Disposition);
                        m_protocolError = ProtocolErrorCode.INTERNAL_ERROR;
                        return State.SendProtocolError;
                }
            }

            // shutdown requested between reading frames
            return State.Disconnecting;
        }

        private async Task<State> DoSendProtocolErrorAsync()
        {
            ProtocolErrorCode errorCode = m_protocolError;
            Error details = m_errorDetails;

            var frame = MakeProtocolErrorFrame(errorCode, details);
            Log.Debug("{0}.{1}: Sending protocol error with code {2} and details {3}.",
                this, nameof(DoSendProtocolErrorAsync), errorCode, details == null ? "<null>" : details.error_code + details.message);

            await SendFrameAsync(frame);
            Log.Debug("{0}.{1}: Sent protocol error with code {2}.", this, nameof(DoSendProtocolErrorAsync), errorCode);

            return State.Disconnecting;
        }

        private State DoDisconnect()
        {
            Log.Debug("{0}.{1}: Shutting down.", this, nameof(DoDisconnect));

            netSocket.Shutdown();

            if (m_connectionType == ConnectionType.Server)
            {
                var args = new DisconnectedEventArgs(this, m_errorDetails);
                m_parentListener.InvokeOnDisconnected(args);
            }

            return State.Disconnected;
        }

        private void DoDisconnected()
        {
            // signal after state change to prevent races with
            // EnsureCorrectState
            m_startTask.TrySetResult(true);
            m_stopTask.SetResult(true);

            duration.Stop();
            connectionMetrics.duration_millis = (float) duration.Elapsed.TotalMilliseconds;
            Metrics.Emit(connectionMetrics);
        }

        private State? DispatchRequest(EpoxyHeaders headers, ArraySegment<byte> payload, ArraySegment<byte> layerData)
        {
            if (headers.error_code != (int)ErrorCode.OK)
            {
                Log.Error("{0}.{1}: Received conversation ID {2} with a non-zero error code.",
                    this, nameof(DispatchRequest), headers.conversation_id);
                m_protocolError = ProtocolErrorCode.PROTOCOL_VIOLATED;
                return State.SendProtocolError;
            }

            IMessage request = Message.FromPayload(Unmarshal.From(payload));

            var receiveContext = new EpoxyReceiveContext(this);

            Error layerError = ProcessLayerStackOnReceive(MessageType.Request, receiveContext, layerData);

            Task.Run(async () =>
            {
                IMessage result = null;

                if (layerError == null)
                {
                    result = await m_serviceHost.DispatchRequest(headers.method_name, receiveContext, request,
                            connectionMetrics);
                }
                else
                {
                    result = Message.FromError(layerError);
                }

                await SendReplyAsync(headers.conversation_id, result);
            });

            // no state change needed
            return null;
        }

        private void DispatchResponse(EpoxyHeaders headers, ArraySegment<byte> payload, ArraySegment<byte> layerData)
        {
            TaskCompletionSource<IMessage> responseCompletionSource;

            lock (m_requestsLock)
            {
                if (!m_outstandingRequests.TryGetValue(headers.conversation_id, out responseCompletionSource))
                {
                    Log.Error("{0}.{1}: Got a response with unexpected ID {2}.",
                        this, nameof(DispatchResponse), headers.conversation_id);
                    return;
                }

                m_outstandingRequests.Remove(headers.conversation_id);
            }

            IMessage response;
            if (headers.error_code != (int)ErrorCode.OK)
            {
                response = Message.FromError(Unmarshal<Error>.From(payload));
            }
            else
            {
                response = Message.FromPayload(Unmarshal.From(payload));
            }

            var receiveContext = new EpoxyReceiveContext(this);

            Error layerError = ProcessLayerStackOnReceive(MessageType.Response, receiveContext, layerData);

            if (layerError != null)
            {
                response = Message.FromError(layerError);
            }

            responseCompletionSource.SetResult(response);
        }

        private void DispatchEvent(EpoxyHeaders headers, ArraySegment<byte> payload, ArraySegment<byte> layerData)
        {
            if (headers.error_code != (int)ErrorCode.OK)
            {
                Log.Error("{0}.{1}: Received event ID {2} with a non-zero error code.",
                    this, nameof(DispatchEvent), headers.conversation_id);
                return;
            }

            IMessage request = Message.FromPayload(Unmarshal.From(payload));

            var receiveContext = new EpoxyReceiveContext(this);

            Error layerError = ProcessLayerStackOnReceive(MessageType.Event, receiveContext, layerData);

            if (layerError == null)
            {
                Task.Run(async () =>
                {
                    await m_serviceHost.DispatchEvent(headers.method_name, new EpoxyReceiveContext(this), request,
                            connectionMetrics);
                });
            }
        }

        public override Task StopAsync()
        {
            EnsureCorrectState(State.Connected | State.SendProtocolError | State.Disconnecting | State.Disconnected);
            m_shutdownTokenSource.Cancel();
            netSocket.Shutdown();

            return m_stopTask.Task;
        }

        public async Task<IMessage<TResponse>> RequestResponseAsync<TRequest, TResponse>(string methodName, IMessage<TRequest> message, CancellationToken ct)
        {
            EnsureCorrectState(State.Connected);

            // TODO: cancellation
            IMessage response = await SendRequestAsync(methodName, message);
            return response.Convert<TResponse>();
        }

        public Task FireEventAsync<TPayload>(string methodName, IMessage<TPayload> message)
        {
            EnsureCorrectState(State.Connected);
            return SendEventAsync(methodName, message);
        }

        /// <summary>
        /// Epoxy-private wrapper around <see cref="Socket"/>. Provides idempotent shutdown.
        /// </summary>
        private class EpoxySocket
        {
            Socket socket;
            NetworkStream stream;
            int isShutdown;

            public EpoxySocket(Socket sock)
            {
                socket = sock;
                stream = new NetworkStream(sock, ownsSocket: false);
                WriteLock = new SemaphoreSlim(1, 1);
                isShutdown = 0;
            }

            public Stream NetworkStream
            {
                get
                {
                    if (isShutdown != 0)
                    {
                        throw new ObjectDisposedException(nameof(EpoxySocket));
                    }

                    return stream;
                }
            }

            // It looks like we don't need to .Dispose this SemaphoreSlim. The
            // current implementation of SemaphoreSlim only does interesting
            // stuff during .Dispose if there's an allocated
            // AvailableWaitHandle. We never call that, so there shouldn't be
            // anything needing disposal. If we do end up allocating a wait
            // handle somehow, its finalizer will save us.
            public SemaphoreSlim WriteLock { get; }

            public void Shutdown()
            {
                int oldIsShutdown = Interlocked.CompareExchange(ref isShutdown, 1, 0);
                if (oldIsShutdown == 0)
                {
                    // we are responsible for shutdown
                    try
                    {
                        stream.Dispose();

                        try
                        {
                            socket.Shutdown(SocketShutdown.Both);
                        }
                        catch (ObjectDisposedException)
                        {
                            // ignore, as we're shutting down anyway
                        }

                        // We cannot call socket.Disconnect, as that will block
                        // for longer than we want. So, we just forcible close
                        // the socket with Dispose
                        socket.Dispose();
                    }
                    catch (Exception ex) when (ex is IOException || ex is SocketException)
                    {
                        Log.Error(ex, "Exception during connection shutdown");
                    }

                    stream = null;
                    socket = null;
                }
            }
        }

        private Error ProcessLayerStackOnSend(MessageType messageType, EpoxySendContext sendContext, out IBonded layerData)
        {
            Error error = null;
            ILayerStack layerStack = m_parentTransport.LayerStack;
            layerData = null;
            if (layerStack != null)
            {
                error = layerStack.OnSend(messageType, sendContext, out layerData);
                if (error != null)
                {
                    Log.Warning("{0}.{1}: Layer error occurred sending message of type {2} (Code: {3} Message: {4}).",
                                    this, nameof(ProcessLayerStackOnSend), messageType.ToString(), error.error_code, error.message);
                }
            }

            return error;
        }

        private Error ProcessLayerStackOnReceive(MessageType messageType, EpoxyReceiveContext receiveContext, ArraySegment<byte> layerData)
        {
            Error error = null;
            ILayerStack layerStack = m_parentTransport.LayerStack;

            if (layerStack != null)
            {
                if (layerData.Array == null)
                {
                    Log.Warning("{0}.{1}: Layer stack present but no layer data received.", this, nameof(ProcessLayerStackOnReceive));
                }

                error = layerStack.OnReceive(
                                        messageType,
                                        receiveContext,
                                        (layerData.Array == null ? null : Unmarshal.From(layerData)));
                if (error != null)
                {
                    Log.Warning("{0}.{1}: Layer error occurred receiving message of type {2} (Code: {3} Message: {4}).",
                                    this, nameof(ProcessLayerStackOnReceive), messageType.ToString(), error.error_code, error.message);
                }
            }
            return error;
        }
    }
}
