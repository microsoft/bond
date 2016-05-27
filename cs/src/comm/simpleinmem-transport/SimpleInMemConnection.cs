// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Collections.Generic;
    using System.Runtime.CompilerServices;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Layers;
    using Bond.Comm.Service;
    using Bond.Comm.SimpleInMem.Processor;

    public enum ConnectionType
    {
        Client,
        Server
    }

    [Flags]
    public enum CnxState
    {
        Created = 0x01,
        Connected = 0x02,
        SendProtocolError = 0x04,
        Disconnecting = 0x08,
        Disconnected = 0x10,
    }

    public class SimpleInMemConnection : Connection, IRequestResponseConnection, IEventConnection
    {
        private readonly Guid connectionId;
        private readonly ConnectionType connectionType;
        private readonly ServiceHost serviceHost;
        private readonly SimpleInMemListener parentListener;
        private InMemFrameQueue communicationQueue;
        private InMemFrameQueueCollection serverqueues;
        private object connectionsLock = new object();
        private long prevConversationId;
        private CancellationTokenSource cancelTokenSource = new CancellationTokenSource();
        private CnxState state;
        private object stateLock = new object();
        private HashSet<SimpleInMemConnection> clientConnections;
        public ConnectionMetrics ConnectionMetrics { get; } = new ConnectionMetrics();

        public SimpleInMemConnection(SimpleInMemTransport parentTransport, SimpleInMemListener parentListener, ConnectionType connectionType) :
            this (new ServiceHost(parentTransport), parentListener, connectionType)
        {
        }

        internal SimpleInMemConnection(ServiceHost serviceHost, SimpleInMemListener parentListener, ConnectionType connectionType)
        {
            if (serviceHost == null) throw new ArgumentNullException(nameof(serviceHost));
            if (parentListener == null) throw new ArgumentNullException(nameof(parentListener));

            connectionId = Guid.NewGuid();
            this.connectionType = connectionType;
            this.serviceHost = serviceHost;
            this.parentListener = parentListener;

            switch(connectionType)
            {
                case ConnectionType.Client:
                    communicationQueue = new InMemFrameQueue();
                    break;

                case ConnectionType.Server:
                    serverqueues = new InMemFrameQueueCollection();
                    clientConnections = new HashSet<SimpleInMemConnection>();
                    break;

                default:
                    throw new NotSupportedException(nameof(connectionType));
            }

            // start at -1 or 0 so the first conversation ID is 1 or 2.
            prevConversationId = (connectionType == ConnectionType.Client) ? -1 : 0;

            state = CnxState.Created;

            ConnectionMetrics.connection_id = connectionId.ToString();
        }

        public CnxState State
        {
            get
            {
                return state;
            }
        }

        public ConnectionType ConnectionType
        {
            get
            {
                return connectionType;
            }
        }

        public Guid Id
        {
            get
            {
                return connectionId;
            }
        }

        public override string ToString()
        {
            return $"{nameof(SimpleInMemConnection)}({connectionId})";
        }

        public override Task StopAsync()
        {
            bool connected = false;
            lock (stateLock)
            {
                if (connected = ((state & CnxState.Connected) != 0))
                {
                    state = CnxState.Disconnecting;
                    cancelTokenSource.Cancel();
                }
            }

            if (connected)
            {
                OnDisconnect();
                lock (stateLock)
                {
                    state = CnxState.Disconnected;
                }
            }

            return TaskExt.CompletedTask;
        }

        public async Task<IMessage<TResponse>> RequestResponseAsync<TRequest, TResponse>(string methodName, IMessage<TRequest> message, CancellationToken ct)
        {
            EnsureCorrectState(CnxState.Connected);
            IMessage response = await SendRequestAsync(methodName, message);
            return response.Convert<TResponse>();
        }

        public Task FireEventAsync<TPayload>(string methodName, IMessage<TPayload> message)
        {
            EnsureCorrectState(CnxState.Connected);
            SendEventAsync(methodName, message);
            return TaskExt.CompletedTask;
        }

        internal InMemFrameQueue CommunicationQueue
        {
            get
            {
                return communicationQueue;
            }
        }

        internal void Start()
        {
            lock (stateLock)
            {
                EnsureCorrectState(CnxState.Created | CnxState.Disconnected);

                if (connectionType == ConnectionType.Client)
                {
                    var responseProcessor = new ResponseProcessor(this, serviceHost.ParentTransport, communicationQueue);
                    Task.Run(() => responseProcessor.ProcessAsync(cancelTokenSource.Token));
                }
                else if (connectionType == ConnectionType.Server)
                {
                    var requestProcessor = new RequestProcessor(this, serviceHost, serverqueues);
                    var eventProcessor = new EventProcessor(this, serviceHost, serverqueues);
                    Task.Run(() => requestProcessor.ProcessAsync(cancelTokenSource.Token));
                    Task.Run(() => eventProcessor.ProcessAsync(cancelTokenSource.Token));
                }
                else
                {
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Connection type {2} not implemented.",
                        this, nameof(Start), connectionType);
                    throw new NotImplementedException(message);
                }

                state = CnxState.Connected;
            }
        }

        internal void AddClientConnection(SimpleInMemConnection connection)
        {
            if (connectionType == ConnectionType.Client)
            {
                var message = LogUtil.FatalAndReturnFormatted(
                    "{0}.{1}: Client connection does not support adding new request response queue.",
                    this, nameof(AddClientConnection));
                throw new NotSupportedException(message);
            }

            serverqueues.Add(connection.Id, connection.CommunicationQueue);
            lock (connectionsLock)
            {
                clientConnections.Add(connection);
            }
        }

        private Task<IMessage> SendRequestAsync(string methodName, IMessage request)
        {
            var conversationId = AllocateNextConversationId();

            var sendContext = new SimpleInMemSendContext(this);
            IBonded layerData;
            Error layerError = LayerStackUtils.ProcessOnSend(this.serviceHost.ParentTransport.LayerStack,
                                                             MessageType.Request, sendContext, out layerData);

            if (layerError != null)
            {
                Log.Error("{0}.{1}: Sending request {2}/{3} failed due to layer error (Code: {4}, Message: {5}).",
                            this, nameof(SendRequestAsync), conversationId, methodName, layerError.error_code, layerError.message);
                return Task.FromResult<IMessage>(Message.FromError(layerError));
            }

            var payload = Util.NewPayLoad(conversationId, PayloadType.Request, layerData, request, new TaskCompletionSource<IMessage>());
            payload.headers.method_name = methodName;
            communicationQueue.Enqueue(payload);

            return payload.outstandingRequest.Task;
        }

        private void SendEventAsync(string methodName, IMessage message)
        {
            var conversationId = AllocateNextConversationId();

            var sendContext = new SimpleInMemSendContext(this);
            IBonded layerData;
            Error layerError = LayerStackUtils.ProcessOnSend(this.serviceHost.ParentTransport.LayerStack,
                                                             MessageType.Event, sendContext, out layerData);

            if (layerError != null)
            {
                Log.Error("{0}.{1}: Sending event {2}/{3} failed due to layer error (Code: {4}, Message: {5}).",
                            this, nameof(SendEventAsync), conversationId, methodName, layerError.error_code, layerError.message);
                return;
            }

            var payload = Util.NewPayLoad(conversationId, PayloadType.Event, layerData, message, null);
            payload.headers.method_name = methodName;
            communicationQueue.Enqueue(payload);
        }

        private ulong AllocateNextConversationId()
        {
            // Interlocked.Add() handles overflow by wrapping, not throwing.
            var newConversationId = Interlocked.Add(ref prevConversationId, 2);
            if (newConversationId < 0)
            {
                throw new SimpleInMemProtocolErrorException("Exhausted conversation IDs");
            }
            return unchecked((ulong)newConversationId);
        }

        private void OnDisconnect()
        {
            Log.Debug("{0}.{1}: Shutting down.", this, nameof(OnDisconnect));

            var args = new DisconnectedEventArgs(this, null);
            parentListener.InvokeOnDisconnected(args);

            if(connectionType == ConnectionType.Client)
            {
                DisconnectClient();
            }
            else
            {
                DisconnectServer();
            }
        }

        private void DisconnectClient()
        {
            communicationQueue.Clear();
        }

        private void DisconnectServer()
        {
            serverqueues.ClearAll();

            lock (connectionsLock)
            {
                foreach (SimpleInMemConnection connection in clientConnections)
                {
                    connection.StopAsync();
                }

                clientConnections.Clear();
            }
        }

        private void EnsureCorrectState(CnxState allowedStates, [CallerMemberName] string methodName = "<unknown>")
        {

            if ((state & allowedStates) == 0)
            {
                var message = $"Connection (${this}) is not in the correct state for the requested operation (${methodName}). Current state: ${state} Allowed states: ${allowedStates}";
                throw new InvalidOperationException(message);
            }
        }
    }
}
