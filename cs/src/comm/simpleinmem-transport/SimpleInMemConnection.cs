// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Runtime.CompilerServices;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Service;
    using Processor;
    using System.Collections.Generic;
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
        private readonly Guid m_connectionId;
        private readonly ConnectionType m_connectionType;
        private readonly ServiceHost m_serviceHost;
        private readonly SimpleInMemListener m_parentListener;
        private InMemFrameQueue m_communicationQueue;
        private InMemFrameQueueCollection m_serverqueues;
        private object m_connectionsLock = new object();
        private long prevConversationId;
        private CancellationTokenSource m_cancelTokenSource = new CancellationTokenSource();
        private CnxState m_state;
        private object m_stateLock = new object();
        private HashSet<SimpleInMemConnection> m_clientConnections;
        public ConnectionMetrics ConnectionMetrics { get; } = new ConnectionMetrics();

        public SimpleInMemConnection(SimpleInMemTransport parentTransport, SimpleInMemListener parentListener, ConnectionType connectionType) :
            this (new ServiceHost(parentTransport), parentListener, connectionType)
        {
        }

        internal SimpleInMemConnection(ServiceHost serviceHost, SimpleInMemListener parentListener, ConnectionType connectionType)
        {
            if (serviceHost == null) throw new ArgumentNullException(nameof(serviceHost));
            if (parentListener == null) throw new ArgumentNullException(nameof(parentListener));

            m_connectionId = Guid.NewGuid();
            m_connectionType = connectionType;
            m_serviceHost = serviceHost;
            m_parentListener = parentListener;

            switch(connectionType)
            {
                case ConnectionType.Client:
                    m_communicationQueue = new InMemFrameQueue();
                    break;

                case ConnectionType.Server:
                    m_serverqueues = new InMemFrameQueueCollection();
                    m_clientConnections = new HashSet<SimpleInMemConnection>();
                    break;

                default:
                    throw new NotSupportedException(nameof(connectionType));
            }

            // start at -1 or 0 so the first conversation ID is 1 or 2.
            prevConversationId = (connectionType == ConnectionType.Client) ? -1 : 0;

            m_state = CnxState.Created;

            ConnectionMetrics.connection_id = m_connectionId.ToString();
        }

        public CnxState State
        {
            get
            {
                return m_state;
            }
        }

        public ConnectionType ConnectionType
        {
            get
            {
                return m_connectionType;
            }
        }

        public Guid Id
        {
            get
            {
                return m_connectionId;
            }
        }

        public override string ToString()
        {
            return $"{nameof(SimpleInMemConnection)}({m_connectionId})";
        }

        public override Task StopAsync()
        {
            bool connected = false;
            lock (m_stateLock)
            {
                if (connected = ((m_state & CnxState.Connected) != 0))
                {
                    m_state = CnxState.Disconnecting;
                    m_cancelTokenSource.Cancel();
                }
            }

            if (connected)
            {
                OnDisconnect();
                lock (m_stateLock)
                {
                    m_state = CnxState.Disconnected;
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
                return m_communicationQueue;
            }
        }

        internal void Start()
        {
            lock (m_stateLock)
            {
                EnsureCorrectState(CnxState.Created | CnxState.Disconnected);

                if (m_connectionType == ConnectionType.Client)
                {
                    var responseProcessor = new ResponseProcessor(this, m_communicationQueue);
                    Task.Run(() => responseProcessor.ProcessAsync(m_cancelTokenSource.Token));
                }
                else if (m_connectionType == ConnectionType.Server)
                {
                    var requestProcessor = new RequestProcessor(this, m_serviceHost, m_serverqueues);
                    var eventProcessor = new EventProcessor(this, m_serviceHost, m_serverqueues);
                    Task.Run(() => requestProcessor.ProcessAsync(m_cancelTokenSource.Token));
                    Task.Run(() => eventProcessor.ProcessAsync(m_cancelTokenSource.Token));
                }
                else
                {
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Connection type {2} not implemented.",
                        this, nameof(Start), m_connectionType);
                    throw new NotImplementedException(message);
                }

                m_state = CnxState.Connected;
            }
        }

        internal void AddClientConnection(SimpleInMemConnection connection)
        {
            if (m_connectionType == ConnectionType.Client)
            {
                var message = LogUtil.FatalAndReturnFormatted(
                    "{0}.{1}: Client connection does not support adding new request response queue.",
                    this, nameof(AddClientConnection));
                throw new NotSupportedException(message);
            }

            m_serverqueues.Add(connection.Id, connection.CommunicationQueue);
            lock (m_connectionsLock)
            {
                m_clientConnections.Add(connection);
            }
        }

        private Task<IMessage> SendRequestAsync(string methodName, IMessage request)
        {
            var conversationId = AllocateNextConversationId();
            var payload = Util.NewPayLoad(conversationId, PayloadType.Request, request, new TaskCompletionSource<IMessage>());
            payload.m_headers.method_name = methodName;
            m_communicationQueue.Enqueue(payload);

            return payload.m_outstandingRequest.Task;
        }

        private void SendEventAsync(string methodName, IMessage message)
        {
            var conversationId = AllocateNextConversationId();
            var payload = Util.NewPayLoad(conversationId, PayloadType.Event, message, null);
            payload.m_headers.method_name = methodName;
            m_communicationQueue.Enqueue(payload);
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
            m_parentListener.InvokeOnDisconnected(args);

            if(m_connectionType == ConnectionType.Client)
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
            m_communicationQueue.Clear();
        }

        private void DisconnectServer()
        {
            m_serverqueues.ClearAll();

            lock (m_connectionsLock)
            {
                foreach (SimpleInMemConnection connection in m_clientConnections)
                {
                    connection.StopAsync();
                }

                m_clientConnections.Clear();
            }
        }

        private void EnsureCorrectState(CnxState allowedStates, [CallerMemberName] string methodName = "<unknown>")
        {

            if ((m_state & allowedStates) == 0)
            {
                var message = $"Connection (${this}) is not in the correct state for the requested operation (${methodName}). Current state: ${m_state} Allowed states: ${allowedStates}";
                throw new InvalidOperationException(message);
            }
        }
    }
}
