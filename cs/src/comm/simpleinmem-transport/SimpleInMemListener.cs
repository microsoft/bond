// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using System.Threading.Tasks.Dataflow;
    using Bond.Comm.Service;
    using Bond.Comm.SimpleInMem.Processor;

    public class SimpleInMemListener : Listener
    {
        private ServiceHost serviceHost;
        private SimpleInMemTransport transport;
        private string address;
        private readonly string logname;
        private Dictionary<Guid, ConnectionPair> connectionPairs;
        private object connectionsLock = new object();
        private CancellationTokenSource cancelTokenSource = new CancellationTokenSource();
        private bool isStarted = false;

        /// <summary>
        /// Creates a new listener using parentTransport and address.
        /// </summary>
        /// <exception cref="ArgumentNullException">parentTransport is null</exception>
        /// <exception cref="ArgumentException">address is null or empty</exception>
        public SimpleInMemListener(
            SimpleInMemTransport transport, string address,
            Logger logger, Metrics metrics) : base(logger, metrics)
        {
            if (transport == null) throw new ArgumentNullException(nameof(transport));
            if (string.IsNullOrEmpty(address)) throw new ArgumentException(nameof(address));

            this.address = address;
            serviceHost = new ServiceHost(logger);
            this.transport = transport;
            connectionPairs = new Dictionary<Guid, ConnectionPair>();
            logname = $"{nameof(SimpleInMemListener)}({this.address})";
        }

        public bool IsStarted
        {
            get
            {
                return isStarted;
            }
        }

        internal ServiceHost ServiceHost
        {
            get
            {
                return serviceHost;
            }
        }

        public override bool IsRegistered(string serviceMethodName)
        {
            return serviceHost.IsRegistered(serviceMethodName);
        }

        public override void AddService<T>(T service)
        {
            logger.Site().Information("{0}: Adding {1}.", logname, typeof(T).Name);
            serviceHost.Register(service);
        }

        public override void RemoveService<T>(T service)
        {
            serviceHost.Deregister((IService)service);
        }

        public override Task StartAsync()
        {
            if (!IsStarted)
            {
                lock (connectionsLock)
                {
                    if (!IsStarted)
                    {
                        Util.CreateLongRunningTask(o => Cleanup(), cancelTokenSource.Token, 1000, 1500).Post(new object());
                        isStarted = true;
                    }
                }
            }

            return TaskExt.CompletedTask;
        }

        public override Task StopAsync()
        {
            if (IsStarted)
            {
                lock (connectionsLock)
                {
                    if (IsStarted)
                    {
                        cancelTokenSource.Cancel();
                        foreach (ConnectionPair connectionPair in connectionPairs.Values)
                        {
                            try
                            {
                                Stop(connectionPair);
                            }
                            catch (Exception e)
                            {
                                logger.Site().Error(e, "{0}: Error stopping connection", logname);
                            }
                        }
                        connectionPairs.Clear();
                        isStarted = false;
                    }
                }
            }

            return TaskExt.CompletedTask;
        }

        public IEnumerable<Guid> GetPairIds()
        {
            lock(connectionsLock)
            {
                return new List<Guid>(connectionPairs.Keys);
            }
        }

        public SimpleInMemConnection GetConnection(Guid pairId, ConnectionType connectionType)
        {
            ConnectionPair pair;
            SimpleInMemConnection connection = null;
            bool foundPair = false;

            lock (connectionsLock)
            {
                foundPair = connectionPairs.TryGetValue(pairId, out pair);
            }

            if (foundPair)
            {
                if (connectionType == ConnectionType.Client)
                {
                    connection = pair.Client;
                }
                else if (connectionType == ConnectionType.Server)
                {
                    connection = pair.Server;
                }
            }

            return connection;
        }

        internal ConnectionPair CreateConnectionPair()
        {
            var clientConnection = new SimpleInMemConnection(this, ConnectionType.Client, new ServiceHost(logger),
                transport, logger, metrics);
            var serverConnection = new SimpleInMemConnection(this, ConnectionType.Server, serviceHost, transport, logger, metrics);
            var connectionPair = SimpleInMemConnection.Pair(clientConnection, serverConnection);

            Add(connectionPair);
            return connectionPair;
        }

        private void Add(ConnectionPair connectionPair)
        {
            var client = connectionPair.Client;
            var connectedEventArgs = new ConnectedEventArgs(client);
            Error error = OnConnected(connectedEventArgs);

            if (error != null)
            {
                logger.Site().Information("{0}: Rejecting connection {1} because {2}:{3}.",
                                       logname, client.Id, error.error_code, error.message);
                throw new SimpleInMemProtocolErrorException(
                    "Connection rejected",
                    details: error,
                    innerException: null);
            }

            lock (connectionsLock)
            {
                connectionPairs.Add(connectionPair.Id, connectionPair);
            }
        }
        
        private void Cleanup()
        {
            List<Guid> disconnectedPairIds = new List<Guid>();
            foreach (ConnectionPair pair in connectionPairs.Values)
            {
                if (!pair.Server.IsConnected || !pair.Client.IsConnected)
                {
                    Stop(pair);
                    disconnectedPairIds.Add(pair.Id);
                }
            }

            lock (connectionsLock)
            {
                foreach (Guid disconnectedPairId in disconnectedPairIds)
                {
                    connectionPairs.Remove(disconnectedPairId);
                }
            }
        }

        private void Stop(ConnectionPair pair)
        {
            if (pair.Client.State == CnxState.Connected)
            {
                pair.Client.StopAsync();
            }

            if (pair.Server.State == CnxState.Connected)
            {
                pair.Server.StopAsync();
            }
        }

        internal Error InvokeOnConnected(ConnectedEventArgs args)
        {
            return OnConnected(args);
        }

        internal void InvokeOnDisconnected(DisconnectedEventArgs args)
        {
            OnDisconnected(args);
        }
    }
}
