// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Net;
    using System.Net.Sockets;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Service;

    public class EpoxyListener : Listener
    {
        private EpoxyTransport parentTransport;
        private System.Net.Sockets.TcpListener listener;
        private ServiceHost serviceHost;

        private object connectionsLock = new object();
        private HashSet<EpoxyConnection> connections;

        private Task acceptTask;

        private CancellationTokenSource shutdownTokenSource;

        public EpoxyListener(EpoxyTransport parentTransport, IPEndPoint listenEndpoint, Logger logger) : base(logger)
        {
            this.parentTransport = parentTransport;
            listener = new System.Net.Sockets.TcpListener(listenEndpoint);
            serviceHost = new ServiceHost(parentTransport, logger);
            connections = new HashSet<EpoxyConnection>();
            shutdownTokenSource = new CancellationTokenSource();
        }

        public IPEndPoint ListenEndpoint
        {
            get
            {
                return (IPEndPoint)listener.LocalEndpoint;
            }
        }

        public override string ToString()
        {
            return $"EpoxyListener({ListenEndpoint})";
        }

        public override bool IsRegistered(string serviceMethodName)
        {
            return serviceHost.IsRegistered(serviceMethodName);
        }

        public override void AddService<T>(T service)
        {
            logger.Site().Information("Listener on {0} adding {1}.", ListenEndpoint, typeof(T).Name);
            serviceHost.Register(service);
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public override Task StartAsync()
        {
            listener.Start();
            acceptTask = Task.Run(() => AcceptAsync(shutdownTokenSource.Token), shutdownTokenSource.Token);
            return TaskExt.CompletedTask;
        }

        public override Task StopAsync()
        {
            shutdownTokenSource.Cancel();
            listener.Stop();

            return acceptTask;
        }

        internal Error InvokeOnConnected(ConnectedEventArgs args)
        {
            return OnConnected(args);
        }

        internal void InvokeOnDisconnected(DisconnectedEventArgs args)
        {
            OnDisconnected(args);
        }

        private async Task AcceptAsync(CancellationToken t)
        {
            logger.Site().Information("Accepting connections on {0}", ListenEndpoint);
            while (!t.IsCancellationRequested)
            {
                Socket socket = null;

                try
                {
                    socket = await listener.AcceptSocketAsync();
                    var connection = EpoxyConnection.MakeServerConnection(
                        parentTransport,
                        this,
                        serviceHost,
                        socket,
                        logger);
                    socket = null; // connection now owns the socket and will close it

                    lock (connectionsLock)
                    {
                        connections.Add(connection);
                    }

                    await connection.StartAsync();
                    logger.Site().Debug("Accepted connection from {0}.", connection.RemoteEndPoint);
                }
                catch (SocketException ex)
                {
                    logger.Site().Error(ex, "Accept failed with error {0}.", ex.SocketErrorCode);

                    ShutdownSocketSafe(socket, logger);
                }
                catch (ObjectDisposedException)
                {
                    ShutdownSocketSafe(socket, logger);

                    // TODO: ignoring this exception is needed during shutdown,
                    //       but there should be a cleaner way. We should
                    //       switch to having a proper life-cycle for a
                    //       connection.
                }
            }
            logger.Site().Information("Shutting down connection on {0}", ListenEndpoint);
        }

        private static void ShutdownSocketSafe(Socket socket, Logger logger)
        {
            try
            {
                socket?.Shutdown(SocketShutdown.Both);
                socket?.Close();
            }
            catch (SocketException ex)
            {
                // We tried to cleanly shutdown the socket, oh well.
                logger.Site().Debug(ex, "Exception encountered when shutting down a socket.");
            }
        }
    }
}
