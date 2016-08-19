// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Net;
    using System.Net.Sockets;
    using System.Security.Authentication;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Service;

    public class EpoxyListener : Listener
    {
        EpoxyTransport parentTransport;
        EpoxyServerTlsConfig tlsConfig;
        TcpListener listener;
        ServiceHost serviceHost;

        object connectionsLock = new object();
        HashSet<EpoxyConnection> connections;

        Task acceptTask;

        CancellationTokenSource shutdownTokenSource;

        internal EpoxyListener(
            EpoxyTransport parentTransport,
            IPEndPoint listenEndpoint,
            EpoxyServerTlsConfig tlsConfig,
            Logger logger, Metrics metrics) : base(logger, metrics)
        {
            Debug.Assert(parentTransport != null);
            Debug.Assert(listenEndpoint != null);

            this.parentTransport = parentTransport;

            // will be null if not using TLS
            this.tlsConfig = tlsConfig;

            listener = new TcpListener(listenEndpoint);
            serviceHost = new ServiceHost(logger);
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
                EpoxyNetworkStream epoxyStream = null;

                try
                {
                    socket = await listener.AcceptSocketAsync();
                    logger.Site().Debug("Accepted connection from {0}.", socket.RemoteEndPoint);

                    epoxyStream = await EpoxyNetworkStream.MakeServerStreamAsync(socket, tlsConfig, logger);
                    socket = null; // epoxyStream now owns the socket

                    var connection = EpoxyConnection.MakeServerConnection(
                        parentTransport,
                        this,
                        serviceHost,
                        epoxyStream,
                        logger,
                        metrics);

                    // connection now owns the EpoxyNetworkStream
                    epoxyStream = null;

                    lock (connectionsLock)
                    {
                        connections.Add(connection);
                    }

                    await connection.StartAsync();
                    logger.Site().Debug("Started server-side connection for {0}", connection.RemoteEndPoint);
                }
                catch (AuthenticationException ex)
                {
                    logger.Site().Error(ex, "Failed to authenticate remote connection from {0}", socket?.RemoteEndPoint);
                    ShutdownSocketSafe(socket, epoxyStream);
                }
                catch (SocketException ex)
                {
                    logger.Site().Error(ex, "Accept failed with error {0}.", ex.SocketErrorCode);
                    ShutdownSocketSafe(socket, epoxyStream);
                }
                catch (ObjectDisposedException)
                {
                    ShutdownSocketSafe(socket, epoxyStream);

                    // TODO: ignoring this exception is needed during shutdown,
                    //       but there should be a cleaner way. We should
                    //       switch to having a proper life-cycle for a
                    //       connection.
                }
            }

            logger.Site().Information("Shutting down connection on {0}", ListenEndpoint);
        }

        private void ShutdownSocketSafe(Socket socket, EpoxyNetworkStream epoxyStream)
        {
            if (epoxyStream != null)
            {
                epoxyStream.Shutdown();
                // epoxyStream owns the socket, so we shouldn't try to shutdown
                socket = null;
            }

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
