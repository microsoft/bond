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
        readonly EpoxyTransport parentTransport;
        readonly EpoxyServerTlsConfig tlsConfig;
        readonly EpoxyTransport.TimeoutConfig timeoutConfig;
        readonly TcpListener listener;
        readonly ServiceHost serviceHost;

        readonly object connectionsLock = new object();
        readonly HashSet<EpoxyConnection> connections;

        // used to request that other components start shutting down
        readonly CancellationTokenSource shutdownTokenSource;

        public IPEndPoint ListenEndpoint { get; }

        Task acceptTask;

        public EpoxyListener(
            EpoxyTransport parentTransport,
            IPEndPoint listenEndpoint,
            EpoxyServerTlsConfig tlsConfig,
            EpoxyTransport.TimeoutConfig timeoutConfig,
            Logger logger, Metrics metrics) : base(logger, metrics)
        {
            Debug.Assert(parentTransport != null);
            Debug.Assert(listenEndpoint != null);

            this.parentTransport = parentTransport;

            // will be null if not using TLS
            this.tlsConfig = tlsConfig;
            this.timeoutConfig = timeoutConfig;

            listener = new TcpListener(listenEndpoint);
            serviceHost = new ServiceHost(logger);
            connections = new HashSet<EpoxyConnection>();
            shutdownTokenSource = new CancellationTokenSource();

            ListenEndpoint = listenEndpoint;
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
                try
                {
                    Socket socket = await listener.AcceptSocketAsync();
                    EndPoint remoteEndpoint = socket.RemoteEndPoint;
                    logger.Site().Debug("Accepted connection from {0}.", remoteEndpoint);

                    // Disabling CS4014 "Because this call is not awaited,
                    // execution of the current method continues before the
                    // call is completed.Consider applying the 'await' operator
                    // to the result of the call.", as we've attached a logger
                    // task that doesn't need to be observed.
                    #pragma warning disable 4014
                    // Don't need to wait for the connection to get fully established before
                    // accepting the next one, so fire and forget (with logging) StartAsync.
                    StartClientConnectionAsync(socket)
                        .ContinueWith(startTask => LogClientStartResult(remoteEndpoint, startTask));
                    #pragma warning restore 4014
                }
                catch (SocketException ex)
                {
                    logger.Site().Error(ex, "Accept failed with error {0}.", ex.SocketErrorCode);
                    // continue on to accept the next connection
                }
                catch (ObjectDisposedException)
                {
                    // continue on to accept the next connection
                }
            }

            logger.Site().Information("Shutting down connection on {0}", ListenEndpoint);
        }

        async Task StartClientConnectionAsync(Socket socket)
        {
            EpoxyNetworkStream epoxyStream = null;

            try
            {
                EpoxyTransport.ConfigureSocketKeepAlive(socket, timeoutConfig, logger);

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

                logger.Site().Debug("Setup server-side connection for {0}. Starting Epoxy handshake.", connection.RemoteEndPoint);
                await connection.StartAsync();
            }
            catch (Exception)
            {
                ShutdownSocketSafe(socket, epoxyStream);
                throw;
            }
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

        private void LogClientStartResult(EndPoint remoteEndPoint, Task startTask)
        {
            if (startTask.IsFaulted)
            {
                logger.Site().Error(
                    startTask.Exception,
                    "Starting connection for {0} failed",
                    remoteEndPoint);
            }
            else if (startTask.IsCanceled)
            {
                logger.Site().Debug("Starting connection for {0} was canceled", remoteEndPoint);
            }
            else
            {
                logger.Site().Debug("Finished starting connection for {0}", remoteEndPoint);
            }
        }
    }
}
