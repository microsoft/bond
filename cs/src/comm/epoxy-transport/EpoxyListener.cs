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

        public override async Task StopAsync()
        {
            // Request that the accept loop stop
            shutdownTokenSource.Cancel();
            // Stop listening. This causes the accept loop's wait for an
            // incomming socket to fail, which then causes the accept loop to
            // look at the value of shutdownTokenSource.Token.
            listener.Stop();

            // Wait for the accept loop to exit. Once it has exited, no new
            // connections will be accepted, so we can close the outstanding
            // ones.
            await acceptTask;

            // Collect the connections to stop.
            HashSet<EpoxyConnection> connectionsToClose;
            lock (connectionsLock)
            {
                connectionsToClose = new HashSet<EpoxyConnection>(connections);
                connections.Clear();
            }

            // Stop all outstanding connections. EpoxyConnection shutdown is
            // idempotent, so it's safe to call even if someone else called
            // StopAsync.
            var connectionShutdownTasks = new Task[connectionsToClose.Count];
            int idx = 0;
            foreach (var connection in connectionsToClose)
            {
                connectionShutdownTasks[idx] = connection.StopAsync();
                ++idx;
            }

            await Task.WhenAll(connectionShutdownTasks);
        }

        internal Error InformConnected(EpoxyConnection connection)
        {
            var args = new ConnectedEventArgs(connection);
            return OnConnected(args);
        }

        internal void InformDisconnected(EpoxyConnection connection, Error error)
        {
            lock (connectionsLock)
            {
                connections.Remove(connection);
            }

            var args = new DisconnectedEventArgs(connection, error);
            OnDisconnected(args);
        }

        private async Task AcceptAsync(CancellationToken t)
        {
            logger.Site().Information("Accepting connections on {0}", ListenEndpoint);
            while (!t.IsCancellationRequested)
            {
                // We only need to handle exceptions from AcceptSocketAsync. Exceptions from other
                // code are handled via StartClientConnectionAsync and LogClientStartResult.
                try
                {
                    // We don't directly await this call so that we have a Task<Socket> to pass to
                    // StartClientConnectionAsync without having to wrap the socket in a dummy task.
                    Task<Socket> socket = listener.AcceptSocketAsync();
                    await socket;

                    logger.Site().Debug("Accepted TCP connection from {0}.", socket.Result.RemoteEndPoint);

                    // Disabling CS4014 "Because this call is not awaited,
                    // execution of the current method continues before the
                    // call is completed.Consider applying the 'await' operator
                    // to the result of the call.", as we've attached a logger
                    // task that doesn't need to be observed.
                    #pragma warning disable 4014
                    // Don't need to wait for the connection to get fully established before
                    // accepting the next one, so fire and forget (with logging) StartAsync.
                    StartClientConnectionAsync(socket)
                        .ContinueWith(startTask => LogClientStartResult(socket.Result.RemoteEndPoint, startTask));
                    #pragma warning restore 4014
                }
                catch (SocketException ex)
                {
                    logger.Site().Error(ex, "Accept failed with error {0}.", ex.SocketErrorCode);
                    // Continue on and try to accept the next connection.
                }
                catch (ObjectDisposedException)
                {
                    // listener.AcceptSocketAsync() can throw if the listener has been shutdown. If
                    // that's the case, we'll exit the loop because the cancellation token will have
                    // been signaled before the listener was shut down.
                    //
                    // Socket.RemoteEndPoint can throw if the socket was closed by the remote party.
                    // If that's the case, continue on and try to accept the next connection.
                }
            }

            logger.Site().Information("Stopping listening on {0}", ListenEndpoint);
        }

        async Task StartClientConnectionAsync(Task<Socket> socketTask)
        {
            // If this throws, it will have cleaned up before getting here, so we don't need to
            // handle cleanup.
            EpoxyNetworkStream epoxyStream = await EpoxyNetworkStream.MakeAsync(
                socketFunc: () => socketTask,
                streamFunc: socket => EpoxyNetworkStream.MakeServerStreamAsync(socket, tlsConfig, logger),
                timeoutConfig: timeoutConfig,
                logger: logger);

            // We now have a completely established EpoxyNetworkStream that will eventually need to
            // be shutdown. None of the code between here and adding the connection to our collection
            // will throw (for network I/O reasons), so we'll safely save the connection. (The
            // intervening code may throw for things like OOM. If it does, we've got bigger problems
            // that we likely won't be able to recover from anyway. Ignoring that contingency for
            // now.)
            var connection = EpoxyConnection.MakeServerConnection(
                parentTransport,
                this,
                serviceHost,
                epoxyStream,
                logger,
                metrics);

            lock (connectionsLock)
            {
                connections.Add(connection);
            }

            logger.Site().Debug("Setup server-side connection for {0}. Starting Epoxy handshake.", connection.RemoteEndPoint);
            await connection.StartAsync();
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
