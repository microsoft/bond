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
        private EpoxyTransport m_parentTransport;
        private System.Net.Sockets.TcpListener m_listener;
        private ServiceHost m_serviceHost;

        private object m_connectionsLock = new object();
        private HashSet<EpoxyConnection> m_connections;

        private Task m_acceptTask;

        private CancellationTokenSource m_shutdownTokenSource;

        public EpoxyListener(EpoxyTransport parentTransport, IPEndPoint listenEndpoint)
        {
            m_parentTransport = parentTransport;
            m_listener = new System.Net.Sockets.TcpListener(listenEndpoint);
            m_serviceHost = new ServiceHost(parentTransport);
            m_connections = new HashSet<EpoxyConnection>();
            m_shutdownTokenSource = new CancellationTokenSource();
        }

        public IPEndPoint ListenEndpoint
        {
            get
            {
                return (IPEndPoint)m_listener.LocalEndpoint;
            }
        }

        public override string ToString()
        {
            return $"EpoxyListener({ListenEndpoint})";
        }

        public override bool IsRegistered(string serviceMethodName)
        {
            return m_serviceHost.IsRegistered(serviceMethodName);
        }

        public override void AddService<T>(T service)
        {
            Log.Information("{0}.{1}: Adding {2}.", this, nameof(AddService), typeof(T).Name);
            m_serviceHost.Register(service);
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public override Task StartAsync()
        {
            m_listener.Start();
            m_acceptTask = Task.Run(() => AcceptAsync(m_shutdownTokenSource.Token), m_shutdownTokenSource.Token);
            return TaskExt.CompletedTask;
        }

        public override Task StopAsync()
        {
            m_shutdownTokenSource.Cancel();
            m_listener.Stop();

            return m_acceptTask;
        }

        private async Task AcceptAsync(CancellationToken t)
        {
            Log.Information("{0}.{1}: Accepting connections...", this, nameof(AcceptAsync));
            while (!t.IsCancellationRequested)
            {
                Socket socket = null;

                try
                {
                    socket = await m_listener.AcceptSocketAsync();
                    var connection = new EpoxyConnection(m_parentTransport, socket, m_serviceHost, ConnectionType.Server);
                    socket = null; // connection now owns the socket and will close it

                    var connectedEventArgs = new ConnectedEventArgs(connection);
                    Error disconnectError = OnConnected(connectedEventArgs);

                    if (disconnectError != null)
                    {
                        Log.Information("Rejecting connection {0} because {1}:{2}", connection,
                            disconnectError.error_code, disconnectError.message);
                        await connection.SendProtocolErrorAsync(ProtocolErrorCode.CONNECTION_REJECTED, disconnectError);
                        await connection.StopAsync();
                        continue;
                    }

                    lock (m_connectionsLock)
                    {
                        m_connections.Add(connection);
                    }

                    connection.Start();
                    Log.Debug("{0}.{1}: Accepted connection from {2}.", 
                        this, nameof(AcceptAsync), connection.RemoteEndPoint);
                }
                catch (SocketException ex)
                {
                    Log.Fatal(ex, "{0}.{1}: Accept failed with error {2}.",
                        this, nameof(AcceptAsync), ex.SocketErrorCode);

                    ShutdownSocketSafe(socket);
                }
                catch (ObjectDisposedException)
                {
                    ShutdownSocketSafe(socket);

                    // TODO: ignoring this exception is needed during shutdown,
                    //       but there should be a cleaner way. We should
                    //       switch to having a proper life-cycle for a
                    //       connection.
                }
            }
            Log.Information("{0}.{1}: Shutting down.", this, nameof(AcceptAsync));
        }

        private static void ShutdownSocketSafe(Socket socket)
        {
            try
            {
                socket?.Shutdown(SocketShutdown.Both);
                socket?.Close();
            }
            catch (SocketException ex)
            {
                // We tried to cleanly shutdown the socket, oh well.
                Log.Debug(ex, "Exception encountered when shutting down a socket.");
            }
        }
    }
}
