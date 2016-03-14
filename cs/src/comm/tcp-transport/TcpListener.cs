// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    using System;
    using System.Collections.Generic;
    using System.Net;
    using System.Net.Sockets;
    using System.Threading;
    using System.Threading.Tasks;

    public class TcpListener : Listener
    {
        private System.Net.Sockets.TcpListener m_listener;
        private TcpServiceHost m_serviceHost;

        private object m_connectionsLock = new object();
        private HashSet<TcpConnection> m_connections;

        private Task m_acceptTask;

        private CancellationTokenSource m_shutdownTokenSource;

        public TcpListener(IPEndPoint listenEndpoint)
        {
            m_listener = new System.Net.Sockets.TcpListener(listenEndpoint);
            m_serviceHost = new TcpServiceHost();
            m_connections = new HashSet<TcpConnection>();
            m_shutdownTokenSource = new CancellationTokenSource();
        }

        public IPEndPoint ListenEndpoint
        {
            get
            {
                return (IPEndPoint)m_listener.LocalEndpoint;
            }
        }

        public override void AddService<T>(T server)
        {
            m_serviceHost.Register((IService)server);
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public override void SetUnhandledExceptionHandler(UnhandledExceptionHandler handler)
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
            while (!t.IsCancellationRequested)
            {
                try
                {
                    TcpClient client = await m_listener.AcceptTcpClientAsync();
                    var connection = new TcpConnection(client, m_serviceHost, ConnectionType.Server);

                    lock (m_connectionsLock)
                    {
                        m_connections.Add(connection);
                    }

                    connection.Start();
                }
                catch (SocketException ex)
                {
                    System.Diagnostics.Debug.Write("Accept failed with error " + ex.SocketErrorCode);
                }
                catch (ObjectDisposedException)
                {
                    // TODO: this is needed during shutdown, but there should be a cleaner way
                }
            }
        }
    }
}
