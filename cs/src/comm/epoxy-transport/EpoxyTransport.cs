// Copyright (c) Microsoft. All rights reserved.w
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Net;
    using System.Net.Sockets;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Service;

    public class EpoxyTransportBuilder : TransportBuilder<EpoxyTransport>
    {
        public override EpoxyTransport Construct()
        {
            return new EpoxyTransport(LayerStackProvider, LogSink, EnableDebugLogs);
        }
    }

    public class EpoxyTransport : Transport
    {
        public const int DefaultPort = 25188;

        readonly ILayerStackProvider layerStackProvider;
        readonly Logger logger;

        public EpoxyTransport(ILayerStackProvider layerStackProvider, ILogSink logSink, bool enableDebugLogs)
        {
            // Layer stack provider may be null
            this.layerStackProvider = layerStackProvider;
            // Log Sink may be null
            logger = new Logger(logSink, enableDebugLogs);
        }

        public override Error GetLayerStack(out ILayerStack stack)
        {
            if (layerStackProvider != null)
            {
                return layerStackProvider.GetLayerStack(out stack);
            }
            else
            {
                stack = null;
                return null;
            }
        }

        public override Task<Connection> ConnectToAsync(string address, CancellationToken ct)
        {
            return ConnectToAsync(ParseStringAddress(address), ct).Upcast<EpoxyConnection, Connection>();
        }

        public Task<EpoxyConnection> ConnectToAsync(IPEndPoint endpoint)
        {
            return ConnectToAsync(endpoint, CancellationToken.None);
        }

        public async Task<EpoxyConnection> ConnectToAsync(IPEndPoint endpoint, CancellationToken ct)
        {
            logger.Site().Information("Connecting to {0}.", endpoint);

            Socket socket = MakeClientSocket();
            await Task.Factory.FromAsync(socket.BeginConnect, socket.EndConnect, endpoint, state: null);

            // TODO: keep these in some master collection for shutdown
            var connection = EpoxyConnection.MakeClientConnection(this, socket, logger);
            await connection.StartAsync();
            return connection;
        }

        public override Listener MakeListener(string address)
        {
            return MakeListener(ParseStringAddress(address));
        }

        public EpoxyListener MakeListener(IPEndPoint address)
        {
            return new EpoxyListener(this, address, logger);
        }

        public override Task StopAsync()
        {
            return TaskExt.CompletedTask;
        }

        public static IPEndPoint ParseStringAddress(string address)
        {
            if (string.IsNullOrEmpty(address))
            {
                throw new ArgumentException("Address cannot be null or empty", nameof(address));
            }

            int portStartIndex = address.IndexOf(':');

            string ipAddressPart;

            if (portStartIndex == -1)
            {
                ipAddressPart = address;
            }
            else
            {
                ipAddressPart = address.Substring(0, portStartIndex);
            }

            IPAddress ipAddr;
            if (!IPAddress.TryParse(ipAddressPart, out ipAddr))
            {
                throw new ArgumentException("Couldn't parse IP address from \"" + address + "\"", nameof(address));
            }

            int port;
            if (portStartIndex == -1)
            {
                port = DefaultPort;
            }
            else
            {
                string portPart = address.Substring(portStartIndex + 1);
                if (!int.TryParse(portPart, out port))
                {
                    throw new ArgumentException("Couldn't parse port from \"" + address + "\"", nameof(address));
                }
            }

            return new IPEndPoint(ipAddr, port);
        }

        private Socket MakeClientSocket()
        {
            return new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        }
    }
}
