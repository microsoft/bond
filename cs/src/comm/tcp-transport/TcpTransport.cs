// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    using System;
    using System.Net;
    using System.Net.Sockets;
    using System.Threading;
    using System.Threading.Tasks;

    public class TcpTransportBuilder : TransportBuilder<TcpTransport>
    {
        public override TransportBuilder<TcpTransport> AddDeserializer<TReader>(Type type, Deserializer<TReader> deserializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddDeserializers<TReader>(System.Collections.Generic.Dictionary<Type, Deserializer<TReader>> deserializers)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddSerializer<TWriter>(Type type, Serializer<TWriter> serializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddSerializers<TWriter>(System.Collections.Generic.Dictionary<Type, Serializer<TWriter>> serializers)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> SetDefaultTransportArgs(TransportArgs defaults)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> SetLayerStack<TLayerData>(LayerStack<TLayerData> layerStack)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> SetUnhandledExceptionHandler(UnhandledExceptionHandler handler)
        {
            throw new NotImplementedException();
        }

        public override TcpTransport Construct()
        {
            return new TcpTransport();
        }
    }

    public class TcpTransport : Transport
    {
        public const int DefaultPort = 25188;

        private TcpTransportArgs m_defaultTransportArgs = new TcpTransportArgs();

        public override TransportArgs DefaultTransportArgs
        {
            get
            {
                return m_defaultTransportArgs.Clone<TcpTransportArgs>();
            }
        }

        public async override Task<Connection> ConnectToAsync(string address, CancellationToken ct)
        {
            return await ConnectToAsync(ParseStringAddress(address), ct);
        }

        public Task<TcpConnection> ConnectToAsync(IPEndPoint endpoint)
        {
            return ConnectToAsync(endpoint, CancellationToken.None);
        }

        public async Task<TcpConnection> ConnectToAsync(IPEndPoint endpoint, CancellationToken ct)
        {
            var tcpClient = new TcpClient();
            await tcpClient.ConnectAsync(endpoint.Address, endpoint.Port);

            // TODO: keep these in some master collection for shutdown
            var tcpConnection = new TcpConnection(tcpClient, ConnectionType.Client);
            tcpConnection.Start();
            return tcpConnection;
        }

        public override Listener MakeListener(string address)
        {
            return MakeListener(ParseStringAddress(address));
        }

        public TcpListener MakeListener(IPEndPoint address)
        {
            return new TcpListener(address);
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
    }
}
