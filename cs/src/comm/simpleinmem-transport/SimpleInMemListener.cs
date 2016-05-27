// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using Bond.Comm.Service;
    using System.Collections.Generic;
    using System.Threading.Tasks;

    public class SimpleInMemListener : Listener
    {
        private ServiceHost serviceHost;
        private string address;
        private SimpleInMemConnection connection;
        private readonly string logname;

        public SimpleInMemListener(SimpleInMemTransport parentTransport, string address)
        {
            this.address = address;
            serviceHost = new ServiceHost(parentTransport);
            connection = new SimpleInMemConnection(serviceHost, this, ConnectionType.Server);
            logname = $"{nameof(SimpleInMemListener)}({address})";
        }

        public override bool IsRegistered(string serviceMethodName)
        {
            return serviceHost.IsRegistered(serviceMethodName);
        }

        public override void AddService<T>(T service)
        {
            Log.Information("{0}.{1}: Adding {2}.", logname, nameof(AddService), typeof(T).Name);
            serviceHost.Register(service);
        }

        public override void RemoveService<T>(T service)
        {
            serviceHost.Deregister((IService)service);
        }

        public override Task StartAsync()
        {
            connection.Start();
            return TaskExt.CompletedTask;
        }

        public override Task StopAsync()
        {
            return connection.StopAsync();
        }

        internal void AddClient(SimpleInMemConnection client)
        {
            var connectedEventArgs = new ConnectedEventArgs(client);
            Error disconnectError = OnConnected(connectedEventArgs);

            if (disconnectError != null)
            {
                Log.Information("{0}.{1}: Rejecting connection {2} because {3}:{4}.",
                    logname, nameof(AddClient), client.Id, disconnectError.error_code, disconnectError.message);
                throw new SimpleInMemProtocolErrorException(
                    "Connection rejected",
                    details: disconnectError,
                    innerException: null);
            }

            connection.AddClientConnection(client);
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
