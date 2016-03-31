// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Threading.Tasks;

    public class ConnectedEventArgs : EventArgs
    {
        public readonly Connection Connection;

        // set to non-null to reject the connection with the specified error
        public Error DisconnectError = null;

        public ConnectedEventArgs(Connection connection)
        {
            Connection = connection;
        }
    }

    public class DisconnectedEventArgs : EventArgs
    {
        public readonly Connection Connection;

        // the reason the connection was disconnected
        public readonly Error DisconnectError;

        public DisconnectedEventArgs(Connection connection, Error error)
        {
            Connection = connection;
            DisconnectError = error;
        }
    }

    public abstract class Listener
    {
        // Called whenever a new connection is being established.
        public event EventHandler<ConnectedEventArgs> Connected;

        // Called whenever an existing connection has disconnected.
        public event EventHandler<DisconnectedEventArgs> Disconnected;

        public abstract void AddService<T>(T server);
        public abstract void RemoveService<T>(T service);

        public abstract Task StartAsync();
        public abstract Task StopAsync();
    }
}
