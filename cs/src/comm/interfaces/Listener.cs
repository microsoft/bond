// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Threading.Tasks;

    /// <summary>
    /// Provides data for the <see cref="Listener.Connected"/> event.
    /// </summary>
    public class ConnectedEventArgs : EventArgs
    {
        /// <summary>
        /// The connection that was just established.
        /// </summary>
        public readonly Connection Connection;

        /// <summary>
        /// A value indicating whether the connection should be rejected.
        /// </summary>
        /// <remarks>
        /// If this is set to a non-null value, the connection will be rejected
        /// with the given error.
        /// </remarks>
        public Error DisconnectError = null;

        /// <summary>
        /// Initializes a new instance of the ConnectedEventArgs class with the
        /// given <see cref="Connection"/>.
        /// </summary>
        /// <param name="connection">
        /// The connection that was just established.
        /// </param>
        public ConnectedEventArgs(Connection connection)
        {
            Connection = connection;
        }
    }

    /// <summary>
    /// Provides data for the <see cref="Listener.Connected"/> event.
    /// </summary>
    public class DisconnectedEventArgs : EventArgs
    {
        /// <summary>
        /// The connection that was just disconnected.
        /// </summary>
        public readonly Connection Connection;

        /// <summary>
        /// The reason the connection was disconnected.
        /// </summary>
        /// <remarks>
        /// The error may indicate a graceful shutdown, depending on the value
        /// of <see cref="Error.error_code"/>.
        /// </remarks>
        public readonly Error DisconnectError;

        /// <summary>
        /// Initializes a new instance of the DisconnectedEventArgs class.
        /// </summary>
        /// <param name="connection">
        /// The connection that was just disconnected.
        /// </param>
        /// <param name="error">
        /// The reason the connection was disconnected.
        /// </param>
        public DisconnectedEventArgs(Connection connection, Error error)
        {
            Connection = connection;
            DisconnectError = error;
        }
    }

    /// <summary>
    /// Listens for connections and routes messages to the appropriate
    /// <see cref="IService">services</see>.
    /// </summary>
    public abstract class Listener
    {
        /// <summary>
        /// Occurs when a new connection has being established.
        /// </summary>
        /// <remarks>
        /// The Connected event is raised when a new connection has been
        /// established but before any messages have been transmitted across
        /// it. The event can be used to setup connection-specific data
        /// structures or to reject the connection.
        /// </remarks>
        public event EventHandler<ConnectedEventArgs> Connected;

        /// <summary>
        /// Occurs when a connection has been disconnected.
        /// </summary>
        public event EventHandler<DisconnectedEventArgs> Disconnected;

        /// <summary>
        /// Adds a service to the listener.
        /// </summary>
        /// <typeparam name="T">The type of the service.</typeparam>
        /// <param name="service">The service.</param>
        /// <remarks>
        /// After a service has been added, the listener will direct messages
        /// addressed to the service's methods to this instance of the service.
        /// </remarks>
        public abstract void AddService<T>(T service) where T : IService;

        /// <summary>
        /// Removes a service from the listener.
        /// </summary>
        /// <typeparam name="T">The type of the service.</typeparam>
        /// <param name="service">The service.</param>
        /// <remarks>
        /// After a service has been removed, the listener will cease directing
        /// messages to its methods and will respond with a
        /// <see cref="ErrorCode.MethodNotFound"/> error.
        /// </remarks>
        public abstract void RemoveService<T>(T service) where T : IService;

        /// <summary>
        /// Tests whether a given method has been registered.
        /// </summary>
        /// <param name="serviceMethodName">
        /// The fully qualified method name in the Bond namespace.
        /// </param>
        /// <returns>
        /// <c>true</c> if the method has been registered; otherwise
        /// <c>false</c>.
        /// </returns>
        public abstract bool IsRegistered(string serviceMethodName);

        /// <summary>
        /// Starts an operation to begin accepting connections and dispatching
        /// messages to services.
        /// </summary>
        /// <returns>
        /// A task that represents the asynchronous operation.
        /// </returns>
        public abstract Task StartAsync();

        /// <summary>
        /// Starts an operation to stop listening.
        /// </summary>
        /// <returns>
        /// A task that represents the asynchronous operation.
        /// </returns>
        public abstract Task StopAsync();
    }
}
