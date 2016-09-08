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
        protected readonly Logger logger;
        protected readonly Metrics metrics;

        /// <summary>
        /// Occurs when a new connection has being established.
        /// </summary>
        /// <remarks>
        /// <para>
        /// The Connected event is raised when a new connection has been
        /// established but before any messages have been transmitted across
        /// it. The event can be used to setup connection-specific data
        /// structures or to reject the connection.
        /// </para>
        /// <para>
        /// Connected event handlers must not throw exceptions. The Listener
        /// has no sensible way to handle the exception, and there is no way to
        /// bubble the exception back to the application code. If an exception
        /// is thrown, the process will be terminated.
        /// </para>
        /// </remarks>
        public event EventHandler<ConnectedEventArgs> Connected;

        /// <summary>
        /// Occurs when a connection has been disconnected.
        /// </summary>
        /// <remarks>
        /// <para>
        /// Disconnected event handlers must not throw exceptions. The Listener
        /// has no sensible way to handle the exception, and there is no way to
        /// bubble the exception back to the application code. If an exception
        /// is thrown, the process will be terminated.
        /// </para>
        /// </remarks>
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
        /// <see cref="ErrorCode.METHOD_NOT_FOUND"/> error.
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

        public Listener(Logger logger, Metrics metrics)
        {
            if (logger == null)
            {
                throw new ArgumentNullException(nameof(logger));
            }

            this.logger = logger;
            this.metrics = metrics;
        }

        protected virtual Error OnConnected(ConnectedEventArgs args)
        {
            if (args == null)
            {
                throw new ArgumentNullException(nameof(args));
            }

            // make a local copy so we can iterate through it even if it gets
            // changed while we're iterating
            EventHandler<ConnectedEventArgs> connectedEventHandler = Connected;

            if (connectedEventHandler != null)
            {
                // invoke one at a time so that we can check whether
                // DisconnectError was set to non-null and halt processing the
                // moment it is set to null
                foreach (var handler in connectedEventHandler.GetInvocationList())
                {
                    var connectedHandler = (EventHandler<ConnectedEventArgs>) handler;

                    try
                    {
                        connectedHandler.Invoke(this, args);
                    }
                    catch (Exception ex)
                    {
                        logger.Site().Error(ex, "Exception in handler for connection {0}: {1}", args.Connection, ex.Message);
                        args.DisconnectError = Errors.MakeInternalServerError(ex, args.Connection.Id, includeDetails: false);
                    }

                    if (args.DisconnectError != null)
                    {
                        break;
                    }
                }
            }

            return args.DisconnectError;
        }

        protected virtual void OnDisconnected(DisconnectedEventArgs args)
        {
            if (args == null)
            {
                throw new ArgumentNullException(nameof(args));
            }

            // make a local copy so we can invoke it even if it gets changed
            // while we're invoking
            EventHandler<DisconnectedEventArgs> disconnectedEventHandler = Disconnected;

            try
            {
                disconnectedEventHandler?.Invoke(this, args);
            }
            catch (Exception ex)
            {
                logger.Site().Error(ex, "Exception in handler for connection {0}: {1}", args.Connection, ex.Message);
            }
        }
    }
}
