// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System.Threading;
    using System.Threading.Tasks;

    /// <summary>
    /// Defines the operations that a connection that supports the
    /// request/response messageing pattern must implement.
    /// </summary>
    public interface IRequestResponseConnection
    {
        /// <summary>
        /// Starts an asynchronous operation to invoke a request/response
        /// method.
        /// </summary>
        /// <typeparam name="TRequest">The type of the request.</typeparam>
        /// <typeparam name="TResponse">The type of the response.</typeparam>
        /// <param name="serviceName">
        /// The fully qualified name in the Bond namespace of the service to
        /// invoke.
        /// </param>
        /// <param name="methodName">
        /// The name of the method to invoke within the indicated service.
        /// </param>
        /// <param name="message">The message to send.</param>
        /// <param name="ct">
        /// The cancellation token for cooperative cancellation.
        /// </param>
        /// <returns>
        /// A task that represents the result of invoking the method. This is
        /// usually the response to the method, but may be an error.
        /// </returns>
        Task<IMessage<TResponse>> RequestResponseAsync<TRequest, TResponse>(string serviceName, string methodName,
                                                                            IMessage<TRequest> message, CancellationToken ct);
    }

    /// <summary>
    /// Defines the operations that a connection that supports the event
    /// messaging pattern must implement.
    /// </summary>
    public interface IEventConnection
    {
        /// <summary>
        /// Starts an asynchronous operation to invoke an event method.
        /// </summary>
        /// <typeparam name="TPayload">The type of the event.</typeparam>
        /// <param name="serviceName">
        /// The fully qualified name in the Bond namespace of the service to
        /// invoke.
        /// </param>
        /// <param name="methodName">
        /// The name of the method to invoke within the indicated service.
        /// </param>
        /// <param name="message">The message to send.</param>
        /// <returns>A task representing the asynchronous operation.</returns>
        /// <remarks>
        /// Event methods cannot send responses or error. However, the returned
        /// task may represent an error if there was a local error sending the
        /// message.
        /// </remarks>
        Task FireEventAsync<TPayload>(string serviceName, string methodName, IMessage<TPayload> message);
    }

    /// <summary>
    /// Defines a basic requirements of a connection.
    /// </summary>
    public abstract class Connection
    {
        public readonly ConnectionMetrics ConnectionMetrics = Metrics.StartConnectionMetrics();

        public string Id => ConnectionMetrics.connection_id;

        /// <summary>
        /// Starts an asynchronous operation to close a connection.
        /// </summary>
        /// <returns>A task representing the asynchronous operation.</returns>
        /// <remarks>
        /// Stopping a connection does not drain the connection of outstanding
        /// messages. They may have been sent/received or not. Outstanding
        /// requests expecting responses will be completed with failures.
        /// </remarks>
        public abstract Task StopAsync();
    }
}
