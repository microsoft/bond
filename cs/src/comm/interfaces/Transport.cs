// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System.Threading;
    using System.Threading.Tasks;

    /// <summary>
    /// Provides a convenient way to initialize and create
    /// <see cref="Transport{TConnection,TListener}">Transports</see>.
    /// </summary>
    /// <typeparam name="TTransport">
    /// The type of transport this builder can build.
    /// </typeparam>
    public abstract class TransportBuilder<TTransport>
    {
        protected ILayerStackProvider LayerStackProvider { get; private set; }

        protected ILogSink LogSink { get; private set; }

        protected bool EnableDebugLogs { get; private set; } = false;

        protected IMetricsSink MetricsSink { get; private set; }

        /// <summary>
        /// Set the layer stack.
        /// </summary>
        /// <param name="layerStackProvider">The layer stack provider. May be null</param>
        /// <returns>The builder.</returns>
        /// <remarks>
        /// The layer stack provider supplies the set of layers to be used
        /// by the built transport. May be null.
        /// </remarks>
        public virtual TransportBuilder<TTransport> SetLayerStackProvider(ILayerStackProvider layerStackProvider)
        {
            this.LayerStackProvider = layerStackProvider;
            return this;
        }

        public virtual TransportBuilder<TTransport> SetLogSink(ILogSink logSink)
        {
            LogSink = logSink;
            return this;
        }

        public virtual TransportBuilder<TTransport> EnableDebugLogging(bool enable)
        {
            EnableDebugLogs = enable;
            return this;
        }

        public virtual TransportBuilder<TTransport> SetMetricsSink(IMetricsSink metricsSink)
        {
            MetricsSink = metricsSink;
            return this;
        }

        /// <summary>
        /// Create the transport.
        /// </summary>
        /// <returns>
        /// An instance of <typeparamref name="TTransport"/>.
        /// </returns>
        public abstract TTransport Construct();
    }

    /// <summary>
    /// Defines the basic requirements of a Transport.
    /// </summary>
    public abstract class Transport<TConnection, TListener> where TConnection : Connection where TListener : Listener
    {
        /// <summary>
        /// Get a layer stack instance for this transport.
        /// </summary>
        /// <param name="uniqueId">A unique ID identifying the request or connection that triggered this call. Used in
        /// layer-originated errors.</param>
        /// <param name="stack">The layer stack instance. Will be null if an error is returned.</param>
        /// <returns>An error if one occurred, null otherwise.</returns>
        public abstract Error GetLayerStack(string uniqueId, out ILayerStack stack);

        /// <summary>
        /// Starts an asynchronous operation to connect to the specified
        /// address.
        /// </summary>
        /// <param name="address">The address to connect to.</param>
        /// <returns>A task of the <see cref="Connection"/>.</returns>
        /// <remarks>
        /// Each transport uses its own format for addresses. Consult the
        /// concrete implementation that is being used for details.
        /// </remarks>
        public virtual Task<TConnection> ConnectToAsync(string address)
        {
            return ConnectToAsync(address, CancellationToken.None);
        }

        /// <summary>
        /// Starts an asynchronous operation to connect to the specified
        /// address.
        /// </summary>
        /// <param name="address">The address to connect to.</param>
        /// <param name="ct">The cancellation token.</param>
        /// <returns>A task of the <see cref="Connection"/>. The returned connection may be null.</returns>
        /// <remarks>
        /// Each transport uses its own format for addresses. Consult the
        /// concrete implementation that is being used for details.
        /// </remarks>
        public abstract Task<TConnection> ConnectToAsync(string address, CancellationToken ct);

        /// <summary>
        /// Creates a <see cref="Listener"/> that can be used to accept
        /// connections.
        /// </summary>
        /// <param name="address">The address to listen on.</param>
        /// <returns>
        /// A Listener that when started will listen on
        /// <paramref name="address"/>.
        /// </returns>
        /// <remarks>
        /// Each transport uses its own format for addresses. Consult the
        /// concrete implementation that is being used for details.
        /// </remarks>
        public abstract TListener MakeListener(string address);

        /// <summary>
        /// Starts an asynchronous operation to close all connections and stop
        /// all listeners.
        /// </summary>
        /// <returns>
        /// A task that represents the shutting down the connections and
        /// listeners.
        /// </returns>
        /// <remarks>
        /// Stopping a transport does not drain the connection of outstanding
        /// messages. They may have been sent/received or not. Outstanding
        /// requests expecting responses will be completed with failures.
        /// </remarks>
        public abstract Task StopAsync();
    }
}
