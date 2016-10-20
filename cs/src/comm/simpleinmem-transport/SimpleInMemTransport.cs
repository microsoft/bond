// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm.Service;

    public class SimpleInMemTransportBuilder : TransportBuilder<SimpleInMemTransport>
    {
        public override SimpleInMemTransport Construct()
        {
            return new SimpleInMemTransport(LayerStackProvider, new Logger(LogSink, EnableDebugLogs), new Metrics(MetricsSink));
        }
    }

    public class SimpleInMemTransport : Transport<SimpleInMemConnection, SimpleInMemListener>
    {
        object listenersLock = new object();
        IDictionary<string, SimpleInMemListener> listeners = new Dictionary<string, SimpleInMemListener>();
        readonly ILayerStackProvider layerStackProvider;
        readonly Logger logger;
        readonly Metrics metrics;

        public SimpleInMemTransport(ILayerStackProvider layerStackProvider, Logger logger, Metrics metrics)
        {
            this.layerStackProvider = layerStackProvider;
            this.logger = logger;
            this.metrics = metrics;
        }

        public override Error GetLayerStack(string uniqueId, out ILayerStack stack)
        {
            if (layerStackProvider != null)
            {
                return layerStackProvider.GetLayerStack(uniqueId, out stack, logger);
            }
            else
            {
                stack = null;
                return null;
            }
        }

        /// <summary>
        /// Connects to the <see cref="SimpleInMemListener"/> at given address.
        /// </summary>
        /// <returns>a <see cref="SimpleInMemConnection"/> that may be used to perform request operations.</returns>
        /// <exception cref="ArgumentException">the listener for given address does not exist.</exception>
        public override Task<SimpleInMemConnection> ConnectToAsync(string address, CancellationToken ct)
        {
            logger.Site().Information("Connecting to {0}.", address);
            SimpleInMemListener listener;

            lock (listenersLock)
            {
                if (!listeners.TryGetValue(address, out listener))
                {
                    var message = logger.Site().FatalAndReturnFormatted("Listener not found for address: {0}", address);
                    throw new ArgumentException(message);
                }
            }

            return Task.Run(() => listener.CreateConnectionPair().Client, ct);
        }

        /// <summary>
        /// Makes a new instance of <see cref="SimpleInMemListener"/> and returns it
        /// if one does not exist already for provided address. Returns an existing one
        /// otherwise.
        /// </summary>
        /// <returns><see cref="SimpleInMemListener"/> instance represented by address</returns>
        public override SimpleInMemListener MakeListener(string address)
        {
            SimpleInMemListener listener;

            if (!listeners.TryGetValue(address, out listener))
            {
                lock (listenersLock)
                {
                    if (!listeners.TryGetValue(address, out listener))
                    {
                        listener = new SimpleInMemListener(this, address, logger, metrics);
                        listeners.Add(address, listener);
                    }
                }
            }

            return listener;
        }

        /// <summary>
        /// Gets the <see cref="SimpleInMemListener"/> instance represented by address.
        /// </summary>
        /// <returns><see cref="SimpleInMemListener"/> instance represented by address
        /// if it exists. Otherwise null is returned.</returns>
        public Listener GetListener(string address)
        {
            SimpleInMemListener listener;
            listeners.TryGetValue(address, out listener);
            return listener;
        }

        /// <summary>
        /// Stops the underlying <see cref="SimpleInMemListener"/> instances and
        /// clears them from this transport's registry.
        /// </summary>
        public override Task StopAsync()
        {
            lock (listenersLock)
            {
                foreach (SimpleInMemListener listener in listeners.Values)
                {
                    listener.StopAsync();
                }
                listeners.Clear();
            }

            return TaskExt.CompletedTask;
        }

        public bool ListenerExists(string address)
        {
            return listeners.ContainsKey(address);
        }
    }
}