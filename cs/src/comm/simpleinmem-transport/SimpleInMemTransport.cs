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
            return new SimpleInMemTransport(LayerStack);
        }
    }

    public class SimpleInMemTransport : Transport
    {
        object listenersLock = new object();
        IDictionary<string, SimpleInMemListener> listeners = new Dictionary<string, SimpleInMemListener>();
        readonly ILayerStack layerStack;

        public SimpleInMemTransport(ILayerStack layerStack)
        {
            this.layerStack = layerStack;
        }

        public override ILayerStack LayerStack
        {
            get
            {
                return this.layerStack;
            }
        }

        /// <summary>
        /// Connects to the <see cref="SimpleInMemListener"/> at given address.
        /// </summary>
        /// <returns>a <see cref="SimpleInMemConnection"/> that may be used to perform request operations.</returns>
        /// <exception cref="ArgumentException">the listener for given address does not exist.</exception>
        public async override Task<Connection> ConnectToAsync(string address, CancellationToken ct)
        {
            Log.Site().Information("Connecting to {0}.", address);
            SimpleInMemListener listener;

            lock (listenersLock)
            {
                if (!listeners.TryGetValue(address, out listener))
                {
                    var message = Log.Site().FatalAndReturnFormatted("Listener not found for address: {0}", address);
                    throw new ArgumentException(message);
                }
            }

            return await Task.Run<Connection>(() =>
            {
                return listener.CreateConnectionPair().Client;
            }, ct);
        }

        /// <summary>
        /// Makes a new instance of <see cref="SimpleInMemListener"/> and returns it
        /// if one does not exist already for provided address. Returns an existing one
        /// otherwise.
        /// </summary>
        /// <returns><see cref="SimpleInMemListener"/> instance represented by address</returns>
        public override Listener MakeListener(string address)
        {
            SimpleInMemListener listener;

            if (!listeners.TryGetValue(address, out listener))
            {
                lock (listenersLock)
                {
                    if (!listeners.TryGetValue(address, out listener))
                    {
                        listener = new SimpleInMemListener(this, address);
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