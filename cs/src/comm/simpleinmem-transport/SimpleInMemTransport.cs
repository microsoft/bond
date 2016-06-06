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

        public async override Task<Connection> ConnectToAsync(string address, CancellationToken ct)
        {
            Log.Information("{0}.{1}: Connecting to {2}.",
                nameof(SimpleInMemTransport), nameof(ConnectToAsync), address);
            SimpleInMemListener listener;

            lock (listenersLock)
            {
                if (!listeners.TryGetValue(address, out listener))
                {
                    var errorFormat = "{0}.{1}: Listener not found for address: {2}";
                    var message = LogUtil.FatalAndReturnFormatted(errorFormat,
                        nameof(SimpleInMemTransport), nameof(ConnectToAsync), address);
                    throw new ArgumentException(message);
                }
            }

            return await Task.Run<Connection>(() =>
            {
                return listener.CreateConnectionPair().Client;
            }, ct);
        }

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

        public Listener GetListener(string address)
        {
            SimpleInMemListener listener;
            listeners.TryGetValue(address, out listener);
            return listener;
        }

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

        public SimpleInMemListener RemoveListener(string address)
        {
            SimpleInMemListener listener;

            if (listeners.TryGetValue(address, out listener))
            {
                listeners.Remove(address);
            }

            return listener;
        }
    }
}
