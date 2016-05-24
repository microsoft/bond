// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using Service;
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;

    public class SimpleInMemTransportBuilder : TransportBuilder<SimpleInMemTransport>
    {
        public override SimpleInMemTransport Construct()
        {
            return new SimpleInMemTransport(LayerStack);
        }
    }

    public class SimpleInMemTransport : Transport
    {
        IDictionary<string, SimpleInMemListener> m_listeners = new Dictionary<string, SimpleInMemListener>();
        object m_lock = new object();
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

            lock (m_lock)
            {
                if (!m_listeners.TryGetValue(address, out listener))
                {
                    var errorFormat = "{0}.{1}: Listener not found for address: {2}";
                    var message = LogUtil.FatalAndReturnFormatted(errorFormat,
                        nameof(SimpleInMemTransport), nameof(ConnectToAsync), address);
                    throw new ArgumentException(message);
                }
            }

            return await Task.Run<Connection>(() =>
            {
                var connection = new SimpleInMemConnection(this, (SimpleInMemListener)GetListener(address), ConnectionType.Client);
                listener.AddClient(connection);
                connection.Start();
                return connection;
            }, ct);
        }

        public override Listener MakeListener(string address)
        {
            SimpleInMemListener listener;

            if (!m_listeners.TryGetValue(address, out listener))
            {
                lock (m_lock)
                {
                    if (!m_listeners.TryGetValue(address, out listener))
                    {
                        listener = new SimpleInMemListener(this, address);
                        m_listeners.Add(address, listener);
                    }
                }
            }

            return listener;
        }

        public Listener GetListener(string address)
        {
            SimpleInMemListener listener;
            m_listeners.TryGetValue(address, out listener);
            return listener;
        }

        public override Task StopAsync()
        {
            lock (m_lock)
            {
                foreach (SimpleInMemListener listener in m_listeners.Values)
                {
                    listener.StopAsync();
                }
                m_listeners.Clear();
            }

            return TaskExt.CompletedTask;
        }

        public bool ListenerExists(string address)
        {
            return m_listeners.ContainsKey(address);
        }

        public SimpleInMemListener RemoveListener(string address)
        {
            SimpleInMemListener listener;

            if (m_listeners.TryGetValue(address, out listener))
            {
                m_listeners.Remove(address);
            }

            return listener;
        }
    }
}
