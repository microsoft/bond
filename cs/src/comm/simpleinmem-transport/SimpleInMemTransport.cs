// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    
    public class SimpleInMemTransportBuilder : TransportBuilder<SimpleInMemTransport>
    {
        private ExceptionHandler m_exceptionHandler = Transport.DefaultExceptionHandler;

        public override TransportBuilder<SimpleInMemTransport> AddDeserializer<TReader>(Type type, Deserializer<TReader> deserializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<SimpleInMemTransport> AddDeserializers<TReader>(System.Collections.Generic.Dictionary<Type, Deserializer<TReader>> deserializers)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<SimpleInMemTransport> AddSerializer<TWriter>(Type type, Serializer<TWriter> serializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<SimpleInMemTransport> AddSerializers<TWriter>(System.Collections.Generic.Dictionary<Type, Serializer<TWriter>> serializers)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<SimpleInMemTransport> SetDefaultTransportArgs(TransportArgs defaults)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<SimpleInMemTransport> SetLayerStack<TLayerData>(LayerStack<TLayerData> layerStack)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<SimpleInMemTransport> SetUnhandledExceptionHandler(ExceptionHandler handler)
        {
            m_exceptionHandler = handler;
            return this;
        }

        public override SimpleInMemTransport Construct()
        {
            return new SimpleInMemTransport(m_exceptionHandler);
        }
    }

    public class SimpleInMemTransport : Transport
    {
        private SimpleInMemTransportArgs m_defaultTransportArgs = new SimpleInMemTransportArgs();
        private IDictionary<string, SimpleInMemListener> m_listeners = new Dictionary<string, SimpleInMemListener>();
        private object m_lock = new object();

        private readonly ExceptionHandler m_exceptionHandler;

        public SimpleInMemTransport(ExceptionHandler exceptionHandler)
        {
            m_exceptionHandler = exceptionHandler;
        }

        public override TransportArgs DefaultTransportArgs
        {
            get
            {
                return m_defaultTransportArgs.Clone<SimpleInMemTransportArgs>();
            }
        }

        public override ExceptionHandler UnhandledExceptionHandler
        {
            get
            {
                return m_exceptionHandler;
            }
        }

        public async override Task<Connection> ConnectToAsync(string address, CancellationToken ct)
        {
            SimpleInMemListener listener;
            if (!m_listeners.TryGetValue(address, out listener))
            {
                throw new InMemTransportListenerException(string.Format("Listener not found for address: {0}", address));
            }

            //TODO Invoke unhandled exception handler if this throws exception
            return await Task.Run<Connection>(() =>
            {
                var connection = new SimpleInMemConnection(this, ConnectionType.Client);
                listener.Connection.AddRequestResponseQueue(connection.Id, connection.RequestResponseQueue);
                connection.Start();
                return connection;
            });
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
            throw new NotImplementedException();
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
