// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Interfaces
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Threading;
    using System.Threading.Tasks;

    // TODO: logging
    // TODO: performance data

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Interface,
        Inherited = true)]
    public sealed class ServiceAttribute : Attribute
    {
    }

    [AttributeUsage(AttributeTargets.Method, Inherited = true)]
    public sealed class ServiceMethodAttribute : Attribute
    {
    }

    // Make this immutable? But if we do, how does someone take the
    // defaults and the perturb only what they want to change for a
    // per-request basis?
    public class TransportArgs
    {
        public TimeSpan SendTimeout;
        public TimeSpan ReceiveTimeout;

        public virtual T Clone<T>() where T : TransportArgs, new()
        {
            return new T
            {
                SendTimeout = SendTimeout,
                ReceiveTimeout = ReceiveTimeout,
            };
        }
    }

    public class Context
    {
        // For received messages, the connection the message was
        // received on. For messages being sent, will be set to the connection
        // that is being used.
        // Allows getting things like the remote endpoint information.
        // Maybe be null if a connection-less protocol is being used.
        public Connection Connection { get; }
    }

    public class SendContext : Context
    {
        // TransportArgs specific to this request. If null, will use the transport/connection default.
        private TransportArgs TransportArgs { get; }
    }

    public class ReceiveContext : Context
    {
    }

    public class Message<TPayload>
    {
        private IBonded<TPayload> m_payload;
        // Could use bool and have one IBonded field for both. Let's let perf tests show the way.
        private IBonded<Error> m_error;

        // Should there be some way to replace the payload with a live
        // object if, say, a layer need to fully deserializes the
        // payload?
        public IBonded<TPayload> Payload
        {
            get
            {
                if (IsError)
                {
                    throw new InvalidOperationException("Cannot access payload when there is an error.");
                }

                Debug.Assert(m_payload != null);
                return m_payload;
            }
        }

        public IBonded<Error> Error
        {
            get
            {
                return m_error;
            }
        }

        public bool IsError
        {
            get
            {
                return m_error != null;
            }
        }

        public Message(TPayload payload) : this(new Bonded<TPayload>(payload)) { }

        public Message(IBonded<TPayload> payload)
        {
            m_payload = payload;
            m_error = null;
        }

        public Message(Error error) : this(new Bonded<Error>(error)) { }

        public Message(IBonded<Error> error)
        {
            m_payload = null;
            m_error = error;
        }
    }

    public interface IRequestResponseConnection
    {
        Task<Message<TResponse>> RequestResponseAsync<TRequest, TResponse>(Message<TRequest> message, CancellationToken ct);
    }

    public interface IEventConnection
    {
        Task FireEventAsync<TPayload>(Message<TPayload> message, CancellationToken ct);
    }

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

    public abstract class Connection
    {
        // connection-specific TransportArgs. If null, will default to the transport's settings.
        public TransportArgs DefaultTransportArgs { get; set; }

        public abstract Task StopAsync();

        // register/remove per-connections services, like for server-to-client callbacks
        // won't be implemented in first version
        public abstract void AddService<T>(T server);
        public abstract void RemoveService<T>(T service);
    }

    public abstract class Listener
    {
        // Called whenever a new connection is being established.
        public event EventHandler<ConnectedEventArgs> Connected;

        // Called whenever an existing connection has disconnected.
        public event EventHandler<DisconnectedEventArgs> Disconnected;

        public abstract void AddService<T>(T server);
        public abstract void RemoveService<T>(T service);

        public abstract void SetUnhandledExceptionHandler(UnhandledExceptionHandler handler);

        public abstract Task StartAsync();
        public abstract Task StopAsync();
    }

    public interface ILayer<TLayerData> where TLayerData : class
    {
        void OnSend(Message<Bond.Void> message, SendContext context, ref TLayerData layerData);

        void OnReceive(Message<Bond.Void> message, ReceiveContext context, ref TLayerData layerData);
    }

    public interface ILayerStack
    {
        IBonded<Bond.Void> OnSend(Message<Bond.Void> message, SendContext context, object layerData);
        object OnReceive(Message<Bond.Void> message, ReceiveContext context, IBonded<Bond.Void> layerData);
    }

    public class LayerStack<TLayerData> : ILayerStack where TLayerData : class
    {
        private List<ILayer<TLayerData>> m_layers = new List<ILayer<TLayerData>>();
        private UnhandledExceptionHandler m_exceptionHandler;

        public LayerStack()
        {
        }

        // Order matters when adding a Layer. Messages being sent will
        // be passed through layer in the order they were added. When
        // messages are received, the opposite order will be used.
        public LayerStack<TLayerData> AddLayer(ILayer<TLayerData> layer)
        {
            m_layers.Add(layer);
            return this;
        }

        public IBonded<Bond.Void> OnSend(Message<Bond.Void> message, SendContext context, object layerData)
        {
            TLayerData realLayerData;

            if (layerData == null)
            {
                realLayerData = default(TLayerData);
            }
            else
            {
                realLayerData = layerData as TLayerData;
                if (realLayerData == null)
                {
                    throw new ArgumentException("layerData is not of the expected type");
                }
            }

            OnSendImpl(message, context, ref realLayerData);

            // TODO: will want to serialize here to catch any errors
            return ((IBonded<TLayerData>)new Bonded<TLayerData>(realLayerData)).Convert<Bond.Void>();
        }

        public object OnReceive(Message<Bond.Void> message, ReceiveContext context, IBonded<Bond.Void> layerData)
        {
            TLayerData realLayerData;

            if (layerData == null)
            {
                realLayerData = default(TLayerData);
            }
            else
            {
                try
                {
                    realLayerData = layerData.Deserialize<TLayerData>();
                }
                catch (Exception)
                {
                    // call into transport unhandled exception handler
                    // but, for interface exposition purpuses, just rethrow
                    throw;
                }
            }

            OnReceiveImpl(message, context, ref realLayerData);
            return realLayerData;
        }

        private void OnSendImpl(Message<Bond.Void> message, SendContext context, ref TLayerData layerData)
        {
            try
            {
                for (int layerIndex = 0; layerIndex < m_layers.Count; ++layerIndex)
                {
                    m_layers[layerIndex].OnSend(message, context, ref layerData);
                }
            }
            catch (Exception)
            {
                // call into transport unhandled exception handler
                // but, for interface exposition purpuses, just rethrow
                throw;
            }
        }

        private void OnReceiveImpl(Message<Bond.Void> message, ReceiveContext context, ref TLayerData layerData)
        {
             try
            {
                for (int layerIndex = m_layers.Count; layerIndex >= 0; --layerIndex)
                {
                    m_layers[layerIndex].OnReceive(message, context, ref layerData);
                }
            }
            catch (Exception ex)
            {
                // TODO: figure out a more specific set of exceptions to catch
                // TODO: figure out error handshake
                m_exceptionHandler(ex);
            }
        }
    }

    // Optional method to convert an unhandled service-side exception into
    // an error. If not set at the transport/connection level, default
    // behavior is to call Environment.FailFast
    // There will be a default implementation that returns a generic error
    // saying something like "Unhandled Server-side Error: check server logs"
    public delegate Error UnhandledExceptionHandler(Exception ex);

    // There are a number of places where we could use the builder
    // pattern to avoid needing to handle changes later (e.g., if we
    // make service registration static we'd use the builder pattern there
    // as well).
    public abstract class TransportBuilder<TTransport>
    {
        public abstract TransportBuilder<TTransport> SetLayerStack<TLayerData>(LayerStack<TLayerData> layerStack) where TLayerData : class;

        public abstract TransportBuilder<TTransport> SetDefaultTransportArgs(TransportArgs defaults);

        // chain these like we chain layers?
        public abstract TransportBuilder<TTransport> SetUnhandledExceptionHandler(UnhandledExceptionHandler handler);

        // Open question here: how to get an instance of the right
        // reader/writer for these.
        // Probably cut these from the first release and just use Serialize
        // and Deserialize's cached readers/writers.
        public abstract TransportBuilder<TTransport> AddSerializer<TWriter>(Type type, Serializer<TWriter> serializer);
        public abstract TransportBuilder<TTransport> AddSerializers<TWriter>(Dictionary<Type, Serializer<TWriter>> serializers);
        public abstract TransportBuilder<TTransport> AddDeserializer<TReader>(Type type, Deserializer<TReader> deserializer);
        public abstract TransportBuilder<TTransport> AddDeserializers<TReader>(Dictionary<Type, Deserializer<TReader>> deserializers);

        public abstract TTransport Construct();
    }

    public abstract class Transport
    {
        // will have to return a clone of the internal state so that modifications can't be made
        public abstract TransportArgs DefaultMessageArgs { get; }

        public abstract Task<Connection> ConnectToAsync(string address);

        public abstract Task<Connection> ConnectToAsync(string address, CancellationToken ct);

        public abstract Listener MakeListener(string address);

        // close all connections and stop all listeners
        public abstract Task StopAsync();
    }
}
