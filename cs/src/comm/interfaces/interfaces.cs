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

    // this feels like it is getting to big
    // perhaps split into sending context and receiving context
    public class Message<TPayload, TLayerData>
    {
        private IBonded<TPayload> m_payload;
        // Could use bool and have one IBonded field for both. Let's let perf tests show the way.
        private IBonded<Error> m_error;
        private IBonded<TLayerData> m_layer;
        private TransportArgs m_args;

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

        public IBonded<TLayerData> LayerData
        {
            get
            {
                return m_layer;
            }
        }

        public TransportArgs Args
        {
            get
            {
                return m_args;
            }
        }

        // For received messages, the connection the message was
        // received on. Allows getting things like the remote endpoint
        // information.
        // For messages being sent, must be null.
        public readonly Connection Connection;

        public Message(IBonded<TPayload> payload) : this(payload, default(IBonded<TLayerData>), null) { }

        public Message(TPayload payload) : this(new Bonded<TPayload>(payload)) { }

        public Message(IBonded<TPayload> payload, IBonded<TLayerData> layer, TransportArgs args)
        {
            m_payload = payload;
            m_error = null;
            m_layer = layer;
            m_args = args;
        }

        public Message(IBonded<Error> error) : this(error, default(IBonded<TLayerData>), null) { }

        public Message(Error error) : this(new Bonded<Error>(error)) { }

        public Message(IBonded<Error> error, IBonded<TLayerData> layer, TransportArgs args)
        {
            m_payload = null;
            m_error = error;
            m_layer = layer;
            m_args = args;
        }
    }

    public class Message<TPayload> : Message<TPayload, Bond.Void>
    {
        public Message(IBonded<TPayload> payload) : base(payload) { }

        public Message(TPayload payload) : base(payload) { }

        public Message(IBonded<Error> error) : base(error) { }

        public Message(Error error) : base(error) { }
    }

    public interface IRequestResponseConnection
    {
        Task<Message<TResponse, TLayerData>> RequestResponseAsync<TRequest, TResponse, TLayerData>(Message<TRequest, TLayerData> message, CancellationToken ct);
    }

    public interface IEventConnection
    {
        Task FireEventAsync<TPayload, TLayerData>(Message<TPayload, TLayerData> message, CancellationToken ct);
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
        public abstract Task StopAsync();

        // register/remove per-connections services, like for callbacks
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

    public interface ILayer<TPayload, TLayerData>
    {
        void OnSend(Message<TPayload, TLayerData> message);

        void OnReceive(Message<TPayload, TLayerData> message);
    }

    public interface ILayer<TLayerData> : ILayer<Bond.Void, TLayerData>
    {
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
    public abstract class TransportBuilder<TTransport, TLayerData>
    {
        // Order matters when adding a Layer. Messages being sent will
        // be passed through layer in the order they were added. When
        // messages are received, the opposite order will be used.
        public abstract TransportBuilder<TTransport, TLayerData> AddLayer<TPayload>(ILayer<TPayload, TLayerData> layer);

        public abstract TransportBuilder<TTransport, TLayerData> SetDefaultMessageArgs(TransportArgs defaults);

        // chain these like we chain layers?
        public abstract TransportBuilder<TTransport, TLayerData> SetUnhandledExceptionHandler(UnhandledExceptionHandler handler);

        // Open question here: how to get an instance of the right
        // reader/writer for these.
        // Probably cut these from the first release and just use Serialize
        // and Deserialize's cached readers/writers.
        public abstract TransportBuilder<TTransport, TLayerData> AddSerializer<TWriter>(Type type, Serializer<TWriter> serializer);
        public abstract TransportBuilder<TTransport, TLayerData> AddSerializers<TWriter>(Dictionary<Type, Serializer<TWriter>> serializers);
        public abstract TransportBuilder<TTransport, TLayerData> AddDeserializer<TReader>(Type type, Deserializer<TReader> deserializer);
        public abstract TransportBuilder<TTransport, TLayerData> AddDeserializers<TReader>(Dictionary<Type, Deserializer<TReader>> deserializers);

        public abstract TTransport Construct();
    }

    public abstract class Transport<TLayerData>
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
