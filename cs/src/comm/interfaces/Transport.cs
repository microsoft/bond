// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;

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
        public abstract TransportArgs DefaultTransportArgs { get; }

        public virtual Task<Connection> ConnectToAsync(string address)
        {
            return ConnectToAsync(address, CancellationToken.None);
        }

        public abstract Task<Connection> ConnectToAsync(string address, CancellationToken ct);

        public abstract Listener MakeListener(string address);

        // close all connections and stop all listeners
        public abstract Task StopAsync();
    }
}
