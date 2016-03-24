// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;

    // Optional method to convert an service-side exception into
    // an error. If not explicitly set on a transport,
    // Transport.DefaultExceptionHandler will be called. The default
    // implementation calls Environment.FailFast, which will kill the
    // process, as Bond doesn't know the state of the rest of the
    // application and assumes that it is irrecoverably corrupted.
    // Transport.ToErrorExceptionHandler can be used to turn an
    // exception into a generic error instead, and
    // Transport.DebugExceptionHandler can be used to turn an
    // exception into an Error with server-side details.
    public delegate Error ExceptionHandler(Exception ex);

    // There are a number of places where we could use the builder
    // pattern to avoid needing to handle changes later (e.g., if we
    // make service registration static we'd use the builder pattern there
    // as well).
    public abstract class TransportBuilder<TTransport>
    {
        public abstract TransportBuilder<TTransport> SetLayerStack<TLayerData>(LayerStack<TLayerData> layerStack) where TLayerData : class;

        public abstract TransportBuilder<TTransport> SetDefaultTransportArgs(TransportArgs defaults);

        // chain these like we chain layers?
        public abstract TransportBuilder<TTransport> SetUnhandledExceptionHandler(ExceptionHandler handler);

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

        public abstract ExceptionHandler UnhandledExceptionHandler { get; }

        public virtual Task<Connection> ConnectToAsync(string address)
        {
            return ConnectToAsync(address, CancellationToken.None);
        }

        public abstract Task<Connection> ConnectToAsync(string address, CancellationToken ct);

        public abstract Listener MakeListener(string address);

        // close all connections and stop all listeners
        public abstract Task StopAsync();

        public static readonly ExceptionHandler DefaultExceptionHandler = FailFastExceptionHandler;

        public static Error FailFastExceptionHandler(Exception ex)
        {
            Environment.FailFast("An unhandled exception was encountered by Bond.Comm", ex);
            return MakeInternalServerError(ex, includeDetails: false);
        }

        public static Error ToErrorExceptionHandler(Exception ex)
        {
            // TODO: log
            return MakeInternalServerError(ex, includeDetails: false);
        }

        public static Error DebugExceptionHandler(Exception ex)
        {
            // TODO: log
            return MakeInternalServerError(ex, includeDetails: true);
        }

        public static InternalServerError MakeInternalServerError(Exception exception, bool includeDetails)
        {
            var internalServerError = new InternalServerError
            {
                error_code = (int) ErrorCode.InternalServerError,
                unique_id = Guid.NewGuid().ToString("D")
            };

            if (includeDetails && exception != null)
            {
                internalServerError.message = "The server has encounted an error: " + exception.Message;
                internalServerError.server_stack_trace = exception.StackTrace;

                var aggEx = exception as AggregateException;
                if (aggEx != null)
                {
                    internalServerError.inner_errors = new List<IBonded<Error>>(aggEx.InnerExceptions.Count);

                    foreach (var innerException in aggEx.InnerExceptions)
                    {
                        var innerError = MakeInternalServerError(innerException, includeDetails);
                        internalServerError.inner_errors.Add(new Bonded<InternalServerError>(innerError));
                    }
                }
                else if (exception.InnerException != null)
                {
                    internalServerError.inner_errors = new List<IBonded<Error>>(1);
                    var innerError = MakeInternalServerError(exception.InnerException, includeDetails);
                    internalServerError.inner_errors.Add(new Bonded<InternalServerError>(innerError));
                }
            }
            else
            {
                internalServerError.message = "The server has encounted an error.";
            }

            return internalServerError;
        }
    }
}
