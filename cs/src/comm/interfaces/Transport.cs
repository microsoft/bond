// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;

    /// <summary>
    /// Represents a method that can be used to handle exceptions and convert
    /// them into <see cref="Error">Errors</see>.
    /// </summary>
    /// <param name="ex">The exception that needs to be handled.</param>
    /// <returns>
    /// A representation of the exception as an <see cref="Error"/>. If
    /// <c>null</c> is returned, this indicates that the exception should be
    /// ignored.
    /// </returns>
    public delegate Error ExceptionHandler(Exception ex);

    /// <summary>
    /// Provides a convenient way to initialize and create
    /// <see cref="Transport">Transports</see>.
    /// </summary>
    /// <typeparam name="TTransport">
    /// The type of transport this builder can build.
    /// </typeparam>
    public abstract class TransportBuilder<TTransport>
    {
        ExceptionHandler exceptionHandler;

        protected ExceptionHandler GetUnhandledExceptionHandler()
        {
            return exceptionHandler;
        }

        protected ILayerStack LayerStack { get; private set; }

        /// <summary>
        /// Set the unhandled exception handler.
        /// </summary>
        /// <param name="exceptionHandler">The exception handler.</param>
        /// <returns>The builder.</returns>
        /// <remarks>
        /// The unhandled exception handler is invoked when an exception
        /// escapes user code back into the Bond Comm framework. The exception
        /// handler can be used to convert exceptions into Bond errors, log
        /// exceptions, and so on.
        /// </remarks>
        public virtual TransportBuilder<TTransport> SetUnhandledExceptionHandler(ExceptionHandler exceptionHandler)
        {
            if (exceptionHandler == null)
            {
                throw new ArgumentNullException(nameof(exceptionHandler));
            }

            this.exceptionHandler = exceptionHandler;
            return this;
        }

        /// <summary>
        /// Set the layer stack.
        /// </summary>
        /// <param name="layerStack">The layer stack.</param>
        /// <returns>The builder.</returns>
        /// <remarks>
        /// The layer stack supplies the set of layers to be used
        /// by the built transport. May be null.
        /// </remarks>
        public virtual TransportBuilder<TTransport> SetLayerStack(ILayerStack layerStack)
        {
            LayerStack = layerStack;
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
    public abstract class Transport
    {
        /// <summary>
        /// The unhandled exception handler.
        /// </summary>
        public abstract ExceptionHandler UnhandledExceptionHandler { get; }

        /// <summary>
        /// The layer stack for this transport. May be null.
        /// </summary>
        public abstract ILayerStack LayerStack { get; }

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
        public virtual Task<Connection> ConnectToAsync(string address)
        {
            return ConnectToAsync(address, CancellationToken.None);
        }

        /// <summary>
        /// Starts an asynchronous operation to connect to the specified
        /// address.
        /// </summary>
        /// <param name="address">The address to connect to.</param>
        /// <param name="ct">The cancellation token.</param>
        /// <returns>A task of the <see cref="Connection"/>.</returns>
        /// <remarks>
        /// Each transport uses its own format for addresses. Consult the
        /// concrete implementation that is being used for details.
        /// </remarks>
        public abstract Task<Connection> ConnectToAsync(string address, CancellationToken ct);

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
        public abstract Listener MakeListener(string address);

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

        /// <summary>
        /// An exception handler that calls
        /// <see cref="Environment.FailFast(string, Exception)"/> to terminate
        /// the process.
        /// </summary>
        /// <param name="ex">The exception.</param>
        /// <returns>Does not return.</returns>
        public static Error FailFastExceptionHandler(Exception ex)
        {
            // We intentionally don't use Log.Fatal here, as that might cause
            // the logger to kill the process, and this exception handler
            // explicitly uses Environment.FailFast.
            Log.Error(ex, "An unhandled exception was encountered by Bond.Comm. Calling Environment.FailFast");
            Environment.FailFast("An unhandled exception was encountered by Bond.Comm", ex);
            return MakeInternalServerError(ex, includeDetails: false);
        }

        /// <summary>
        /// An exception handler that converts exceptions into generic
        /// <see cref="Error">Errors</see>.
        /// </summary>
        /// <param name="ex">The exception.</param>
        /// <returns>
        /// An Error with a generic message and a GUID for later correlation
        /// with server logs.
        /// </returns>
        /// <remarks>
        /// This handler does not include any exception information in the
        /// Error, so clients will not be exposed to potentially sensitive
        /// server-side implementation details.
        /// </remarks>
        public static Error ToErrorExceptionHandler(Exception ex)
        {
            Log.Error(ex, "An unhandled exception was encountered by Bond.Comm. It has been converted to an Error.");
            return MakeInternalServerError(ex, includeDetails: false);
        }

        /// <summary>
        /// An exception handler that converts exceptions into detailed
        /// <see cref="Error">Errors</see>, including server-side stack traces
        /// and inner exceptions,
        /// </summary>
        /// <param name="ex">The exception.</param>
        /// <returns>An Error with many excetion details.</returns>
        /// <remarks>
        /// This handler creates Errors that include potentially sensitive
        /// server-side implementation details. It should only be used for
        /// debugging purposes.
        /// </remarks>
        public static Error DebugExceptionHandler(Exception ex)
        {
            Log.Error(ex, "An unhandled exception was encountered by Bond.Comm. It has been converted to an Error with debug details.");
            return MakeInternalServerError(ex, includeDetails: true);
        }

        /// <summary>
        /// Creates an <see cref="Error"/> for an internal server error from an
        /// exception.
        /// </summary>
        /// <param name="exception">The exception.</param>
        /// <param name="includeDetails">
        /// <c>true</c> if debugging details should be included; <c>false</c>
        /// to omit this potentailly sensitive information
        /// </param>
        /// <returns>An Error representing the exception.</returns>
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
