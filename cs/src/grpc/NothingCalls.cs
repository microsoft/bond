// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Grpc.Internal
{
    using System.Diagnostics;
    using System.Threading.Tasks;

    using global::Grpc.Core;
    using System;

    /// <summary>
    /// Helper methods for client-side invocation of Bond's nothing methods.
    /// </summary>
    /// <remarks>
    /// These methods are used by the generated code and are not
    /// considered part of Bond's public API surface. They may change at
    /// any time for any reason.
    /// </remarks>
    public static class NothingCallInvoker
    {
        /// <summary>
        /// Invokes a Bond nothing method.
        /// </summary>
        /// <remarks>
        /// The result--whether successful or unsuccessful--of a nothing method cannot be observed.
        /// This method invokes the call and sets up the gRPC response to be ignored.
        /// </remarks>
        /// <typeparam name="TRequest">The type of the request</typeparam>
        /// <param name="callInvoker">A client call invoker</param>
        /// <param name="method">The method to invoke</param>
        /// <param name="host">The host</param>
        /// <param name="options">The call options</param>
        /// <param name="request">The request object</param>
        public static void NothingCall<TRequest>(
            CallInvoker callInvoker,
            Method<TRequest, IMessage<Bond.Void>> method,
            string host,
            CallOptions options,
            TRequest request)
            where TRequest : class
        {
            AsyncUnaryCall<IMessage<Bond.Void>> call = callInvoker.AsyncUnaryCall(method, host, options, request);
            call.ResponseAsync.ContinueWith(ObserveAndIgnoreTaskException, TaskContinuationOptions.OnlyOnFaulted);
        }

        /// <summary>
        /// Observes and ignores exceptions from a given faulted task.
        /// </summary>
        /// <remarks>
        /// <para>
        /// <paramref name="t"/> must be a faulted task.
        /// </para>
        /// <para>
        /// This method is used to avoid polluting
        /// <see cref="TaskScheduler.UnobservedTaskException">the UnobservedTaskException event
        /// handler</see> with exceptions from a task that may throw, but should have all exceptions
        /// ignored.
        /// </para>
        /// </remarks>
        /// <param name="t">The Task from which to ignore exceptions.</param>
        private static void ObserveAndIgnoreTaskException(Task t)
        {
            Debug.Assert(t.IsFaulted);
            // Access the Exception property to mark it as observed.
            var ignored = t.Exception;
        }
    }

    /// <summary>
    /// Helper methods for server-side handling of Bond's nothing methods.
    /// </summary>
    /// <remarks>
    /// These methods are used by the generated code and are not
    /// considered part of Bond's public API surface. They may change at
    /// any time for any reason.
    /// </remarks>
    public static class NothingCallHandler
    {
        /// <summary>
        /// Helper used to invoke a nothing method handler and deal with
        /// potential exceptions.
        /// </summary>
        /// <remarks>
        /// Nothing methods cannot return or throw anything. This helper
        /// prevents exceptions from a nothing method handler from leaking
        /// into the response.
        /// </remarks>
        /// <typeparam name="T">The type of the message payload</typeparam>
        /// <param name="handler">The actual handler to invoke</param>
        /// <param name="request">The request</param>
        /// <param name="context">The context</param>
        /// <returns><see cref="Message.Void"/></returns>
        public static async Task<IMessage<Bond.Void>> Handle<T>(
            Func<IMessage<T>, ServerCallContext, Task> handler,
            IMessage<T> request,
            ServerCallContext context)
        {
            try
            {
                await handler(request, context).ConfigureAwait(false);
            }
            catch (Exception)
            {
                // Ignore the exception. Exceptions from nothing methods cannot be observed by clients.
            }

            // Reset status in case handler changed it. Nothing methods are always successful.
            if (context != null)
            {
                context.Status = Status.DefaultSuccess;
            }

            return Message.Void;
        }

        /// <summary>
        /// Helper used to invoke a nothing method handler and deal with
        /// potential exceptions.
        /// </summary>
        /// <remarks>
        /// Nothing methods cannot return or throw anything. This helper
        /// prevents exceptions from a nothing method handler from leaking
        /// into the response.
        /// </remarks>
        /// <param name="handler">The actual handler to invoke</param>
        /// <param name="context">The context</param>
        /// <returns><see cref="Message.Void"/></returns>
        public static async Task<IMessage<Bond.Void>> Handle(
            Func<ServerCallContext, Task> handler,
            ServerCallContext context)
        {
            try
            {
                await handler(context).ConfigureAwait(false);
            }
            catch (Exception)
            {
                // Ignore the exception. Exceptions from nothing methods cannot be observed by clients.
            }

            // Reset status in case handler changed it. Nothing methods are always successful.
            if (context != null)
            {
                context.Status = Status.DefaultSuccess;
            }

            return Message.Void;
        }
    }
}
