// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Grpc
{
    using System.Diagnostics;
    using System.Threading.Tasks;

    using global::Grpc.Core;

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
            Method<TRequest, IMessage<Void>> method,
            string host,
            CallOptions options,
            TRequest request)
            where TRequest : class
        {
            AsyncUnaryCall<IMessage<Void>> call = callInvoker.AsyncUnaryCall(method, host, options, request);
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
}
