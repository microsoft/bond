// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Threading;
    using System.Threading.Tasks;

    /// <summary>
    /// Maintains a collection of conversations expecting responses and
    /// provides once-and-only-once completion.
    /// </summary>
    internal class ResponseMap
    {
        /// <summary>
        /// The <see cref="ErrorCode"/> that is used in the <see cref="Error"/>
        /// response when the ResponseMap has been shutdown.
        /// </summary>
        public static readonly ErrorCode ShutDownErrorCode = ErrorCode.CONNECTION_SHUT_DOWN;
        const string ShutdownMessage = "Connection has already been shutdown";

        static readonly Lazy<IMessage>  alreadyShutdownResponse = new Lazy<IMessage>(InitAlreadyShutdownResponse, LazyThreadSafetyMode.PublicationOnly);
        static readonly Lazy<Task<IMessage>> alreadyShutdownTask = new Lazy<Task<IMessage>>(InitAlreadyShutdownTask, LazyThreadSafetyMode.PublicationOnly);

        // We use this as the lock object for these members. Since we control
        // both the clients of this class and the class itself, we can ensure
        // that the clients never lock on this. By doing so, we reduce the size
        // of this object by a reference.
        readonly Dictionary<ulong, TaskCompletionSource<IMessage>> outstandingConvos = new Dictionary<ulong, TaskCompletionSource<IMessage>>();
        bool isShutdown = false;

        /// <summary>
        /// Gets the number of currently outstanding conversations.
        /// </summary>
        public int OutstandingCount => outstandingConvos.Count;

        /// <summary>
        /// Adds the given conversation to the collection of outstanding
        /// conversations.
        /// </summary>
        /// <remarks>
        /// The caller must ensure Add is not called with duplicate
        /// <paramref name="conversationId">conversationId</paramref>.
        /// </remarks>
        /// <param name="conversationId">The conversation to register.</param>
        /// <param name="state">State to store in task. Defaults to null.</param>
        /// <returns>
        /// A <see cref="Task{T}">Task&lt;IMessage&gt;</see> that will be
        /// completed when a corresponding call to <see cref="Complete"/> is
        /// made.
        /// </returns>
        /// <exception cref="ArgumentException">
        /// When the same <paramref name="conversationId"/> had already been
        /// registered.
        /// </exception>
        public Task<IMessage> Add(ulong conversationId, object state = null)
        {
            // Speculative creation to reduce duration of lock. In the event
            // that the connection is already shutdown, this will have been a
            // waste, but shutdown is not the common case.
            var tcs = new TaskCompletionSource<IMessage>(state);

            lock (this)
            {
                if (isShutdown)
                {
                    return alreadyShutdownTask.Value;
                }

                outstandingConvos.Add(conversationId, tcs);
            }

            return tcs.Task;
        }

        /// <summary>
        /// Attempts to complete the given conversation with the given
        /// response.
        /// </summary>
        /// <param name="conversationId">The conversation to complete.</param>
        /// <param name="response">The response for that conversation.</param>
        /// <returns>
        /// <c>true</c> if this call to Complete resulted in completion of the
        /// conversation; <c>false</c> otherwise (e.g., the conversation was
        /// previously completed, shutdown has been requested).
        /// </returns>
        /// <remarks>
        /// Since completing a conversation may cause application code to run,
        /// completion is done on the thread pool to avoid having long-running
        /// application code block further progress of the caller of Complete.
        /// </remarks>
        public bool Complete(ulong conversationId, IMessage response)
        {
            TaskCompletionSource<IMessage> tcs = TakeTaskCompletionSource(conversationId);
            if (tcs == null)
            {
                return false;
            }

            CompleteOnThreadPool(tcs, response);
            return true;
        }

        public TaskCompletionSource<IMessage> TakeTaskCompletionSource(ulong conversationId)
        {
            TaskCompletionSource<IMessage> tcs;
            lock (this)
            {
                if (!outstandingConvos.TryGetValue(conversationId, out tcs))
                {
                    return null;
                }

                outstandingConvos.Remove(conversationId);
            }

            return tcs;
        }

        /// <summary>
        /// Stops accepting new conversations and completes any outstanding
        /// conversations with an <see cref="Error"/>.
        /// </summary>
        /// <remarks>
        /// <para>
        /// Any subsequent calls to <see cref="Add"/> will be completed with an
        /// Error response.
        /// </para>
        /// <para>
        /// The <see cref="Error.error_code"/> in the Error response is set to
        /// <see cref="ShutDownErrorCode"/>.
        /// </para>
        /// </remarks>
        public void Shutdown()
        {
            List<ulong> convosToComplete;

            lock (this)
            {
                if (isShutdown)
                {
                    return;
                }

                isShutdown = true;

                // We may still be racing with other calls to Complete, so we
                // grab a snapshot of the keys that need to be completed and
                // then drop the lock. Then, we'll invoke Complete on each one
                // to make sure we only complete each once, regardless who
                // calls Complete first.
                //
                // Since we set isShutdown to true, nothing new will be added
                // to the collection after we take the snapshot, so we can be
                // safe capturing the outstanding conversations once.
                convosToComplete = outstandingConvos.Keys.ToList();
            }

            foreach (ulong conversationId in convosToComplete)
            {
                Complete(conversationId, alreadyShutdownResponse.Value);
            }
        }

        void CompleteOnThreadPool(TaskCompletionSource<IMessage> tcs, IMessage response)
        {
            // Completing this Task might result in user code running, so we
            // explicitly use Task.Run to force this onto the thread pool so
            // that we can resume reading from the socket.
            //
            // When we target .NET 4.6 or later, this can be replaced with
            // creating the TaskCompletionSource with
            // TaskCreationOptions.RunContinuationsAsynchronously
            Task.Run(() =>
            {
                tcs.SetResult(response);
            });
        }

        static IMessage InitAlreadyShutdownResponse()
        {
            var error = new Error
            {
                error_code = (int)ShutDownErrorCode,
                message = ShutdownMessage
            };

            return Message.FromError(error);
        }

        static Task<IMessage> InitAlreadyShutdownTask()
        {
            return Task.FromResult(alreadyShutdownResponse.Value);
        }
    }
}
