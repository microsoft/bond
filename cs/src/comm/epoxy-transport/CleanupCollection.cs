// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Threading.Tasks;

    /// <summary>
    /// Maintains a collection of things that will need to be cleaned up at
    /// sometime in the future.
    /// </summary>
    /// <typeparam name="T">The type of the items needing to be cleaned up.</typeparam>
    /// <remarks>
    /// The items of type <typeparamref name="T"/> need to be comparable by
    /// <see cref="EqualityComparer{T}.Default">EqualityComparer&lt;T&gt;.Default</see>.
    /// </remarks>
    internal class CleanupCollection<T>
    {
        readonly object lockObj = new object();
        bool isShutdown = false;
        HashSet<T> items = new HashSet<T>();

        /// <summary>
        /// Adds an item to the collection.
        /// </summary>
        /// <param name="item">The item to be added.</param>
        /// <remarks>
        /// It's safe to add the same item more than once. It will only be
        /// cleaned up once.
        /// </remarks>
        /// <exception cref="InvalidOperationException">
        /// Thrown if the collection has already been cleaned up.
        /// </exception>
        public void Add(T item)
        {
            lock (lockObj)
            {
                if (isShutdown)
                {
                    throw new InvalidOperationException("The cleanup collection has been cleaned up.");
                }

                items.Add(item);
            }
        }

        /// <summary>
        /// Removes an item from the collection.
        /// </summary>
        /// <param name="item">The item to remove.</param>
        /// <remarks>
        /// This may be called after the collection has been cleaned up;
        /// however, doing so is a no-op.
        /// </remarks>
        public void Remove(T item)
        {
            lock (lockObj)
            {
                if (isShutdown)
                {
                    return;
                }

                items.Remove(item);
            }
        }

        /// <summary>
        /// Starts cleaning up this collection. Once clean up has been started,
        /// no new items are accepted. The optional
        /// <paramref name="cleanupFunc"/> function will be run on all
        /// accumulated items.
        /// </summary>
        /// <param name="cleanupFunc">
        /// An optional function to run to clean up each item.
        /// </param>
        /// <returns>
        /// A Task indicating that all of the invocations of
        /// <paramref name="cleanupFunc"/> have completed.
        /// </returns>
        /// <remarks>The order of item clean up is unspecified.</remarks>
        public Task CleanupAsync(Func<T, Task> cleanupFunc = null)
        {
            HashSet<T> localItems;

            lock (lockObj)
            {
                if (isShutdown)
                {
                    return CodegenHelpers.CompletedTask;
                }

                isShutdown = true;
                localItems = items;
                items = null;
            }

            Debug.Assert(localItems != null);

            if (localItems.Count == 0 || cleanupFunc == null)
            {
                return CodegenHelpers.CompletedTask;
            }

            var shutdownTasks = new Task[localItems.Count];
            int idx = 0;
            foreach (var item in localItems)
            {
                shutdownTasks[idx] = cleanupFunc(item);
                ++idx;
            }

            return Task.WhenAll(shutdownTasks);
        }
    }
}
