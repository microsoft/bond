// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System.Threading.Tasks;

    /// <summary>
    /// Contains a collection of helper methods used primarily by
    /// Bond-generated code.
    /// </summary>
    public static class CodegenHelpers
    {
        /// <summary>
        /// Upcasts a Task&lt;Derived&gt; to Task&lt;Base&gt;
        /// </summary>
        /// <typeparam name="TDerived">The derived type.</typeparam>
        /// <typeparam name="TBase">The base type.</typeparam>
        /// <param name="task">The task to upcast.</param>
        /// <returns>The upcast task.</returns>
        /// <remarks>
        /// Work around for Task&lt;T&gt;'s lack of co-variance.
        /// </remarks>
        public static Task<TBase> Upcast<TDerived, TBase>(this Task<TDerived> task) where TDerived : TBase
        {
            var tcsOuter = new TaskCompletionSource<TBase>();

            task.ContinueWith((t, state) =>
            {
                // by referencing tcs via the state argument, we make this
                // lambda capture-less
                var tcs = (TaskCompletionSource<TBase>)state;

                if (t.IsFaulted)
                {
                    tcs.SetException(t.Exception.InnerExceptions);
                }
                else if (t.IsCanceled)
                {
                    tcs.SetCanceled();
                }
                else
                {
                    // this "casts" to TBase
                    tcs.SetResult(t.Result);
                }
            }, tcsOuter, TaskContinuationOptions.ExecuteSynchronously);

            return tcsOuter.Task;
        }

        private static Task completedTask = Task.FromResult(default(object));

        /// <summary>
        /// Returns a completed Task
        /// </summary>
        public static Task CompletedTask
        {
            get
            {
                return completedTask;
            }
        }
    }
}
