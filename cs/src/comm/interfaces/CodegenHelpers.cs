// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System.Threading.Tasks;

    public static class CodegenHelpers
    {
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
    }
}
