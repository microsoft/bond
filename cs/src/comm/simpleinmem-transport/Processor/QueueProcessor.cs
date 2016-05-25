// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    using System.Threading.Tasks.Dataflow;

    internal abstract class QueueProcessor
    {
        protected const int PROCESSING_BATCH_SIZE = 1000;

        private readonly Random m_randomDelay = new Random(DateTime.UtcNow.Millisecond);
        private readonly object m_processInput = new object();
        
        public void ProcessAsync(CancellationToken t)
        {
            NewLongRunningTask(o => Process(), t).Post(m_processInput);
        }

        internal abstract void Process();

        private ITargetBlock<object> NewLongRunningTask(Action<object> process, CancellationToken t)
        {
            ActionBlock<object> actionBlock = null;
            
            actionBlock = new ActionBlock<object>(async o => {
                process(o);
                // Delay between 1 to 10 milliseconds before posting itself. 
                // Need it for task context switching for long running, CPU intensive tasks.
                await Task.Delay(TimeSpan.FromMilliseconds(m_randomDelay.Next(1, 11)), t);
                actionBlock.Post(o);
            }, 
            new ExecutionDataflowBlockOptions
            {
                CancellationToken = t
            });

            return actionBlock;
        }
    }
}