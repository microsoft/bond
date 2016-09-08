// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    using System.Threading.Tasks.Dataflow;

    internal static class Util
    {
        internal static InMemFrame NewPayLoad(
            ulong conversationId,
            SimpleInMemMessageType messageType,
            IBonded layerData,
            IMessage message,
            TaskCompletionSource<IMessage> taskSource)
        {
            var headers = new SimpleInMemHeaders
            {
                conversation_id = conversationId,
                message_type = messageType
            };

            var payload = new InMemFrame
            {
                headers = headers,
                layerData = layerData,
                message = message,
                outstandingRequest = taskSource
            };

            Validate(payload);
            return payload;
        }
        
        internal static InMemFrame Validate(InMemFrame frame)
        {
            if (frame == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame)}");
            }
            else if (frame.headers == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame.headers)} in frame");
            }
            else if (frame.message == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame.message)} in frame");
            }

            return frame;
        }

        internal static ITargetBlock<object> CreateLongRunningTask(Action<object> process, CancellationToken t, int minDelay, int maxDelay)
        {
            ActionBlock<object> actionBlock = null;
            Random randomDelay = new Random(DateTime.UtcNow.Millisecond);

            actionBlock = new ActionBlock<object>(async o => {
                process(o);
                // Delay between minDelay and maxDelay - 1 milliseconds before posting itself. 
                // Need it for task context switching for long running, CPU intensive tasks.
                await Task.Delay(TimeSpan.FromMilliseconds(randomDelay.Next(minDelay, maxDelay)), t);
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
