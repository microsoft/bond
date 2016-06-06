// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System.Collections.Generic;
    using System.Threading.Tasks;

    internal class InMemFrame
    {
        internal SimpleInMemHeaders headers;
        internal IBonded layerData;
        internal IMessage message;
        internal TaskCompletionSource<IMessage> outstandingRequest;
    }

    internal class InMemFrameQueue
    {
        private Queue<InMemFrame> queue;
        private object qLock = new object();

        internal InMemFrameQueue()
        {
            queue = new Queue<InMemFrame>();
        }

        internal void Clear()
        {
            lock (qLock)
            {
                foreach (InMemFrame frame in queue)
                {
                    frame.outstandingRequest.SetCanceled();
                }
                queue.Clear();
            }
        }

        internal void Enqueue(InMemFrame frame)
        {
            Util.Validate(frame);

            lock (qLock)
            {
                queue.Enqueue(frame);
            }
        }

        internal InMemFrame Dequeue()
        {
            InMemFrame frame = null;

            lock (qLock)
            {
                if (queue.Count > 0)
                {
                    frame = queue.Dequeue();
                }
            }

            return frame;
        }

        internal int Count
        {
            get{ return queue.Count; }
        }
    }
}
