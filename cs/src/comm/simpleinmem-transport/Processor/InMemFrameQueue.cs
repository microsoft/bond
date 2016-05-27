// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System;
    using System.Collections.Concurrent;
    using System.Collections.Generic;
    using System.Threading.Tasks;

    internal class InMemFrameQueue
    {
        private Queue<InMemFrame> requests;
        private Queue<InMemFrame> responses;
        private Queue<InMemFrame> events;
        private object requestsLock = new object();
        private object responsesLock = new object();
        private object eventsLock = new object();

        internal InMemFrameQueue()
        {
            requests = new Queue<InMemFrame>();
            responses = new Queue<InMemFrame>();
            events = new Queue<InMemFrame>();
        }

        internal void Clear()
        {
            lock (requestsLock)
            {
                Clear(requests);
            }

            lock (responsesLock)
            {
                Clear(responses);
            }

            lock (eventsLock)
            {
                Clear(events);
            }
        }

        internal void Enqueue(InMemFrame frame)
        {
            Util.Validate(frame);

            switch(frame.headers.payload_type)
            {
                case PayloadType.Request:
                    lock (requestsLock)
                    {
                        requests.Enqueue(frame);
                    }
                    break;

                case PayloadType.Response:
                    lock (responsesLock)
                    {
                        responses.Enqueue(frame);
                    }
                    break;

                case PayloadType.Event:
                    lock (events)
                    {
                        events.Enqueue(frame);
                    }
                    break;

                default:
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Payload type {2} not supported!",
                        nameof(InMemFrameQueue), nameof(Enqueue), frame.headers.payload_type);
                    throw new NotImplementedException(message);
            }

        }

        internal InMemFrame Dequeue(PayloadType payloadType)
        {
            InMemFrame frame;
            switch (payloadType)
            {
                case PayloadType.Request:
                    lock (requestsLock)
                    {
                        frame = requests.Dequeue();
                    }
                    break;

                case PayloadType.Response:
                    lock (responsesLock)
                    {
                        frame = responses.Dequeue();
                    }
                    break;

                case PayloadType.Event:
                    lock (eventsLock)
                    {
                        frame = events.Dequeue();
                    }
                    break;

                default:
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Payload type {2} not supported!",
                        nameof(InMemFrameQueue), nameof(Dequeue), payloadType);
                    throw new NotImplementedException(message);
            }

            return frame;
        }

        internal int Count(PayloadType payloadType)
        {
            int count;
            switch(payloadType)
            {
                case PayloadType.Request:
                    count = requests.Count;
                    break;

                case PayloadType.Response:
                    count = responses.Count;
                    break;

                case PayloadType.Event:
                    count = events.Count;
                    break;

                default:
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Payload type {2} not supported!",
                        nameof(InMemFrameQueue), nameof(Count), payloadType);
                    throw new NotImplementedException(message);
            }

            return count;
        }

        private void Clear(Queue<InMemFrame> queue)
        {
            foreach (InMemFrame frame in queue)
            {
                frame.outstandingRequest.SetCanceled();
            }
            queue.Clear();
        }
    }

    internal class InMemFrameQueueCollection
    {
        private ConcurrentDictionary<Guid, InMemFrameQueue> reqresqueue;

        internal InMemFrameQueueCollection()
        {
            reqresqueue = new ConcurrentDictionary<Guid, InMemFrameQueue>();
        }

        internal void Add(Guid id, InMemFrameQueue queue)
        {
            if (!reqresqueue.TryAdd(id, queue))
            {
                var message = LogUtil.FatalAndReturnFormatted(
                    "{0}.{1}: Guid collison must never happen for client connection Ids: {2}",
                    nameof(InMemFrameQueueCollection), nameof(Add), id);
                throw new InvalidOperationException(message);
            }
        }

        internal ICollection<Guid> GetKeys()
        {
            return reqresqueue.Keys;
        }

        internal InMemFrameQueue GetQueue(Guid id)
        {
            InMemFrameQueue queue;
            reqresqueue.TryGetValue(id, out queue);
            return queue;
        }

        internal void ClearAll()
        {
            ICollection<Guid> keys = GetKeys();

            foreach (Guid key in keys)
            {
                GetQueue(key).Clear();
            }
            reqresqueue.Clear();
        }
    }

    internal class InMemFrame
    {
        internal SimpleInMemHeaders headers;
        internal IBonded layerData;
        internal IMessage message;
        internal TaskCompletionSource<IMessage> outstandingRequest;
    }
}
