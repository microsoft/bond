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
        private Queue<InMemFrame> m_requests;
        private Queue<InMemFrame> m_responses;
        private Queue<InMemFrame> m_events;
        private object m_lockreq = new object();
        private object m_lockres = new object();
        private object m_lockevent = new object();

        internal InMemFrameQueue()
        {
            m_requests = new Queue<InMemFrame>();
            m_responses = new Queue<InMemFrame>();
            m_events = new Queue<InMemFrame>();
        }

        internal void Clear()
        {
            lock(m_lockreq)
            {
                Clear(m_requests);
            }

            lock (m_lockres)
            {
                Clear(m_responses);
            }

            lock (m_lockevent)
            {
                Clear(m_events);
            }
        }

        internal void Enqueue(InMemFrame frame)
        {
            Util.Validate(frame);

            switch(frame.m_headers.payload_type)
            {
                case PayloadType.Request:
                    lock(m_lockreq)
                    {
                        m_requests.Enqueue(frame);
                    }
                    break;

                case PayloadType.Response:
                    lock (m_lockres)
                    {
                        m_responses.Enqueue(frame);
                    }
                    break;

                case PayloadType.Event:
                    lock (m_events)
                    {
                        m_events.Enqueue(frame);
                    }
                    break;

                default:
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Payload type {2} not supported!",
                        nameof(InMemFrameQueue), nameof(Enqueue), frame.m_headers.payload_type);
                    throw new NotImplementedException(message);
            }

        }

        internal InMemFrame Dequeue(PayloadType payloadType)
        {
            InMemFrame frame;
            switch (payloadType)
            {
                case PayloadType.Request:
                    lock (m_lockreq)
                    {
                        frame = m_requests.Dequeue();
                    }
                    break;

                case PayloadType.Response:
                    lock (m_lockres)
                    {
                        frame = m_responses.Dequeue();
                    }
                    break;

                case PayloadType.Event:
                    lock (m_lockevent)
                    {
                        frame = m_events.Dequeue();
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
                    count = m_requests.Count;
                    break;

                case PayloadType.Response:
                    count = m_responses.Count;
                    break;

                case PayloadType.Event:
                    count = m_events.Count;
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
                frame.m_outstandingRequest.SetCanceled();
            }
            queue.Clear();
        }
    }

    internal class InMemFrameQueueCollection
    {
        private ConcurrentDictionary<Guid, InMemFrameQueue> m_reqresqueue;

        internal InMemFrameQueueCollection()
        {
            m_reqresqueue = new ConcurrentDictionary<Guid, InMemFrameQueue>();
        }

        internal void Add(Guid id, InMemFrameQueue queue)
        {
            if (!m_reqresqueue.TryAdd(id, queue))
            {
                var message = LogUtil.FatalAndReturnFormatted(
                    "{0}.{1}: Guid collison must never happen for client connection Ids: {2}",
                    nameof(InMemFrameQueueCollection), nameof(Add), id);
                throw new InvalidOperationException(message);
            }
        }

        internal ICollection<Guid> GetKeys()
        {
            return m_reqresqueue.Keys;
        }

        internal InMemFrameQueue GetQueue(Guid id)
        {
            InMemFrameQueue queue;
            m_reqresqueue.TryGetValue(id, out queue);
            return queue;
        }

        internal void ClearAll()
        {
            ICollection<Guid> keys = GetKeys();

            foreach (Guid key in keys)
            {
                GetQueue(key).Clear();
            }
            m_reqresqueue.Clear();
        }
    }

    internal class InMemFrame
    {
        internal SimpleInMemHeaders m_headers;
        internal IBonded m_layerData;
        internal IMessage m_message;
        internal TaskCompletionSource<IMessage> m_outstandingRequest;
    }
}