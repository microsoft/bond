// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Collections.Concurrent;
    using System.Collections.Generic;
    using System.Threading.Tasks;

    internal class RequestResponseQueue
    {
        private Queue<InMemFrame> m_requests;
        private Queue<InMemFrame> m_responses;
        private object m_lockreq = new object();
        private object m_lockres = new object();

        internal RequestResponseQueue()
        {
            m_requests = new Queue<InMemFrame>();
            m_responses = new Queue<InMemFrame>();
        }

        internal void Enqueue(InMemFrame frame)
        {
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

                default:
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Payload type {2} not supported!",
                        nameof(RequestResponseQueue), nameof(Enqueue), frame.m_headers.payload_type);
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

                default:
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Payload type {2} not supported!",
                        nameof(RequestResponseQueue), nameof(Dequeue), payloadType);
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

                default:
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Payload type {2} not supported!",
                        nameof(RequestResponseQueue), nameof(Count), payloadType);
                    throw new NotImplementedException(message);
            }

            return count;
        }
    }

    internal class RequestResponseQueueCollection
    {
        private ConcurrentDictionary<Guid, RequestResponseQueue> m_reqresqueue;

        internal RequestResponseQueueCollection()
        {
            m_reqresqueue = new ConcurrentDictionary<Guid, RequestResponseQueue>();
        }

        internal void AddRequestResponseQueue(Guid id, RequestResponseQueue queue)
        {
            if (!m_reqresqueue.TryAdd(id, queue))
            {
                var message = LogUtil.FatalAndReturnFormatted(
                    "{0}.{1}: Guid collison must never happen for client connection Ids: {2}",
                    nameof(RequestResponseQueueCollection), nameof(AddRequestResponseQueue), id);
                throw new InvalidOperationException(message);
            }
        }

        internal ICollection<Guid> GetKeys()
        {
            return m_reqresqueue.Keys;
        }

        internal RequestResponseQueue GetQueue(Guid id)
        {
            RequestResponseQueue queue;
            m_reqresqueue.TryGetValue(id, out queue);
            return queue;
        }
    }

    internal class InMemFrame
    {
        internal SimpleInMemHeaders m_headers;
        internal IMessage m_message;
        internal TaskCompletionSource<IMessage> m_outstandingRequest;
    }
}