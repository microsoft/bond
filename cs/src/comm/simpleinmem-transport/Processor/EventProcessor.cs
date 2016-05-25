// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using Service;
    using System;
    using System.Threading.Tasks;

    internal class EventProcessor : QueueProcessor
    {
        private InMemFrameQueueCollection m_serverqueues;
        private SimpleInMemConnection m_connection;
        private ServiceHost m_serviceHost;

        internal EventProcessor(SimpleInMemConnection connection, ServiceHost host, InMemFrameQueueCollection queues)
        {
            if (connection == null) throw new ArgumentNullException(nameof(connection));
            if (host == null) throw new ArgumentNullException(nameof(host));
            if (queues == null) throw new ArgumentNullException(nameof(queues));

            m_connection = connection;
            m_serviceHost = host;
            m_serverqueues = queues;
        }

        override internal void Process()
        {
            const PayloadType payloadType = PayloadType.Event;

            foreach (Guid key in m_serverqueues.GetKeys())
            {
                InMemFrameQueue queue = m_serverqueues.GetQueue(key);
                Task.Run(() => ProcessQueue(queue, payloadType));
            }
        }

        private void ProcessQueue(InMemFrameQueue queue, PayloadType payloadType)
        {
            int queueSize = queue.Count(payloadType);

            int batchIndex = 0;

            if (queueSize == 0)
            {
                return;
            }

            while (batchIndex < PROCESSING_BATCH_SIZE && queueSize > 0)
            {
                var payload = queue.Dequeue(payloadType);
                var headers = payload.m_headers;
                var message = payload.m_message;

                DispatchEvent(headers, message);
                queueSize = queue.Count(payloadType);
                batchIndex++;
            }
        }

        private void DispatchEvent(SimpleInMemHeaders headers, IMessage message)
        {
            Task.Run(async () =>
            {
                await m_serviceHost.DispatchEvent(headers.method_name, new SimpleInMemReceiveContext(m_connection), message);
            });
        }
    }
}
