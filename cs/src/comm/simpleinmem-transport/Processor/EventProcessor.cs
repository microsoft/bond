// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using Service;
    using System;
    using System.Threading.Tasks;
    using Bond.Comm.Layers;

    internal class EventProcessor : QueueProcessor
    {
        readonly SimpleInMemConnection m_connection;
        readonly ServiceHost m_serviceHost;
        readonly InMemFrameQueueCollection m_serverqueues;

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
                var layerData = payload.m_layerData;
                var message = payload.m_message;

                DispatchEvent(headers, layerData, message);
                queueSize = queue.Count(payloadType);
                batchIndex++;
            }
        }

        private void DispatchEvent(SimpleInMemHeaders headers, IBonded layerData, IMessage message)
        {
            var receiveContext = new SimpleInMemReceiveContext(m_connection);

            Error layerError = LayerStackUtils.ProcessOnReceive(m_serviceHost.ParentTransport.LayerStack,
                                                                MessageType.Event, receiveContext, layerData);

            if (layerError != null)
            {
                Log.Error("{0}.{1}: Receiving event {2}/{3} failed due to layer error (Code: {4}, Message: {5}).",
                            this, nameof(DispatchEvent), headers.conversation_id, headers.method_name,
                            layerError.error_code, layerError.message);
                return;
            }

            Task.Run(async () =>
            {
                await m_serviceHost.DispatchEvent(
                    headers.method_name, receiveContext,  message, m_connection.ConnectionMetrics);
            });
        }
    }
}
