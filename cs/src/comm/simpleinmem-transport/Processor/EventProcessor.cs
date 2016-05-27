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
        readonly SimpleInMemConnection connection;
        readonly ServiceHost serviceHost;
        readonly InMemFrameQueueCollection serverqueues;

        internal EventProcessor(SimpleInMemConnection connection, ServiceHost host, InMemFrameQueueCollection queues)
        {
            if (connection == null) throw new ArgumentNullException(nameof(connection));
            if (host == null) throw new ArgumentNullException(nameof(host));
            if (queues == null) throw new ArgumentNullException(nameof(queues));

            this.connection = connection;
            serviceHost = host;
            serverqueues = queues;
        }

        override internal void Process()
        {
            const PayloadType payloadType = PayloadType.Event;

            foreach (Guid key in serverqueues.GetKeys())
            {
                InMemFrameQueue queue = serverqueues.GetQueue(key);
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
                var headers = payload.headers;
                var layerData = payload.layerData;
                var message = payload.message;

                DispatchEvent(headers, layerData, message);
                queueSize = queue.Count(payloadType);
                batchIndex++;
            }
        }

        private void DispatchEvent(SimpleInMemHeaders headers, IBonded layerData, IMessage message)
        {
            var receiveContext = new SimpleInMemReceiveContext(connection);

            Error layerError = LayerStackUtils.ProcessOnReceive(serviceHost.ParentTransport.LayerStack,
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
                await serviceHost.DispatchEvent(
                    headers.method_name, receiveContext,  message, connection.ConnectionMetrics);
            });
        }
    }
}
