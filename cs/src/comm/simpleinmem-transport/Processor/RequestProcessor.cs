// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm.Layers;
    using Bond.Comm.Service;

    internal class RequestProcessor : QueueProcessor
    {
        readonly SimpleInMemConnection connection;
        readonly ServiceHost serviceHost;
        readonly InMemFrameQueueCollection serverqueues;

        internal RequestProcessor(SimpleInMemConnection connection, ServiceHost host, InMemFrameQueueCollection queues)
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
            const PayloadType payloadType = PayloadType.Request;

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
                var taskSource = payload.outstandingRequest;

                Task.Run(() => DispatchRequest(headers, layerData, message, queue, taskSource));
                queueSize = queue.Count(payloadType);
                batchIndex++;
            }
        }

        private async void DispatchRequest(SimpleInMemHeaders headers, IBonded layerData, IMessage message, InMemFrameQueue queue,
                                            TaskCompletionSource<IMessage> taskSource)
        {
            var receiveContext = new SimpleInMemReceiveContext(connection);

            Error layerError = LayerStackUtils.ProcessOnReceive(this.serviceHost.ParentTransport.LayerStack,
                                                                MessageType.Request, receiveContext, layerData);

            IMessage response;

            if (layerError == null)
            {
                response = await serviceHost.DispatchRequest(headers.method_name, receiveContext, message, connection.ConnectionMetrics);
            }
            else
            {
                Log.Error("{0}.{1}: Receiving request {2}/{3} failed due to layer error (Code: {4}, Message: {5}).",
                            this, nameof(DispatchRequest), headers.conversation_id, headers.method_name,
                            layerError.error_code, layerError.message);
                response = Message.FromError(layerError);
            }
            SendReply(headers.conversation_id, response, queue, taskSource);
        }

        internal void SendReply(ulong conversationId, IMessage response, InMemFrameQueue queue,
                                TaskCompletionSource<IMessage> taskSource)
        {
            var sendContext = new SimpleInMemSendContext(connection);
            IBonded layerData;
            Error layerError = LayerStackUtils.ProcessOnSend(this.serviceHost.ParentTransport.LayerStack,
                                                             MessageType.Response, sendContext, out layerData);

            // If there was a layer error, replace the response with the layer error
            if (layerError != null)
            {
                Log.Error("{0}.{1}: Sending reply for request ID {2} failed due to layer error (Code: {3}, Message: {4}).",
                            this, nameof(SendReply), conversationId, layerError.error_code, layerError.message);
                response = Message.FromError(layerError);
            }

            var payload = Util.NewPayLoad(conversationId, PayloadType.Response, layerData, response, taskSource);
            queue.Enqueue(payload);
        }
    }
}
