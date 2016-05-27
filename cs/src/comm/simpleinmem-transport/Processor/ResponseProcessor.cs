// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm.Layers;

    internal class ResponseProcessor : QueueProcessor
    {
        readonly SimpleInMemConnection connection;
        readonly Transport parentTransport;
        readonly InMemFrameQueue clientreqresqueue;

        internal ResponseProcessor(SimpleInMemConnection connection, Transport parentTransport, InMemFrameQueue queue)
        {
            if (queue == null) throw new ArgumentNullException(nameof(queue));
            if (connection == null) throw new ArgumentNullException(nameof(connection));

            this.connection = connection;
            this.parentTransport = parentTransport;
            clientreqresqueue = queue;
        }

        override internal void Process()
        {
            const PayloadType payloadType = PayloadType.Response;
            int queueSize = clientreqresqueue.Count(payloadType);
            int batchIndex = 0;

            if (queueSize == 0)
            {
                return;
            }

            while (batchIndex < PROCESSING_BATCH_SIZE && queueSize > 0)
            {
                var frame = clientreqresqueue.Dequeue(payloadType);
                var headers = frame.headers;
                var layerData = frame.layerData;
                var message = frame.message;
                var taskSource = frame.outstandingRequest;

                DispatchResponse(headers, layerData, message, taskSource);
                queueSize = clientreqresqueue.Count(payloadType);
                batchIndex++;
            }
        }

        private void DispatchResponse(SimpleInMemHeaders headers, IBonded layerData, IMessage message,
                                        TaskCompletionSource<IMessage> responseCompletionSource)
        {
            var receiveContext = new SimpleInMemReceiveContext(connection);

            Error layerError = LayerStackUtils.ProcessOnReceive(parentTransport.LayerStack,
                                                                MessageType.Response, receiveContext, layerData);

            if (layerError != null)
            {
                Log.Error("{0}.{1}: Receiving response {2}/{3} failed due to layer error (Code: {4}, Message: {5}).",
                            this, nameof(DispatchResponse), headers.conversation_id, headers.method_name,
                            layerError.error_code, layerError.message);
                message = Message.FromError(layerError);
            }

            responseCompletionSource.SetResult(message);
        }
    }
}
