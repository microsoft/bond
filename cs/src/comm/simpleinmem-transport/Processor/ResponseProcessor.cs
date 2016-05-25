// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm.Layers;

    internal class ResponseProcessor : QueueProcessor
    {
        readonly SimpleInMemConnection m_connection;
        readonly Transport m_parentTransport;
        readonly InMemFrameQueue m_clientreqresqueue;

        internal ResponseProcessor(SimpleInMemConnection connection, Transport parentTransport, InMemFrameQueue queue)
        {
            if (queue == null) throw new ArgumentNullException(nameof(queue));
            if (connection == null) throw new ArgumentNullException(nameof(connection));

            m_connection = connection;
            m_parentTransport = parentTransport;
            m_clientreqresqueue = queue;
        }

        override internal void Process()
        {
            const PayloadType payloadType = PayloadType.Response;
            int queueSize = m_clientreqresqueue.Count(payloadType);
            int batchIndex = 0;

            if (queueSize == 0)
            {
                return;
            }

            while (batchIndex < PROCESSING_BATCH_SIZE && queueSize > 0)
            {
                var frame = m_clientreqresqueue.Dequeue(payloadType);
                var headers = frame.m_headers;
                var layerData = frame.m_layerData;
                var message = frame.m_message;
                var taskSource = frame.m_outstandingRequest;

                DispatchResponse(headers, layerData, message, taskSource);
                queueSize = m_clientreqresqueue.Count(payloadType);
                batchIndex++;
            }
        }

        private void DispatchResponse(SimpleInMemHeaders headers, IBonded layerData, IMessage message,
                                        TaskCompletionSource<IMessage> responseCompletionSource)
        {
            var receiveContext = new SimpleInMemReceiveContext(m_connection);

            Error layerError = LayerStackUtils.ProcessOnReceive(m_parentTransport.LayerStack,
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
