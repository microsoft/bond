// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem.Processor
{
    using System.Threading.Tasks;

    internal static class Util
    {
        internal static InMemFrame NewPayLoad(ulong conversationId, PayloadType payloadType, IBonded layerData,
                                                IMessage message, TaskCompletionSource<IMessage> taskSource)
        {
            var headers = new SimpleInMemHeaders
            {
                conversation_id = conversationId,
                payload_type = payloadType
            };

            var payload = new InMemFrame
            {
                m_headers = headers,
                m_layerData = layerData,
                m_message = message,
                m_outstandingRequest = taskSource
            };

            Validate(payload);
            return payload;
        }
        
        internal static InMemFrame Validate(InMemFrame frame)
        {
            if (frame == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame)}");
            }
            else if (frame.m_headers == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame.m_headers)} in frame");
            }
            else if (frame.m_message == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame.m_message)} in frame");
            }

            return frame;
        }
    }
}
