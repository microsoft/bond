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
                headers = headers,
                layerData = layerData,
                message = message,
                outstandingRequest = taskSource
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
            else if (frame.headers == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame.headers)} in frame");
            }
            else if (frame.message == null)
            {
                throw new SimpleInMemProtocolErrorException($"null {nameof(frame.message)} in frame");
            }

            return frame;
        }
    }
}
