// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.PingPong
{
    using System;
    using System.Collections.Generic;
    using System.Threading.Tasks;

    using Bond.Comm;

    public class PingPongService : IPingPongService, IService
    {
        private const UInt16 MaxDelayMilliseconds = 2000;

        public IEnumerable<ServiceMethodInfo> Methods
        {
            get
            {
                var pingMethodInfo = new ServiceMethodInfo { MethodName = "Bond.Examples.PingPong.Ping", Callback = PingAsync_Glue };
                return new[] { pingMethodInfo };
            }
        }

        public async Task<IMessage<PingResponse>> PingAsync(IMessage<PingRequest> message)
        {
            var request = message.Payload.Deserialize();

            if (request.DelayMilliseconds > 0)
            {
                UInt16 delayMs = Math.Min(MaxDelayMilliseconds, request.DelayMilliseconds);
                await Task.Delay(delayMs);
            }

            var response = new PingResponse { Payload = request.Payload };
            return Message.FromPayload(response);
        }

        private async Task<IMessage> PingAsync_Glue(IMessage message, ReceiveContext context)
        {
            return await PingAsync(message.Convert<PingRequest>());
        }
    }
}
