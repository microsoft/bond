// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.PingPong
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Threading.Tasks;

    using Bond.Comm;

    public class ReversePingPongService : IPingPongService, IService
    {
        private const UInt16 MaxDelayMilliseconds = 2000;

        public IEnumerable<Comm.ServiceMethodInfo> Methods
        {
            get
            {
                var pingMethodInfo = new Comm.ServiceMethodInfo { MethodName = "Bond.Examples.ReversePingPong.Ping", Callback = PingAsync_Glue };
                return new[] { pingMethodInfo };
            }
        }

        public async Task<IMessage<PingResponse>> PingAsync(IMessage<PingRequest> message)
        {
            PingRequest request = message.Payload.Deserialize();

            if (request.DelayMilliseconds > 0)
            {
                UInt16 delayMs = Math.Min(MaxDelayMilliseconds, request.DelayMilliseconds);
                await Task.Delay(delayMs);
            }

            var response = new PingResponse { Payload = string.Concat(request.Payload.Reverse()) };
            return Message.FromPayload(response);
        }

        private async Task<IMessage> PingAsync_Glue(IMessage message, ReceiveContext context)
        {
            return await PingAsync(message.Convert<PingRequest>());
        }
    }
}
