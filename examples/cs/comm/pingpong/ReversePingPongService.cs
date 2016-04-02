// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.PingPong
{
    using System;
    using System.Linq;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    public class ReversePingPongServiceImpl : PingPongService
    {
        private const UInt16 MaxDelayMilliseconds = 2000;

        public override async Task<IMessage<PingResponse>> PingAsync(IMessage<PingRequest> param, CancellationToken ct)
        {
            PingRequest request = param.Payload.Deserialize();

            if (request.DelayMilliseconds > 0)
            {
                UInt16 delayMs = Math.Min(MaxDelayMilliseconds, request.DelayMilliseconds);
                await Task.Delay(delayMs);
            }

            var response = new PingResponse { Payload = string.Concat(request.Payload.Reverse()) };
            return Message.FromPayload(response);
        }
    }
}
