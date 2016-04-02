// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.PingPong
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    public class PingPongServiceImpl : PingPongService
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

            if (request.Payload.Contains("4"))
            {
                return Message.FromError<PingResponse>(new Error
                {
                    error_code = 4,
                    message = "Four? Why would you think I like the number 4? Reverse likes 4, but not me.",
                });
            }

            var response = new PingResponse { Payload = request.Payload };
            return Message.FromPayload(response);
        }
    }
}
