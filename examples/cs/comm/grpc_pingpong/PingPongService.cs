// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcPingPong
{
    using System;
    using System.Threading.Tasks;
    using Bond.Grpc;
    using Grpc.Core;

    public class PingPongService : PingPong<PingRequest>.PingPongBase
    {
        private const UInt16 MaxDelayMilliseconds = 2000;

        public override async Task<IMessage<PingResponse>> Ping(IMessage<PingRequest> param, ServerCallContext context)
        {
            PingRequest request = param.Payload.Deserialize();

            if (request.DelayMilliseconds > 0)
            {
                UInt16 delayMs = Math.Min(MaxDelayMilliseconds, request.DelayMilliseconds);
                await Task.Delay(delayMs);
            }

            var response = new PingResponse { Payload = request.Payload };
            return Message.From(response);
        }
    }
}
