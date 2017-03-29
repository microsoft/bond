// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcPingPong
{
    using System;
    using System.Threading.Tasks;
    using Bond.Grpc;
    using Grpc.Core;

    public class DoublePingService : DoublePing.DoublePingBase
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

            var response = new PingResponse { Payload = request.Payload + request.Payload };
            return Message.From(response);
        }

        public override async Task<IMessage<PingResponse>> PingNoPayload(IMessage<Bond.Void> v, ServerCallContext context)
        {
            await Task.Delay(MaxDelayMilliseconds);

            var response = new PingResponse { Payload = "Ping with no payload" };
            return Message.From(response);
        }

        public override async Task PingNoResponse(IMessage<PingRequest> param, ServerCallContext context)
        {
            PingRequest request = param.Payload.Deserialize();

            if (request.DelayMilliseconds > 0)
            {
                UInt16 delayMs = Math.Min(MaxDelayMilliseconds, request.DelayMilliseconds);
                await Task.Delay(delayMs);
            }

            Console.WriteLine("PingNoResponse: Notified server-side");
        }

        public override async Task<IMessage<Bond.Void>> PingVoid(IMessage<Bond.Void> v, ServerCallContext context)
        {
            await Task.Delay(MaxDelayMilliseconds);

            Console.WriteLine("PingVoidReturn: Notified server-side");

            return Message.Void;
        }

        public override Task<IMessage<PingResponse>> PingShouldThrow(IMessage<PingRequest> param, ServerCallContext context)
        {
            throw new RpcException(new Status(StatusCode.Unimplemented, "Not implemented yet"));
        }
    }
}
