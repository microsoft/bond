// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcStreaming
{
    using System.Collections.Generic;
    using System.Threading.Tasks;
    using Bond;
    using Bond.Grpc;
    using Grpc.Core;

    public class StreamingService : StreamingMethods.StreamingMethodsBase
    {
        public override async Task<IMessage<Box<uint>>> Sum(IAsyncStreamReader<IMessage<Box<uint>>> requests, ServerCallContext context)
        {
            uint total = 0;

            while (await requests.MoveNext())
            {
                var msg = requests.Current;
                uint value = msg.Payload.Deserialize().value;
                total = unchecked(total + value);
            }

            return Message.From(Box.Create(total));
        }

        public override async Task Countdown(IMessage<Box<uint>> request, IAsyncStreamWriter<IMessage<Box<uint>>> responses, ServerCallContext context)
        {
            uint num = request.Payload.Deserialize().value;

            while (num > 0)
            {
                await responses.WriteAsync(Message.From(Box.Create(num)));
                --num;
            }
        }

        public override async Task Reverse(IAsyncStreamReader<IMessage<Box<string>>> requests, IAsyncStreamWriter<IMessage<Box<string>>> responses, ServerCallContext context)
        {
            while (await requests.MoveNext())
            {
                var msg = requests.Current;
                string value = msg.Payload.Deserialize().value;

                await responses.WriteAsync(Message.From(Box.Create(value.ToUpperInvariant())));
            }

            // Requests and responses do not have to correspond 1:1. Both of the streams can be handled independently.
            await responses.WriteAsync(Message.From(Box.Create("BONUS!")));
        }
    }
}
