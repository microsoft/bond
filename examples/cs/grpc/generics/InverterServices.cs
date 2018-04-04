// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcGenerics
{
    using System.Collections.Generic;
    using System.Linq;
    using System.Threading.Tasks;

    using Bond;
    using Bond.Grpc;
    using Grpc.Core;

    class Int32Inverter : Inverter<Box<int>>.InverterBase
    {
        public override async Task Invert(IAsyncStreamReader<IMessage<Box<int>>> requests, IAsyncStreamWriter<IMessage<Box<int>>> responses, ServerCallContext context)
        {
            while (await requests.MoveNext())
            {
                var requestInt = requests.Current.Payload.Deserialize().value;
                var resultInt = ~requestInt;
                await responses.WriteAsync(Message.From(Box.Create(resultInt)));
            }
        }
    }

    class StringInverter : Inverter<Box<string>>.InverterBase
    {
        public override async Task Invert(IAsyncStreamReader<IMessage<Box<string>>> requests, IAsyncStreamWriter<IMessage<Box<string>>> responses, ServerCallContext context)
        {
            while (await requests.MoveNext())
            {
                var requestStr = requests.Current.Payload.Deserialize().value;
                var resultStr = new string(requestStr.Reverse().ToArray());
                await responses.WriteAsync(Message.From(Box.Create(resultStr)));
            }
        }
    }
}
