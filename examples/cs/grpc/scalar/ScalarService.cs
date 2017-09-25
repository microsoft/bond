// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcScalar
{
    using System.Collections.Generic;
    using System.Threading.Tasks;
    using Bond;
    using Bond.Grpc;
    using Grpc.Core;

    public class ScalarService : ScalarMethods.ScalarMethodsBase
    {
        public override Task<IMessage<Box<int>>> Negate(IMessage<Box<int>> param, ServerCallContext context)
        {
            Box<int> request = param.Payload.Deserialize();
            var response = Box.Create(-request.value);
            return Task.FromResult(Message.From(response));
        }

        public override Task<IMessage<Box<ulong>>> Sum(IMessage<Box<List<ulong>>> param, ServerCallContext context)
        {
            Box<List<ulong>> request = param.Payload.Deserialize();
            var response = new Box<ulong>();
            foreach (var v in request.value)
            {
                unchecked
                {
                    response.value += v;
                }
            }

            return Task.FromResult(Message.From(response));
        }
    }
}
