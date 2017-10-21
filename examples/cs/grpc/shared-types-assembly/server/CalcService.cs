// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.SharedTypes
{
    using System.Threading.Tasks;
    using Bond.Grpc;
    using Grpc.Core;

    public class CalcService : Calc.CalcBase
    {
        public override Task<IMessage<Response>> Add(IMessage<Request> request, ServerCallContext context)
        {
            var r = request.Payload.Deserialize();
            var response = new Response
            {
                Result = r.Num1 + r.Num2
            };

            return Task.FromResult(Message.From(response));
        }
    }
}
