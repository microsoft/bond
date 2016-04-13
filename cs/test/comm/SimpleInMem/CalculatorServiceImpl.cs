// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    internal class CalculatorService : CalculatorServiceBase
    {
        public const string ExpectedExceptionMessage = "This method is expected to throw.";
        private const UInt16 DelayMilliseconds = 30;

        public override async Task<IMessage<Output>> AddAsync(IMessage<PairedInput> request, CancellationToken ct)
        {
            PairedInput req = request.Convert<PairedInput>().Payload.Deserialize();
            var res = new Output { Result = req.First + req.Second };
            await Task.Delay(DelayMilliseconds);

            return Message.FromPayload(res);
        }

        public override async Task<IMessage<Output>> SubtractAsync(IMessage<PairedInput> request, CancellationToken ct)
        {
            PairedInput req = request.Convert<PairedInput>().Payload.Deserialize();
            var res = new Output { Result = req.First - req.Second };
            await Task.Delay(DelayMilliseconds);

            return Message.FromPayload(res);
        }

        public override Task<IMessage<Output>> MultiplyAsync(IMessage<PairedInput> request, CancellationToken ct)
        {
            throw new NotImplementedException(ExpectedExceptionMessage);
        }
    }
}