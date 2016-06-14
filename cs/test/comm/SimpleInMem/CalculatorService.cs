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
        private const UInt16 DelayMilliseconds = 30;
        private static object IncrementLock = new object();

        public static ManualResetEvent ClearCalledEvent { get; set; } = new ManualResetEvent(false);
        public static int Count;

        public override async Task<IMessage<Output>> AddAsync(IMessage<PairedInput> request, CancellationToken ct)
        {
            PairedInput req = request.Convert<PairedInput>().Payload.Deserialize();
            var res = new Output
            {
                Result = req.First + req.Second,
                TraceId = req.TraceId
            };
            await Task.Delay(DelayMilliseconds);

            return Message.FromPayload(res);
        }

        public override async Task<IMessage<Output>> SubtractAsync(IMessage<PairedInput> request, CancellationToken ct)
        {
            PairedInput req = request.Convert<PairedInput>().Payload.Deserialize();
            var res = new Output
            {
                Result = req.First - req.Second,
                TraceId = req.TraceId
            };
            await Task.Delay(DelayMilliseconds);

            return Message.FromPayload(res);
        }

        public override Task<IMessage<Output>> MultiplyAsync(IMessage<PairedInput> request, CancellationToken ct)
        {
            throw new NotImplementedException();
        }

        public override void ClearAsync(IMessage<Bond.Void> param)
        {
            ClearCalledEvent.Set();
        }

        public override void IncrementCountAsync(IMessage<Bond.Void> param)
        {
            lock (IncrementLock)
            {
                Count++;
            }
        }
    }
}