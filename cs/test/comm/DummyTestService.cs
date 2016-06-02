// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Comm
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    public partial class Dummy
    {
        public static readonly Dummy Empty = new Dummy();
    }

    public class DummyTestService : DummyTestServiceBase
    {
        public uint RequestCount { get; private set; }
        public Dummy LastRequestReceived { get; private set; } = Dummy.Empty;
        public uint EventCount { get; private set; }
        public Dummy LastEventReceived { get; private set; } = Dummy.Empty;

        ManualResetEventSlim resetEvent = null;
        readonly object lockObject = new object();

        public override Task<IMessage<Dummy>> ReqRspMethodAsync(IMessage<Dummy> param, CancellationToken ct)
        {
            var request = param.Payload.Deserialize();
            var result = new Dummy { int_value = request.int_value + 1 };

            lock (lockObject)
            {
                RequestCount++;
                LastRequestReceived = request;
            }

            return Task.FromResult<IMessage<Dummy>>(Message.FromPayload(result));
        }

        public override void EventMethodAsync(IMessage<Dummy> param)
        {
            var theEvent = param.Payload.Deserialize();

            lock (lockObject)
            {
                EventCount++;
                LastEventReceived = theEvent;

                if (resetEvent != null)
                {
                    resetEvent.Set();
                    resetEvent = null;
                }
            }

        }

        public ManualResetEventSlim CreateResetEvent()
        {
            ManualResetEventSlim prevEvent = null;

            lock (lockObject)
            {
                prevEvent = resetEvent;
                resetEvent = new ManualResetEventSlim(initialState: false);
            }

            if (prevEvent != null)
            {
                throw new InvalidOperationException();
            }

            return resetEvent;
        }

        public void ClearResetEvent()
        {
            resetEvent = null;
        }
    }

    public class GenericDummyTestService : GenericTestServiceBase<Dummy>
    {
        public uint RequestCount { get; private set; }
        public Dummy LastRequestReceived { get; private set; } = Dummy.Empty;
        public uint EventCount { get; private set; }
        public Dummy LastEventReceived { get; private set; } = Dummy.Empty;

        ManualResetEventSlim resetEvent = null;
        readonly object lockObject = new object();

        public override Task<IMessage<Dummy>> ReqRspMethodAsync(IMessage<Dummy> param, CancellationToken ct)
        {
            var request = param.Payload.Deserialize();

            lock (lockObject)
            {
                RequestCount++;
                LastRequestReceived = request;
            }

            var result = new Dummy { int_value = request.int_value + 1 };

            return Task.FromResult<IMessage<Dummy>>(Message.FromPayload(result));
        }

        public override void EventMethodAsync(IMessage<Dummy> param)
        {
            var theEvent = param.Payload.Deserialize();

            lock (lockObject)
            {
                EventCount++;
                LastEventReceived = theEvent;

                if (resetEvent != null)
                {
                    resetEvent.Set();
                    resetEvent = null;
                }
            }

        }

        public ManualResetEventSlim CreateResetEvent()
        {
            ManualResetEventSlim prevEvent = null;

            lock (lockObject)
            {
                prevEvent = resetEvent;
                resetEvent = new ManualResetEventSlim(initialState: false);
            }

            if (prevEvent != null)
            {
                throw new InvalidOperationException();
            }

            return resetEvent;
        }

        public void ClearResetEvent()
        {
            resetEvent = null;
        }
    }


}
