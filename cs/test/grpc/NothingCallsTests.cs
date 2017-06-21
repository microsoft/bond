// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Grpc
{
    using System;
    using System.Threading.Tasks;

    using Bond.Grpc;
    using Bond.Grpc.Internal;
    using global::Grpc.Core;
    using NUnit.Framework;

    [TestFixture]
    class NothingCallsTests
    {
        [Test]
        public async Task NoOpHandler_Returns()
        {
            await NothingCallHandler.Handle(NoOpHandler_Param, Message.From(SomePayload.Any), null);
            await NothingCallHandler.Handle(NoOpHandler_NoParam, null);
        }

        [Test]
        public async Task HandlerThrows_Returns()
        {
            await NothingCallHandler.Handle(HandlerThrows_Param, Message.From(SomePayload.Any), null);
            await NothingCallHandler.Handle(HandlerThrows_NoParam, null);
        }

        [Test]
        public async Task HandlerReturnsFaultedTask_Returns()
        {
            await NothingCallHandler.Handle(HandlerFaultedTask_Param, Message.From(SomePayload.Any), null);
            await NothingCallHandler.Handle(HandlerFaultedTask_NoParam, null);
        }

        private static Task NoOpHandler_Param(IMessage<SomePayload> request, ServerCallContext context)
        {
            return Task.FromResult(false);
        }

        private static Task NoOpHandler_NoParam(ServerCallContext context)
        {
            return Task.FromResult(false);
        }

        private static Task HandlerThrows_Param(IMessage<SomePayload> request, ServerCallContext context)
        {
            throw new Exception("Handler throws");
        }

        private static Task HandlerThrows_NoParam(ServerCallContext context)
        {
            throw new Exception("Handler throws");
        }

        private static Task HandlerFaultedTask_Param(IMessage<SomePayload> request, ServerCallContext context)
        {
            var tcs = new TaskCompletionSource<bool>();
            tcs.SetException(new Exception("Handler throws"));
            return tcs.Task;
        }

        private static Task HandlerFaultedTask_NoParam(ServerCallContext context)
        {
            var tcs = new TaskCompletionSource<bool>();
            tcs.SetException(new Exception("Handler throws"));
            return tcs.Task;
        }
    }
}
