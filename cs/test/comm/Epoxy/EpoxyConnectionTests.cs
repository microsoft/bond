// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.IO.Safe;
    using Bond.Protocols;
    using NUnit.Framework;
    using UnitTest.Interfaces;

    [TestFixture]
    public class EpoxyConnectionTests : EpoxyTestBase
    {
        private const ProtocolErrorCode MeaninglessErrorCode = ProtocolErrorCode.GENERIC_ERROR;

        private static readonly Error AnyDetails = new Error
        {
            error_code = (int) ErrorCode.METHOD_NOT_FOUND,
            message = "This is some error message"
        };

        [Test]
        public void MakeProtocolErrorFrame_JustErrorCode_MakesAFrame()
        {
            var frame = EpoxyConnection.MakeProtocolErrorFrame(MeaninglessErrorCode, null, LoggerTests.BlackHole);
            Assert.NotNull(frame);
            Assert.AreEqual(1, frame.Framelets.Count);
            Assert.AreEqual(FrameletType.ProtocolError, frame.Framelets[0].Type);

            var inputBuffer = new InputBuffer(frame.Framelets[0].Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            var error = Deserialize<ProtocolError>.From(fastBinaryReader);
            Assert.AreEqual(MeaninglessErrorCode, error.error_code);
            Assert.Null(error.details);
        }

        [Test]
        public void MakeProtocolErrorFrame_WithDetails_MakesAFrame()
        {
            var frame = EpoxyConnection.MakeProtocolErrorFrame(MeaninglessErrorCode, AnyDetails, LoggerTests.BlackHole);
            Assert.NotNull(frame);
            Assert.AreEqual(1, frame.Framelets.Count);
            Assert.AreEqual(FrameletType.ProtocolError, frame.Framelets[0].Type);

            var inputBuffer = new Bond.IO.Unsafe.InputBuffer(frame.Framelets[0].Contents);
            var fastBinaryReader = new FastBinaryReader<InputBuffer>(inputBuffer, version: 1);
            var error = Deserialize<ProtocolError>.From(fastBinaryReader);
            Assert.AreEqual(MeaninglessErrorCode, error.error_code);
            Assert.NotNull(error.details);
            var details = error.details.Deserialize();
            Assert.IsTrue(AnyDetails.IsEqual<Error, Error>(details));
        }

        [Test]
        public async Task Connection_StartStop()
        {
            var testClientServer = await SetupTestClientServer<TestService>();
            EpoxyConnection connection = testClientServer.ClientConnection;

            var timeoutTask = Task.Delay(TimeSpan.FromSeconds(10));
            var completedTask = await Task.WhenAny(connection.StopAsync(), timeoutTask);

            Assert.AreNotSame(timeoutTask, completedTask, "Timed out waiting for connection to be shutdown.");
        }

        [Test]
        public async Task Connection_CanBeStoppedMultipleTimes()
        {
            var testClientServer = await SetupTestClientServer<TestService>();
            EpoxyConnection connection = testClientServer.ClientConnection;

            var stopTasks = Task.WhenAll(connection.StopAsync(), connection.StopAsync());
            var timeoutTask = Task.Delay(TimeSpan.FromSeconds(10));

            var completedTask = await Task.WhenAny(stopTasks, timeoutTask);

            Assert.AreNotSame(timeoutTask, completedTask, "Timed out waiting for connection to be shutdown.");
        }

        [Test]
        public async Task Connection_OutstandingRequestsThenShutdown_CompletedWithError()
        {
            var testClientServer = await SetupTestClientServer<TestServiceNeverResponds>();
            EpoxyConnection connection = testClientServer.ClientConnection;

            IMessage<SomePayload> anyPayload = Message.FromPayload(new SomePayload());
            Task<IMessage<SomePayload>> responseTask = connection
                .RequestResponseAsync<SomePayload, SomePayload>(
                    "TestService", "NeverRespond", anyPayload, CancellationToken.None);

            await connection.StopAsync();

            IMessage<SomePayload> response = await responseTask;

            Assert.IsTrue(response.IsError);
            Error err = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.CONNECTION_SHUT_DOWN, err.error_code);
        }

        class TestServiceNeverResponds : IService
        {
            public IEnumerable<ServiceMethodInfo> Methods => new[]
            {
                new ServiceMethodInfo
                {
                    MethodName = "TestService.NeverRespond",
                    Callback = NeverRespond,
                    CallbackType = ServiceCallbackType.RequestResponse
                },
            };

            Task<IMessage> NeverRespond(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                return new TaskCompletionSource<IMessage>().Task;
            }
        }
    }
}
