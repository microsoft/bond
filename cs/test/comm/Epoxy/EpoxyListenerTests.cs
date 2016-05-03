// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;
    using UnitTest.Interfaces;

    [TestFixture]
    public class EpoxyListenerTests
    {
        private static readonly IPEndPoint localhostEndpoint = new IPEndPoint(IPAddress.Loopback, 0);

        [Test]
        public async Task ListenOnPortZero_ActuallyListensOnSomeOtherPort()
        {
            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);

            await listener.StartAsync();

            Assert.AreNotEqual(0, listener.ListenEndpoint.Port);

            await listener.StopAsync();
        }

        [Test]
        public async Task ConnectedEvent_HasRightRemoteEndpointDetails()
        {
            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);

            EpoxyConnection remoteConnection = null;
            var connectedEventDone = new ManualResetEventSlim(initialState: false);

            listener.Connected += (sender, args) =>
            {
                Assert.AreSame(listener, sender);
                remoteConnection = (EpoxyConnection)args.Connection;
                connectedEventDone.Set();
            };

            await listener.StartAsync();
            var connection = await transport.ConnectToAsync(listener.ListenEndpoint);
            bool wasSignaled = connectedEventDone.Wait(TimeSpan.FromSeconds(30));
            Assert.IsTrue(wasSignaled, "Timed out waiting for Connected event to complete");

            Assert.AreEqual(connection.LocalEndPoint, remoteConnection.RemoteEndPoint);
            Assert.AreEqual(connection.RemoteEndPoint, remoteConnection.LocalEndPoint);

            await transport.StopAsync();
        }

        [Test]
        [Ignore("Connection shutdown not yet implemented")]
        public async Task ConnectedEvent_SetDisconnectError_DisconnectsConnection()
        {
            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);

            var connectedEventDone = new ManualResetEventSlim(initialState: false);
            listener.Connected += (sender, args) =>
            {
                args.DisconnectError = new Error {error_code = 100, message = "Go away!"};
                connectedEventDone.Set();
            };

            await listener.StartAsync();
            var connection = await transport.ConnectToAsync(listener.ListenEndpoint);
            bool wasSignaled = connectedEventDone.Wait(TimeSpan.FromMinutes(30));
            Assert.IsTrue(wasSignaled, "Timed out waiting for Connected event to complete");

            IMessage<SomePayload> response =
                await connection.RequestResponseAsync<SomePayload, SomePayload>(
                    "some method",
                    Message.FromPayload(new SomePayload()),
                    CancellationToken.None);

            Assert.IsTrue(response.IsError);
            // TODO: correct error code
            Assert.AreEqual(0, response.Error.Deserialize().error_code);
        }

        private static EpoxyTransport MakeTransport()
        {
            var transport = new EpoxyTransportBuilder()
                .SetUnhandledExceptionHandler(Transport.ToErrorExceptionHandler)
                .Construct();
            return transport;
        }
    }
}
