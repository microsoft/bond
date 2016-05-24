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
        [Ignore("Need to implement protocol handshake to be able to test this properly.")]
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

            var disconnectedEventDone = new ManualResetEventSlim(initialState: false);
            listener.Disconnected += (sender, args) =>
            {
                disconnectedEventDone.Set();
            };

            await listener.StartAsync();
            // ConnectToAsync should throw.
            var connection = await transport.ConnectToAsync(listener.ListenEndpoint);
        }

        [Test]
        public async Task DisconnectedEvent_ClientDisconnects_GetsFired()
        {
            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);

            var disconnectedEventDone = new ManualResetEventSlim(initialState: false);
            EpoxyConnection disconnectedConnection = null;
            listener.Disconnected += (sender, args) =>
            {
                disconnectedConnection = (EpoxyConnection)args.Connection;
                disconnectedEventDone.Set();
            };

            await listener.StartAsync();
            var connection = await transport.ConnectToAsync(listener.ListenEndpoint);
            await connection.StopAsync();

            bool wasSignaled = disconnectedEventDone.Wait(TimeSpan.FromSeconds(30));
            Assert.IsTrue(wasSignaled, "Timed out waiting for Disconnected event to complete");

            Assert.IsNotNull(disconnectedConnection);
            Assert.AreEqual(connection.LocalEndPoint, disconnectedConnection.RemoteEndPoint);
        }

        private static EpoxyTransport MakeTransport()
        {
            var transport = new EpoxyTransportBuilder().Construct();
            return transport;
        }
    }
}
