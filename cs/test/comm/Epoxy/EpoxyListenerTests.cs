// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Net;
    using System.Net.Sockets;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;
    using UnitTest.Comm;

    [TestFixture]
    public class EpoxyListenerTests : EpoxyTestBase
    {
        private const string AnyServiceName = "AnyServiceName";
        private const string AnyMethodName = "AnyMethodName";
        private static readonly IMessage<Dummy> AnyMessage = Message.FromPayload(new Dummy());

        private static readonly IPEndPoint localhostEndpoint = new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultInsecurePort);
        private static readonly string localhostAddress = "epoxy://127.0.0.1";

        [Test]
        public async Task ListenOnPortZero_ActuallyListensOnSomeOtherPort()
        {
            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);

            await listener.StartAsync();

            Assert.AreNotEqual(0, listener.ListenEndpoint.Port);
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
            var connection = await transport.ConnectToAsync(localhostAddress);
            bool wasSignaled = connectedEventDone.Wait(TimeSpan.FromSeconds(10));
            Assert.IsTrue(wasSignaled, "Timed out waiting for Connected event to complete");

            Assert.AreEqual(connection.LocalEndPoint, remoteConnection.RemoteEndPoint);
            Assert.AreEqual(connection.RemoteEndPoint, remoteConnection.LocalEndPoint);
        }

        [Test]
        [Ignore("This test has a race between socket shutdown and reading the protocol error")]
        public async Task ConnectedEvent_SetDisconnectError_DisconnectsConnection()
        {
            const int DisconnectErrorCode = 100;
            const string DisconnectMessage = "Go away!";

            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);

            var connectedEventDone = new ManualResetEventSlim(initialState: false);
            listener.Connected += (sender, args) =>
            {
                args.DisconnectError = new Error {error_code = DisconnectErrorCode, message = DisconnectMessage };
                connectedEventDone.Set();
            };

            var disconnectedEventDone = new ManualResetEventSlim(initialState: false);
            listener.Disconnected += (sender, args) =>
            {
                disconnectedEventDone.Set();
            };

            await listener.StartAsync();

            try
            {
                await transport.ConnectToAsync(localhostAddress);
                Assert.Fail("Expected exception to be thrown.");
            }
            catch (EpoxyProtocolErrorException pex)
            {
                Assert.That(pex.Message, Is.StringContaining("rejected"));
                Assert.IsNotNull(pex.Details);

                var errorDetails = pex.Details.Deserialize();
                Assert.AreEqual(DisconnectErrorCode, errorDetails.error_code);
                Assert.AreEqual(DisconnectMessage, errorDetails.message);
            }
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
            var connection = await transport.ConnectToAsync(localhostAddress);
            await connection.StopAsync();

            bool wasSignaled = disconnectedEventDone.Wait(TimeSpan.FromSeconds(10));
            Assert.IsTrue(wasSignaled, "Timed out waiting for Disconnected event to complete");

            Assert.IsNotNull(disconnectedConnection);
            Assert.AreEqual(connection.LocalEndPoint, disconnectedConnection.RemoteEndPoint);
        }

        [Test]
        public async Task OneConnectionStalledDuringHandshake_CanAcceptAnother()
        {
            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);
            await listener.StartAsync();

            var noHandshakeConnection = new TcpClient();
            // This will just establish a TCP connection. It won't perform the Epoxy handshake, so
            // the listener will just be sitting there waiting for the client to send some data.
            await noHandshakeConnection.ConnectAsync(localhostEndpoint.Address, localhostEndpoint.Port);

            var connectTask = transport.ConnectToAsync(localhostAddress);
            bool didConnect = connectTask.Wait(TimeSpan.FromSeconds(10));
            Assert.IsTrue(didConnect, "Timed out waiting for connection to be established.");
        }

        [Test]
        public async Task StopAsync_ClosesAllOutstandingConnections()
        {
            EpoxyTransport transport = MakeTransport();
            var listener = transport.MakeListener(localhostEndpoint);

            await listener.StartAsync();
            var clientConnection = await transport.ConnectToAsync(localhostAddress);

            await listener.StopAsync();

            Assert.Throws<InvalidOperationException>(
                async () => await clientConnection.RequestResponseAsync<Dummy, Dummy>(
                    AnyServiceName, AnyMethodName, AnyMessage, CancellationToken.None));
        }

        private EpoxyTransport MakeTransport()
        {
            var transport = new EpoxyTransportBuilder().Construct();
            transports.Add(transport);
            return transport;
        }
    }
}
