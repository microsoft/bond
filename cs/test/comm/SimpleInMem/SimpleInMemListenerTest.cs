// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;
    using UnitTest.Interfaces;

    [TestFixture]
    public class SimpleInMemListenerTest
    {
        private readonly string address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport transport;
        private CalculatorService service;

        [SetUp]
        public void Init()
        {
            transport = new SimpleInMemTransportBuilder().Construct();
            service = new CalculatorService();
        }

        [TearDown]
        public void Cleanup()
        {
            transport.StopAsync();
        }

        [Test]
        public void CreateInvalidListener()
        {
            Assert.Throws<ArgumentNullException>(() => new SimpleInMemListener(null, null, LoggerTests.BlackHole, MetricsTests.BlackHole));
            Assert.Throws<ArgumentNullException>(() => new SimpleInMemListener(null, "SomeString", LoggerTests.BlackHole, MetricsTests.BlackHole));
            Assert.Throws<ArgumentException>(() => new SimpleInMemListener(new SimpleInMemTransportBuilder().Construct(), null, LoggerTests.BlackHole, MetricsTests.BlackHole));
        }

        [Test]
        public void CreateInMemTransportListener()
        {
            var listener = transport.MakeListener(address);
            Assert.IsNotNull(listener);
        }

        [Test]
        public async Task StartStopInMemTransportListener()
        {
            var listener = (SimpleInMemListener)transport.MakeListener(address);
            Assert.IsNotNull(listener);
            await listener.StartAsync();
            Assert.IsTrue(listener.IsStarted);
            // This should not throw exception. StartAsync calls on already started listener is a no-op.
            await listener.StartAsync();
            Assert.IsTrue(listener.IsStarted);

            await listener.StopAsync();
            Assert.IsFalse(listener.IsStarted);
            // This should not throw exception. StopAsync calls on already stopped listener is a no-op.
            await listener.StopAsync();
            Assert.IsFalse(listener.IsStarted);
        }

        [Test]
        public void AddRemoveService()
        {
            SimpleInMemListener listener = (SimpleInMemListener)transport.MakeListener(address);
            listener.AddService<CalculatorService>(service);

            foreach (var serviceMethod in service.Methods)
            {
                Assert.True(listener.IsRegistered($"{serviceMethod.MethodName}"));
            }

            Assert.False(listener.IsRegistered("Divide"));
            listener.RemoveService<CalculatorService>(service);

            foreach (var serviceMethod in service.Methods)
            {
                Assert.False(listener.IsRegistered($"{serviceMethod.MethodName}"));
            }
            Assert.False(listener.IsRegistered("Divide"));
        }

        [Test]
        public async Task ConnectedEvent_HasRightRemoteEndpointDetails()
        {
            SimpleInMemConnection listenerConnection = null;
            var connectedEventDone = new ManualResetEventSlim(initialState: false);

            SimpleInMemListener listener = (SimpleInMemListener)transport.MakeListener(address);
            listener.Connected += (sender, args) =>
            {
                Assert.AreSame(listener, sender);
                listenerConnection = (SimpleInMemConnection)args.Connection;
                connectedEventDone.Set();
            };

            await listener.StartAsync();
            var connection = (SimpleInMemConnection)await transport.ConnectToAsync(address);
            bool wasSignaled = connectedEventDone.Wait(TimeSpan.FromSeconds(30));
            Assert.IsTrue(wasSignaled, "Timed out waiting for Connected event to complete");

            Assert.AreEqual(connection.Id, listenerConnection.Id);
        }

        [Test]
        public async Task ConnectedEvent_SetsDisconnectError_ConnectToThrows()
        {
            var disconnectError = new Error { error_code = 100, message = "Go away!" };

            SimpleInMemListener listener = (SimpleInMemListener)transport.MakeListener(address);
            listener.Connected += (sender, args) =>
            {
                args.DisconnectError = disconnectError;
            };

            await listener.StartAsync();
            try
            {
                await transport.ConnectToAsync(address);
                Assert.Fail("Expected an exception to be thrown, but one wasn't.");
            }
            catch (SimpleInMemProtocolErrorException ex)
            {
                Assert.AreSame(disconnectError, ex.Details);
            }
        }
    }
}
