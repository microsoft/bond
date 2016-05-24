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

    [TestFixture]
    public class SimpleInMemListenerTest
    {
        private readonly string m_address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport m_transport;
        private CalculatorService m_service;

        [SetUp]
        public void Init()
        {
            m_transport = new SimpleInMemTransportBuilder().Construct();
            m_service = new CalculatorService();
        }

        [TearDown]
        public void Cleanup()
        {
            m_transport.RemoveListener(m_address);
        }

        [Test]
        public void CreateInMemTransportListener()
        {
            var listener = m_transport.MakeListener(m_address);
            Assert.IsNotNull(listener);
        }

        [Test]
        public async Task StartStopInMemTransportListener()
        {
            var listener = m_transport.MakeListener(m_address);
            Assert.IsNotNull(listener);
            await listener.StartAsync();
            await listener.StopAsync();
        }

        [Test]
        public void AddRemoveService()
        {
            SimpleInMemListener listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            listener.AddService<CalculatorService>(m_service);

            foreach (var serviceMethod in m_service.Methods)
            {
                Assert.True(listener.IsRegistered($"{serviceMethod.MethodName}"));
            }

            Assert.False(listener.IsRegistered("Divide"));
            listener.RemoveService<CalculatorService>(m_service);

            foreach (var serviceMethod in m_service.Methods)
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

            SimpleInMemListener listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            listener.Connected += (sender, args) =>
            {
                Assert.AreSame(listener, sender);
                listenerConnection = (SimpleInMemConnection)args.Connection;
                connectedEventDone.Set();
            };

            await listener.StartAsync();
            var connection = (SimpleInMemConnection)await m_transport.ConnectToAsync(m_address);
            bool wasSignaled = connectedEventDone.Wait(TimeSpan.FromSeconds(30));
            Assert.IsTrue(wasSignaled, "Timed out waiting for Connected event to complete");

            Assert.AreEqual(connection.Id, listenerConnection.Id);
        }

        [Test]
        public async Task ConnectedEvent_SetsDisconnectError_ConnectToThrows()
        {
            var disconnectError = new Error { error_code = 100, message = "Go away!" };

            SimpleInMemListener listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            listener.Connected += (sender, args) =>
            {
                args.DisconnectError = disconnectError;
            };

            await listener.StartAsync();
            try
            {
                await m_transport.ConnectToAsync(m_address);
                Assert.Fail("Expected an exception to be thrown, but one wasn't.");
            }
            catch (SimpleInMemProtocolErrorException ex)
            {
                Assert.AreSame(disconnectError, ex.Details);
            }
        }
    }
}
