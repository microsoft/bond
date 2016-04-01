// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using Bond.Comm;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;
    using System;
    using System.Threading.Tasks;

    [TestFixture]
    public class SimpleInMemTransportTest
    {
        private readonly string m_address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport m_transport;

        [SetUp]
        public void Init()
        {
            m_transport = new SimpleInMemTransportBuilder()
                .SetUnhandledExceptionHandler(Transport.ToErrorExceptionHandler)
                .Construct();
        }

        [TearDown]
        public void Cleanup()
        {
            m_transport = null;
        }

        [Test]
        public void Builder_SetUnhandledExceptionHandler_Null_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new SimpleInMemTransportBuilder().SetUnhandledExceptionHandler(null));
        }

        [Test]
        public void Builder_Construct_DidntSetUnhandledExceptionHandler_Throws()
        {
            var builder = new SimpleInMemTransportBuilder();
            Assert.Throws<InvalidOperationException>(() => builder.Construct());
        }

        [Test]
        public void Construct_InvalidArgs_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new SimpleInMemTransport(null));
        }


        [ExpectedException(typeof(NotImplementedException))]
        public void StopAsync_NotImplemented()
        {
            m_transport.StopAsync();
        }

        [ExpectedException(typeof(InMemTransportListenerException))]
        public async Task ConnectToAsync_NoListenerRunning()
        {
            await m_transport.ConnectToAsync(m_address, new System.Threading.CancellationToken());
        }

        [Test]
        public void MakeListener()
        {
            bool listenerExists = m_transport.ListenerExists(m_address);
            Assert.False(listenerExists);
            var listener = m_transport.MakeListener(m_address);
            listenerExists = m_transport.ListenerExists(m_address);
            Assert.True(listenerExists);
            m_transport.RemoveListener(m_address);
            Assert.Null(m_transport.GetListener(m_address));
        }

        [Test]
        public async Task ConnectToAsync()
        {
            Listener l = m_transport.MakeListener(m_address);
            Connection conn = await m_transport.ConnectToAsync(m_address, new System.Threading.CancellationToken());
            Assert.NotNull(conn);
            Assert.True(conn is SimpleInMemConnection);
            SimpleInMemConnection simpleConn = (SimpleInMemConnection)conn;
            Assert.True(simpleConn.ConnectionType == ConnectionType.Client);
            m_transport.RemoveListener(m_address);
            Assert.Null(m_transport.GetListener(m_address));
        }
    }
}