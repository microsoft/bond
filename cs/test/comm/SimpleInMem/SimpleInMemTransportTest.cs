// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;
    using UnitTest.Comm;

    [TestFixture]
    public class SimpleInMemTransportTest
    {
        private readonly string address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport transport;

        [SetUp]
        public void Init()
        {
            transport = new SimpleInMemTransportBuilder().Construct();
        }

        [TearDown]
        public void Cleanup()
        {
            transport.StopAsync();
            transport = null;
        }

        [Test]
        public void Builder_Construct_NoArgs_Succeeds()
        {
            var builder = new SimpleInMemTransportBuilder();
            Assert.NotNull(builder.Construct());
        }

        [Test]
        public void StopAsync()
        {
            Listener newListener = transport.MakeListener(address);
            Assert.True(newListener == transport.GetListener(address));
            transport.StopAsync();
            Assert.False(transport.ListenerExists(address));
            transport.MakeListener(address);
            Assert.True(transport.ListenerExists(address));
        }

        [Test]
        public void ConnectToAsync_NoListenerRunning()
        {
            Assert.Throws<ArgumentException>(async () => await transport.ConnectToAsync(address, new System.Threading.CancellationToken()));
        }

        [Test]
        public void MakeListener()
        {
            bool listenerExists = transport.ListenerExists(address);
            Assert.False(listenerExists);
            transport.MakeListener(address);
            listenerExists = transport.ListenerExists(address);
            Assert.True(listenerExists);
            transport.StopAsync();
            Assert.Null(transport.GetListener(address));
        }

        [Test]
        public async void ConnectToAsync()
        {
            transport.MakeListener(address);
            Connection conn = await transport.ConnectToAsync(address, new System.Threading.CancellationToken());
            Assert.NotNull(conn);
            Assert.True(conn is SimpleInMemConnection);
            SimpleInMemConnection simpleConn = (SimpleInMemConnection)conn;
            Assert.True(simpleConn.ConnectionType == ConnectionType.Client);
            await transport.StopAsync();
            Assert.Null(transport.GetListener(address));
        }
    }
}
