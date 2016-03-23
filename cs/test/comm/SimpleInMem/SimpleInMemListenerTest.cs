// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;
    using System.Threading.Tasks;

    [TestFixture]
    public class SimpleInMemListenerTest
    {
        private readonly string m_address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport m_transport;

        [SetUp]
        public void Init()
        {
            m_transport = new SimpleInMemTransportBuilder().Construct();
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
            var transport = new SimpleInMemTransportBuilder().Construct();
            var listener = transport.MakeListener(m_address);
            Assert.IsNotNull(listener);
            await listener.StartAsync();
            await listener.StopAsync();
        }
    }
}
