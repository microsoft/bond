// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;

    [TestFixture]
    public class SimpleInMemListenerTest
    {
        private readonly string m_address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport m_transport;
        private CalculatorServiceImpl m_service;

        [SetUp]
        public void Init()
        {
            m_transport =
                new SimpleInMemTransportBuilder()
                .SetUnhandledExceptionHandler(Transport.ToErrorExceptionHandler)
                .Construct();
             m_service = new CalculatorServiceImpl();
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
            var transport = new SimpleInMemTransportBuilder()
                .SetUnhandledExceptionHandler(Transport.ToErrorExceptionHandler)
                .Construct();
            var listener = transport.MakeListener(m_address);
            Assert.IsNotNull(listener);
            await listener.StartAsync();
            await listener.StopAsync();
        }

        [Test]
        public void AddRemoveService()
        {
            var transport = new SimpleInMemTransportBuilder()
                .SetUnhandledExceptionHandler(Transport.ToErrorExceptionHandler)
                .Construct();
            SimpleInMemListener listener = (SimpleInMemListener)transport.MakeListener(m_address);
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
    }
}
