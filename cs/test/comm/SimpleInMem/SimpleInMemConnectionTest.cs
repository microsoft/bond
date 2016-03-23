// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using Bond.Comm;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;
    using System.Threading.Tasks;
    
    [TestFixture]
    public class SimpleInMemConnectionTest
    {
        private const string m_address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport m_transport;
        private CalculatorService m_service;

        [SetUp]
        public void Init()
        {
            m_transport = new SimpleInMemTransportBuilder().Construct();
            m_service = new CalculatorService();
        }

        [Test]
        public async Task SimpleInMemMethodCall()
        {
            int first = 91;
            int second = 23;
            int addResult = first + second;
            int subResult = first - second;

            SimpleInMemListener listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            listener.AddService<CalculatorService>(m_service);
            await listener.StartAsync();
            
            //Client connection
            Connection connection = await m_transport.ConnectToAsync(m_address, new System.Threading.CancellationToken());
            Assert.True(connection is SimpleInMemConnection);
            SimpleInMemConnection simpleConnection = (SimpleInMemConnection)connection;

            Assert.True(simpleConnection.ConnectionType == ConnectionType.Client);
            PairedInput input = new PairedInput();
            input.First = first;
            input.Second = second;
            Message<PairedInput> request = new Message<PairedInput>(input);
            Message<Output> addResponse = await simpleConnection.RequestResponseAsync<PairedInput, Output>("Add", request, System.Threading.CancellationToken.None);
            Message<Output> subResponse = await simpleConnection.RequestResponseAsync<PairedInput, Output>("Subtract", request, System.Threading.CancellationToken.None);

            Output addOutput = addResponse.Payload.Deserialize();
            Output subOutput = subResponse.Payload.Deserialize();
            Assert.True(addOutput.Result == addResult);
            Assert.True(subOutput.Result == subResult);
            await simpleConnection.StopAsync();
        }

        //[Test] TODO implement error handling delegate in SimpleInMemTransport
        public async Task SimpleInMemMethodCall_WithServiceError()
        {
            int first = 91;
            int second = 23;
            int multiplyResult = first * second;

            SimpleInMemListener listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            listener.AddService<CalculatorService>(m_service);
            await listener.StartAsync();

            //Client connection
            Connection connection = await m_transport.ConnectToAsync(m_address, new System.Threading.CancellationToken());
            Assert.True(connection is SimpleInMemConnection);
            SimpleInMemConnection simpleConnection = (SimpleInMemConnection)connection;

            Assert.True(simpleConnection.ConnectionType == ConnectionType.Client);
            PairedInput input = new PairedInput();
            input.First = first;
            input.Second = second;
            Message<PairedInput> request = new Message<PairedInput>(input);
            Message<Output> multiplyResponse = await simpleConnection.RequestResponseAsync<PairedInput, Output>("Multiply", request, new System.Threading.CancellationToken());
            //TODO Assert error returned by the service
        }

        [TearDown]
        public void Cleanup()
        {
            m_transport.RemoveListener(m_address);
        }
    }
}