// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;

    [TestFixture]
    public class SimpleInMemConnectionTest
    {
        private const string m_address = "SimpleInMemTakesAnyRandomConnectionString";
        private SimpleInMemTransport m_transport;
        private CalculatorService m_service;

        [SetUp]
        public void Init()
        {
            m_transport = new SimpleInMemTransportBuilder().SetUnhandledExceptionHandler(Transport.DebugExceptionHandler).Construct();
            m_service = new CalculatorService();
        }

        [Test]
        public async Task SimpleInMemMethodCall()
        {
            const int first = 91;
            const int second = 23;
            int addResult = first + second;
            int subResult = first - second;

            SimpleInMemListener listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            listener.AddService<CalculatorService>(m_service);
            await listener.StartAsync();
            
            // Client connection
            Connection connection = await m_transport.ConnectToAsync(m_address, new System.Threading.CancellationToken());
            Assert.True(connection is SimpleInMemConnection);
            SimpleInMemConnection simpleConnection = (SimpleInMemConnection)connection;

            Assert.True(simpleConnection.ConnectionType == ConnectionType.Client);
            PairedInput input = new PairedInput();
            input.First = first;
            input.Second = second;
            Message<PairedInput> request = new Message<PairedInput>(input);
            IMessage<Output> addResponse = await simpleConnection.RequestResponseAsync<PairedInput, Output>("Add", request, System.Threading.CancellationToken.None);
            IMessage<Output> subResponse = await simpleConnection.RequestResponseAsync<PairedInput, Output>("Subtract", request, System.Threading.CancellationToken.None);

            Output addOutput = addResponse.Payload.Deserialize();
            Output subOutput = subResponse.Payload.Deserialize();
            Assert.True(addOutput.Result == addResult);
            Assert.True(subOutput.Result == subResult);
            await simpleConnection.StopAsync();
        }

        [Test]
        public async Task SimpleInMemMethodCall_WithServiceError()
        {
            const int first = 91;
            const int second = 23;

            SimpleInMemListener listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            listener.AddService<CalculatorService>(m_service);
            await listener.StartAsync();

            // Client connection
            Connection connection = await m_transport.ConnectToAsync(m_address, new System.Threading.CancellationToken());
            Assert.True(connection is SimpleInMemConnection);
            SimpleInMemConnection simpleConnection = (SimpleInMemConnection)connection;

            Assert.True(simpleConnection.ConnectionType == ConnectionType.Client);
            PairedInput input = new PairedInput();
            input.First = first;
            input.Second = second;
            Message<PairedInput> request = new Message<PairedInput>(input);
            IMessage<Output> multiplyResponse = await simpleConnection.RequestResponseAsync<PairedInput, Output>("Multiply", request, new System.Threading.CancellationToken());
            Assert.IsTrue(multiplyResponse.IsError);
            InternalServerError error = multiplyResponse.Error.Deserialize<InternalServerError>();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.That(error.message, Is.StringContaining(CalculatorService.ExpectedExceptionMessage));

            await connection.StopAsync();
        }

        [TearDown]
        public void Cleanup()
        {
            m_transport.RemoveListener(m_address);
        }
    }
}