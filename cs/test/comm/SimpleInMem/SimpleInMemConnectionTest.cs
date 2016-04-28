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
        private SimpleInMemListener m_listener;
        private SimpleInMemConnection m_connection;

        [SetUp]
        public void Init()
        {
            m_transport = new SimpleInMemTransportBuilder().SetUnhandledExceptionHandler(Transport.DebugExceptionHandler).Construct();
            m_service = new CalculatorService();
        }

        public async Task DefaultSetup()
        {
            m_listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            m_listener.AddService(m_service);
            await m_listener.StartAsync();

            // Client connection 
            m_connection = (SimpleInMemConnection)await m_transport.ConnectToAsync(m_address, System.Threading.CancellationToken.None);
        }

        [Test]
        public async void SimpleInMemValidSetup()
        {
            await DefaultSetup();
            Assert.AreEqual(m_connection.ConnectionType, ConnectionType.Client);
        }

        [Test]
        public async Task SimpleInMemMethodCall()
        {
            await DefaultSetup();

            const int first = 91;
            const int second = 23;
            int addResult = first + second;
            int subResult = first - second;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connection);

            PairedInput input = new PairedInput
            {
                First = first,
                Second = second
            };
            Message<PairedInput> request = new Message<PairedInput>(input);
            IMessage<Output> addResponse = await calculatorProxy.AddAsync(request, System.Threading.CancellationToken.None);
            IMessage<Output> subResponse = await calculatorProxy.SubtractAsync(request, System.Threading.CancellationToken.None);
            Output addOutput = addResponse.Payload.Deserialize();
            Output subOutput = subResponse.Payload.Deserialize();
            Assert.True(addOutput.Result == addResult);
            Assert.True(subOutput.Result == subResult);
        }

        [Test]
        public async void SimpleInMemEventCall()
        {
            await DefaultSetup();
            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connection);

            calculatorProxy.ClearAsync();

            bool wasSignaled = CalculatorService.ClearCalledEvent.WaitOne(30000);
            Assert.IsTrue(wasSignaled, "Timed out waiting for event");
        }

        [Test]
        public async Task SimpleInMemMethodCall_WithServiceError()
        {
            await DefaultSetup();

            const int first = 91;
            const int second = 23;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connection);

            PairedInput input = new PairedInput
            {
                First = first,
                Second = second
            };
            Message<PairedInput> request = new Message<PairedInput>(input);
            IMessage<Output> multiplyResponse = await calculatorProxy.MultiplyAsync(request, System.Threading.CancellationToken.None);
            Assert.IsTrue(multiplyResponse.IsError);
            InternalServerError error = multiplyResponse.Error.Deserialize<InternalServerError>();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.That(error.message, Is.StringContaining(CalculatorService.ExpectedExceptionMessage));
        }

        [Test]
        public async Task SimpleInMemMethodCall_WithMethodNotFound()
        {
            await DefaultSetup();

            const int first = 91;
            const int second = 23;
            const string methodName = "Divide";

            PairedInput input = new PairedInput
            {
                First = first,
                Second = second
            };
            Message<PairedInput> request = new Message<PairedInput>(input);
            IMessage<Output> divideResponse = await m_connection.RequestResponseAsync<PairedInput, Output>(methodName, request, new System.Threading.CancellationToken());
            Assert.IsTrue(divideResponse.IsError);
            Error error = divideResponse.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.MethodNotFound, error.error_code);
            Assert.That(error.message, Is.StringContaining($"ServiceHost.DispatchRequest: Got request for unknown method {methodName}."));
        }

        [TearDown]
        public async void Cleanup()
        {
            await m_connection.StopAsync();
            m_transport.RemoveListener(m_address);
        }
    }
}