// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System;
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
        private SimpleInMemConnection[] m_connections;

        [SetUp]
        public void Init()
        {
            m_transport = new SimpleInMemTransportBuilder().SetUnhandledExceptionHandler(Transport.DebugExceptionHandler).Construct();
            m_service = new CalculatorService();
        }

        public async Task DefaultSetup(int count)
        {
            m_listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            m_listener.AddService(m_service);
            await m_listener.StartAsync();

            m_connections = new SimpleInMemConnection[count];

            for (int connectionIndex = 0; connectionIndex < count; connectionIndex++)
            {
                m_connections[connectionIndex] = (SimpleInMemConnection)await m_transport.ConnectToAsync(m_address, System.Threading.CancellationToken.None);
            }
        }

        [Test]
        public async void ConnectionStateCycle()
        {
            await DefaultSetup(1);

            SimpleInMemConnection localConnection = 
                (SimpleInMemConnection)await m_transport.ConnectToAsync(m_address, System.Threading.CancellationToken.None);
            Assert.AreEqual(localConnection.State, CnxState.Connected);

            await localConnection.StopAsync();
            Assert.AreEqual(localConnection.State, CnxState.Disconnected);
        }

        [Test]
        public async void ConnectionStateCycle_CloseAlreadyClosedConnection()
        {
            await DefaultSetup(1);

            SimpleInMemConnection localConnection =
                (SimpleInMemConnection)await m_transport.ConnectToAsync(m_address, System.Threading.CancellationToken.None);
            Assert.AreEqual(localConnection.State, CnxState.Connected);

            // Ensure that closing an already closed connection is no-op
            for (int index = 0; index < 5; index++)
            {
                await localConnection.StopAsync();
                Assert.AreEqual(localConnection.State, CnxState.Disconnected);
            }
        }

        [Test]
        public async void ValidSetup()
        {
            await DefaultSetup(1);
            Assert.AreEqual(m_connections[0].ConnectionType, ConnectionType.Client);
        }

        [Test]
        public async Task MethodCall()
        {
            await DefaultSetup(1);

            const int first = 91;
            const int second = 23;
            int addResult = first + second;
            int subResult = first - second;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connections[0]);

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
        public async void EventCall()
        {
            await DefaultSetup(1);
            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connections[0]);

            calculatorProxy.ClearAsync();

            bool wasSignaled = CalculatorService.ClearCalledEvent.WaitOne(30000);
            Assert.IsTrue(wasSignaled, "Timed out waiting for event");
        }

        [Test]
        public async Task MultipleClientConnectionsMethodCalls()
        {
            await DefaultSetup(10);

            const int first = 91;
            const int second = 23;
            int expectedAddResult = first + second;
            int expectedSubResult = first - second;
            Task[] connectionTasks = new Task[m_connections.Length];

            for (int connectionIndex = 0; connectionIndex < m_connections.Length; connectionIndex++)
            {
                SimpleInMemConnection conn = m_connections[connectionIndex];
                connectionTasks[connectionIndex] = Task.Run(() =>
                {
                    int taskCount = 25;
                    Task[] tasks = new Task[taskCount];

                    for (int taskIndex = 0; taskIndex < taskCount; taskIndex++)
                    {
                        tasks[taskIndex] = Task.Run(async () =>
                        {
                            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(conn);

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
                            Assert.AreEqual(expectedAddResult, addOutput.Result);
                            Assert.AreEqual(expectedSubResult, subOutput.Result);
                        });
                    }

                    Task.WaitAll(tasks);
                });
            }

            Task.WaitAll(connectionTasks);
        }

        [Test]
        public async Task MethodCall_WithServiceError()
        {
            await DefaultSetup(1);

            const int first = 91;
            const int second = 23;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connections[0]);

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
        public async Task MethodCall_WithMethodNotFound()
        {
            await DefaultSetup(1);

            const int first = 91;
            const int second = 23;
            const string methodName = "Divide";

            PairedInput input = new PairedInput
            {
                First = first,
                Second = second
            };
            Message<PairedInput> request = new Message<PairedInput>(input);
            IMessage<Output> divideResponse = await m_connections[0].RequestResponseAsync<PairedInput, Output>(methodName, request, new System.Threading.CancellationToken());
            Assert.IsTrue(divideResponse.IsError);
            Error error = divideResponse.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.MethodNotFound, error.error_code);
            Assert.That(error.message, Is.StringContaining($"Got request for unknown method [{methodName}]."));
        }

        [TearDown]
        public async void Cleanup()
        {
            if (m_connections != null)
            {
                for (int connectionIndex = 0; connectionIndex < m_connections.Length; connectionIndex++)
                {
                    await m_connections[connectionIndex].StopAsync();
                }
            }
            m_connections = null;
            m_transport.RemoveListener(m_address);
        }
    }
}