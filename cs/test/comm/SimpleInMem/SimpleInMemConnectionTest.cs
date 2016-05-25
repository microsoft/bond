// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Layers;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;
    using UnitTest.Comm;
    using UnitTest.Layers;

    [TestFixture]
    public class SimpleInMemConnectionTest
    {
        private const string m_address = "SimpleInMemTakesAnyRandomConnectionString";
        private TransportBuilder<SimpleInMemTransport> m_transportBuilder;
        private SimpleInMemTransport m_transport;
        private SimpleInMemListener m_listener;
        private SimpleInMemConnection[] m_connections;

        [SetUp]
        public void Init()
        {
            m_transportBuilder = new SimpleInMemTransportBuilder();
        }

        public async Task DefaultSetup(IService service, int count)
        {
            m_transport = m_transportBuilder.Construct();
            m_listener = (SimpleInMemListener)m_transport.MakeListener(m_address);
            m_listener.AddService(service);
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
            await DefaultSetup(new CalculatorService(), 1);

            SimpleInMemConnection localConnection = 
                (SimpleInMemConnection)await m_transport.ConnectToAsync(m_address, System.Threading.CancellationToken.None);
            Assert.AreEqual(localConnection.State, CnxState.Connected);

            await localConnection.StopAsync();
            Assert.AreEqual(localConnection.State, CnxState.Disconnected);
        }

        [Test]
        public async void ConnectionStateCycle_CloseAlreadyClosedConnection()
        {
            await DefaultSetup(new CalculatorService(), 1);

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
            await DefaultSetup(new CalculatorService(), 1);
            Assert.AreEqual(m_connections[0].ConnectionType, ConnectionType.Client);
        }

        [Test]
        public async Task MethodCall()
        {
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;
            int addResult = first + second;
            int subResult = first - second;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connections[0]);

            var input = new PairedInput
            {
                First = first,
                Second = second
            };
            var request = new Message<PairedInput>(input);
            IMessage<Output> addResponse = await calculatorProxy.AddAsync(request, System.Threading.CancellationToken.None);
            IMessage<Output> subResponse = await calculatorProxy.SubtractAsync(request, System.Threading.CancellationToken.None);
            Output addOutput = addResponse.Payload.Deserialize();
            Output subOutput = subResponse.Payload.Deserialize();
            Assert.AreEqual(addResult, addOutput.Result);
            Assert.AreEqual(subResult, subOutput.Result);
        }

        [Test]
        public async void EventCall()
        {
            await DefaultSetup(new CalculatorService(), 1);
            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connections[0]);

            calculatorProxy.ClearAsync();

            bool wasSignaled = CalculatorService.ClearCalledEvent.WaitOne(30000);
            Assert.IsTrue(wasSignaled, "Timed out waiting for event");
        }

        [Test]
        public async Task MultipleClientConnectionsMethodCalls()
        {
            Stopwatch sw = Stopwatch.StartNew();

            await DefaultSetup(new CalculatorService(), 10);

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
                            var input = new PairedInput
                            {
                                First = first,
                                Second = second
                            };

                            var request = new Message<PairedInput>(input);
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
            sw.Stop();
            Console.WriteLine($"{nameof(MultipleClientConnectionsMethodCalls)} test time: {sw.Elapsed.TotalSeconds}");
        }

        [Test]
        public async Task MethodCall_WithServiceError()
        {
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connections[0]);

            var input = new PairedInput
            {
                First = first,
                Second = second
            };
            var request = new Message<PairedInput>(input);
            IMessage<Output> multiplyResponse = await calculatorProxy.MultiplyAsync(request, System.Threading.CancellationToken.None);
            Assert.IsTrue(multiplyResponse.IsError);
            InternalServerError error = multiplyResponse.Error.Deserialize<InternalServerError>();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.That(error.message, Is.StringContaining(Transport.InternalErrorMessage));
        }

        [Test]
        public async Task MethodCall_WithMethodNotFound()
        {
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;
            const string methodName = "Divide";

            var input = new PairedInput
            {
                First = first,
                Second = second
            };
            var request = new Message<PairedInput>(input);
            IMessage<Output> divideResponse = await m_connections[0].RequestResponseAsync<PairedInput, Output>(methodName, request, new System.Threading.CancellationToken());
            Assert.IsTrue(divideResponse.IsError);
            Error error = divideResponse.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.MethodNotFound, error.error_code);
            Assert.That(error.message, Is.StringContaining($"Got request for unknown method [{methodName}]."));
        }

        [Test]
        public async Task MethodCall_WithLayerStack()
        {
            var testList = new List<string>();
            var layer1 = new TestLayer_Append("foo", testList);
            var layer2 = new TestLayer_Append("bar", testList);

            m_transportBuilder.SetLayerStack(new LayerStack<Dummy>(layer1, layer2));
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;
            int addResult = first + second;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(m_connections[0]);

            var input = new PairedInput
            {
                First = first,
                Second = second
            };
            var request = new Message<PairedInput>(input);
            IMessage<Output> addResponse = await calculatorProxy.AddAsync(request, System.Threading.CancellationToken.None);
            Output addOutput = addResponse.Payload.Deserialize();
            Assert.AreEqual(addResult, addOutput.Result);

            Assert.AreEqual(8, testList.Count);
            Assert.AreEqual(layer1.value, testList[0]);
            Assert.AreEqual(testList[0] + layer2.value, testList[1]);
            Assert.AreEqual(testList[1] + layer2.value, testList[2]);
            Assert.AreEqual(testList[2] + layer1.value, testList[3]);
            Assert.AreEqual(layer1.value, testList[4]);
            Assert.AreEqual(testList[4] + layer2.value, testList[5]);
            Assert.AreEqual(testList[5] + layer2.value, testList[6]);
            Assert.AreEqual(testList[6] + layer1.value, testList[7]);
        }

        [Test]
        public async Task MethodCall_ReqRsp_WithLayerStackErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            m_transportBuilder.SetLayerStack(new LayerStack<Dummy>(errorLayer));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(m_connections[0]);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Request, errorOnSend: false, errorOnReceive: true);
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, response.Error.Deserialize().error_code, "Bad error 1");

            Assert.AreEqual(0, testService.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.Request, errorOnSend: true, errorOnReceive: false);
            request.int_value = 101;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, response.Error.Deserialize().error_code);

            Assert.AreEqual(0, testService.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.Response, errorOnSend: true, errorOnReceive: false);
            request.int_value = 102;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, response.Error.Deserialize().error_code);

            Assert.AreEqual(1, testService.RequestCount);
            Assert.AreEqual(request.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.Response, errorOnSend: false, errorOnReceive: true);
            request.int_value = 103;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, response.Error.Deserialize().error_code);

            Assert.AreEqual(2, testService.RequestCount);
            Assert.AreEqual(request.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.Event, errorOnSend: true, errorOnReceive: true);
            request.int_value = 104;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(105, response.Payload.Deserialize().int_value);

            Assert.AreEqual(3, testService.RequestCount);
            Assert.AreEqual(request.int_value, testService.LastRequestReceived.int_value);
        }

        [Test]
        public async Task MethodCall_Event_WithLayerStackErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            m_transportBuilder.SetLayerStack(new LayerStack<Dummy>(errorLayer));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(m_connections[0]);
            var theEvent = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Event, errorOnSend: false, errorOnReceive: true);

            ManualResetEventSlim waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire 1");
            testService.ClearResetEvent();

            Assert.AreEqual(0, testService.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.Event, errorOnSend: true, errorOnReceive: false);
            theEvent.int_value = 101;

            waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire 2");
            testService.ClearResetEvent();

            Assert.AreEqual(0, testService.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.Event, errorOnSend: false, errorOnReceive: false);
            theEvent.int_value = 102;

            waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testService.EventCount);
            Assert.AreEqual(theEvent.int_value, testService.LastEventReceived.int_value);
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