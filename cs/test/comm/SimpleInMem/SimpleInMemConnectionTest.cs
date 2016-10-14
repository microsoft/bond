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
    using UnitTest.Interfaces;
    using UnitTest.Layers;

    [TestFixture]
    public class SimpleInMemConnectionTest
    {
        private const string address = "SimpleInMemTakesAnyRandomConnectionString";
        private TransportBuilder<SimpleInMemTransport> transportBuilder;
        private SimpleInMemTransport transport;
        private SimpleInMemListener listener;
        private SimpleInMemConnection[] connections;

        [SetUp]
        public void Init()
        {
            transportBuilder = new SimpleInMemTransportBuilder();
        }

        public async Task DefaultSetup(IService service, int count)
        {
            transport = transportBuilder.Construct();
            listener = (SimpleInMemListener)transport.MakeListener(address);
            listener.AddService(service);
            await listener.StartAsync();

            connections = new SimpleInMemConnection[count];

            for (int connectionIndex = 0; connectionIndex < count; connectionIndex++)
            {
                connections[connectionIndex] = (SimpleInMemConnection)await transport.ConnectToAsync(address, System.Threading.CancellationToken.None);
                Assert.IsTrue(connections[connectionIndex].IsConnected);
                Assert.IsTrue(connections[connectionIndex].IsPaired);
            }
        }

        [Test]
        public async void TestWithServerAndClientConnections()
        {
            await DefaultSetup(new CalculatorService(), 1);
            IEnumerator<Guid> pairIds = listener.GetPairIds().GetEnumerator();
            pairIds.MoveNext();
            Guid firstPair = pairIds.Current;
            SimpleInMemConnection serverConnection = listener.GetConnection(firstPair, ConnectionType.Server);
            SimpleInMemConnection clientConnection = listener.GetConnection(firstPair, ConnectionType.Client);

            const int first = 91;
            const int second = 23;
            int addResult = first + second;
            int subResult = first - second;

            var serverProxy = new CalculatorProxy<SimpleInMemConnection>(serverConnection);
            var clientProxy = new CalculatorProxy<SimpleInMemConnection>(clientConnection);


            var input = new PairedInput
            {
                First = first,
                Second = second
            };
            var request = new Message<PairedInput>(input);
            IMessage<Output> addResponse = await clientProxy.AddAsync(request, System.Threading.CancellationToken.None);
            IMessage<Output> subResponse = await clientProxy.SubtractAsync(request, System.Threading.CancellationToken.None);
            Assert.IsFalse(addResponse.IsError);
            Assert.IsFalse(subResponse.IsError);
            Output addOutput = addResponse.Payload.Deserialize();
            Output subOutput = subResponse.Payload.Deserialize();
            Assert.AreEqual(addResult, addOutput.Result);
            Assert.AreEqual(subResult, subOutput.Result);

            addResponse = await serverProxy.AddAsync(request, System.Threading.CancellationToken.None);
            subResponse = await serverProxy.SubtractAsync(request, System.Threading.CancellationToken.None);
            Assert.IsTrue(addResponse.IsError);
            Assert.IsTrue(subResponse.IsError);
            Error addError = addResponse.Error.Deserialize();
            Error subError = subResponse.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.METHOD_NOT_FOUND, (int)addError.error_code);
            Assert.AreEqual("Got request for unknown method [unittest.simpleinmem.Calculator.Add].", addError.message);
            Assert.AreEqual((int)ErrorCode.METHOD_NOT_FOUND, (int)subError.error_code);
            Assert.AreEqual("Got request for unknown method [unittest.simpleinmem.Calculator.Subtract].", subError.message);
        }

        [Test]
        public async void ConnectionStateCycle()
        {
            await DefaultSetup(new CalculatorService(), 1);

            SimpleInMemConnection localConnection = 
                (SimpleInMemConnection)await transport.ConnectToAsync(address, System.Threading.CancellationToken.None);
            Assert.AreEqual(localConnection.State, CnxState.Connected);

            await localConnection.StopAsync();
            Assert.AreEqual(localConnection.State, CnxState.Disconnected);
        }

        [Test]
        public async void ConnectionStateCycle_CloseAlreadyClosedConnection()
        {
            await DefaultSetup(new CalculatorService(), 1);

            SimpleInMemConnection localConnection =
                (SimpleInMemConnection)await transport.ConnectToAsync(address, System.Threading.CancellationToken.None);
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
            Assert.AreEqual(connections[0].ConnectionType, ConnectionType.Client);
        }

        [Test]
        public async Task MethodCall()
        {
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;
            int addResult = first + second;
            int subResult = first - second;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(connections[0]);

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
            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(connections[0]);

            calculatorProxy.ClearAsync();

            bool wasSignaled = CalculatorService.ClearCalledEvent.WaitOne(30000);
            Assert.IsTrue(wasSignaled, "Timed out waiting for event");
        }

        [Test]
        public async Task MultipleClientConnectionsEventCalls()
        {
            await DefaultSetup(new CalculatorService(), 10);

            Task[] connectionTasks = new Task[connections.Length];
            const int taskCount = 25;

            for (int connectionIndex = 0; connectionIndex < connections.Length; connectionIndex++)
            {
                SimpleInMemConnection conn = connections[connectionIndex];
                connectionTasks[connectionIndex] = Task.Run(() =>
                {
                    Task[] tasks = new Task[taskCount];

                    for (int taskIndex = 0; taskIndex < taskCount; taskIndex++)
                    {
                        tasks[taskIndex] = Task.Run(() =>
                        {
                            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(conn);
                            calculatorProxy.IncrementCountAsync();
                        });
                    }

                    Task.WaitAll(tasks);
                });
            }

            Task.WaitAll(connectionTasks);

            int totalWaitTime = 0;
            const int maxWait = 5000;
            // Intentionally avoiding exponential back-off due to simple nature of this test
            const int incrementalWait = 500;

            while (totalWaitTime < maxWait)
            {
                await Task.Delay(incrementalWait);
                totalWaitTime += incrementalWait;

                try
                {
                    Assert.AreEqual(CalculatorService.Count, connections.Length * taskCount);
                    break;
                }
                catch (AssertionException)
                {
                    // The implementation of SimpleInMem can guarantee delivery of events just be virtue of staying with in the process boundary.
                    // SimpleInMem event failing after 5 seconds needs to raise alarm and investigation.
                    if (totalWaitTime > maxWait)
                    {
                        throw;
                    }
                }
            }

            Console.WriteLine($"{nameof(MultipleClientConnectionsEventCalls)} - Count: {CalculatorService.Count}");
        }

        [Test]
        public async Task MultipleClientConnectionsMethodCalls()
        {
            Stopwatch sw = Stopwatch.StartNew();

            await DefaultSetup(new CalculatorService(), 10);

            Task[] connectionTasks = new Task[connections.Length];

            for (int connectionIndex = 0; connectionIndex < connections.Length; connectionIndex++)
            {
                SimpleInMemConnection conn = connections[connectionIndex];
                connectionTasks[connectionIndex] = Task.Run(() =>
                {
                    int taskCount = 25;
                    Task[] tasks = new Task[taskCount];

                    for (int taskIndex = 0; taskIndex < taskCount; taskIndex++)
                    {
                        tasks[taskIndex] = Task.Run(async () =>
                        {
                            Random rand = new Random(DateTime.UtcNow.Millisecond);
                            int first = rand.Next(1, 100);
                            int second = rand.Next(1, 50);
                            int expectedAddResult = first + second;
                            int expectedSubResult = first - second;
                            var addTraceId = Guid.NewGuid().ToString();
                            var subTraceId = Guid.NewGuid().ToString();
                            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(conn);

                            var addInput = new PairedInput
                            {
                                First = first,
                                Second = second,
                                TraceId = addTraceId
                            };

                            var subInput = new PairedInput
                            {
                                First = first,
                                Second = second,
                                TraceId = subTraceId
                            };

                            Message<PairedInput> addRequest = new Message<PairedInput>(addInput);
                            Message<PairedInput> subRequest = new Message<PairedInput>(subInput);
                            IMessage<Output> addResponse = await calculatorProxy.AddAsync(addRequest, System.Threading.CancellationToken.None);
                            IMessage<Output> subResponse = await calculatorProxy.SubtractAsync(subRequest, System.Threading.CancellationToken.None);
                            Output addOutput = addResponse.Payload.Deserialize();
                            Output subOutput = subResponse.Payload.Deserialize();

                            Assert.AreEqual(expectedAddResult, addOutput.Result);
                            Assert.AreEqual(addInput.TraceId, addOutput.TraceId);
                            Assert.AreEqual(expectedSubResult, subOutput.Result);
                            Assert.AreEqual(subInput.TraceId, subOutput.TraceId);
                        });
                    }

                    Task.WaitAll(tasks);
                });
            }

            Task.WaitAll(connectionTasks);
            sw.Stop();
            Console.WriteLine($"{nameof(MultipleClientConnectionsMethodCalls)} - test time: {sw.Elapsed.TotalSeconds}");
        }

        [Test]
        public async Task MethodCall_WithServiceError()
        {
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(connections[0]);

            var input = new PairedInput
            {
                First = first,
                Second = second
            };
            var request = new Message<PairedInput>(input);
            IMessage<Output> multiplyResponse = await calculatorProxy.MultiplyAsync(request, System.Threading.CancellationToken.None);
            Assert.IsTrue(multiplyResponse.IsError);
            InternalServerError error = multiplyResponse.Error.Deserialize<InternalServerError>();
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
            Assert.That(error.message, Is.StringContaining(Errors.InternalErrorMessage));
        }

        [Test]
        public async Task MethodCall_WithMethodNotFound()
        {
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;
            const string serviceName = "Calculator";
            const string methodName = "Divide";

            var input = new PairedInput
            {
                First = first,
                Second = second
            };
            var request = new Message<PairedInput>(input);
            IMessage<Output> divideResponse = await connections[0].RequestResponseAsync<PairedInput, Output>(serviceName, methodName, request, new System.Threading.CancellationToken());
            Assert.IsTrue(divideResponse.IsError);
            Error error = divideResponse.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.METHOD_NOT_FOUND, error.error_code);
            Assert.That(error.message, Is.StringContaining($"Got request for unknown method [{serviceName}.{methodName}]."));
        }

        [Test]
        public async Task MethodCall_WithLayerStack()
        {
            var testList = new List<string>();
            var layer1 = new TestLayer_Append("foo", testList);
            var layer2 = new TestLayer_Append("bar", testList);

            transportBuilder.SetLayerStackProvider(new LayerStackProvider<Dummy>(layer1, layer2));
            await DefaultSetup(new CalculatorService(), 1);

            const int first = 91;
            const int second = 23;
            int addResult = first + second;

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(connections[0]);

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
        public async Task MethodCall_ReqRsp_WithStatefulLayers()
        {
            var layerProvider = new TestLayerProvider_StatefulAppend("Layer");
            var layerStackProvider = new LayerStackProvider<Dummy>(layerProvider);
            transportBuilder.SetLayerStackProvider(layerStackProvider);
            await DefaultSetup(new CalculatorService(), 1);

            layerProvider.Layers.Clear();

            var calculatorProxy = new CalculatorProxy<SimpleInMemConnection>(connections[0]);

            var request = new Message<PairedInput>(new PairedInput { First = 1, Second = 2 });
            IMessage<Output> response = await calculatorProxy.AddAsync(request, CancellationToken.None);
            Assert.IsFalse(response.IsError);

            Assert.AreEqual(2, layerProvider.Layers.Count);
            Assert.AreEqual("Layer0SendLayer0Receive", layerProvider.Layers[0].State);
            Assert.AreEqual("Layer1ReceiveLayer1Send", layerProvider.Layers[1].State);

            request = new Message<PairedInput>(new PairedInput { First = 1, Second = 2 });
            response = await calculatorProxy.AddAsync(request, CancellationToken.None);
            Assert.IsFalse(response.IsError);

            Assert.AreEqual(4, layerProvider.Layers.Count);
            Assert.AreEqual("Layer2SendLayer2Receive", layerProvider.Layers[2].State);
            Assert.AreEqual("Layer3ReceiveLayer3Send", layerProvider.Layers[3].State);
        }

        [Test]
        public async Task MethodCall_ReqRsp_WithLayerStackErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            transportBuilder.SetLayerStackProvider(new LayerStackProvider<Dummy>(errorLayer));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(connections[0]);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.REQUEST, errorOnSend: false, errorOnReceive: true);
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, error.error_code, "Error 1 does not match");

            Assert.AreEqual(0, testService.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.REQUEST, errorOnSend: true, errorOnReceive: false);
            request.int_value = 101;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, error.error_code, "Error 2 does not match");

            Assert.AreEqual(0, testService.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.RESPONSE, errorOnSend: true, errorOnReceive: false);
            request.int_value = 102;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, error.error_code, "Error 3 does not match");

            Assert.AreEqual(1, testService.RequestCount);
            Assert.AreEqual(request.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.RESPONSE, errorOnSend: false, errorOnReceive: true);
            request.int_value = 103;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsTrue(response.IsError);
            error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, error.error_code, "Error 4 does not match");

            Assert.AreEqual(2, testService.RequestCount);
            Assert.AreEqual(request.int_value, testService.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.EVENT, errorOnSend: true, errorOnReceive: true);
            request.int_value = 104;
            response = await proxy.ReqRspMethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(105, response.Payload.Deserialize().int_value);

            Assert.AreEqual(3, testService.RequestCount);
            Assert.AreEqual(request.int_value, testService.LastRequestReceived.int_value);
        }

        [Test]
        public async Task MethodCall_ReqRsp_FailingLayerStackProvider_ClientSendReq()
        {
            // Fail after 2 successful GetLayerStack calls (1 on client, 1 on server)
            transportBuilder.SetLayerStackProvider(new TestLayerStackProvider_Fails(2));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(connections[0]);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            request.int_value = 101;
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
            Assert.AreEqual(TestLayerStackProvider_Fails.InternalDetails, error.message);
        }

        [Test]
        public async Task MethodCall_ReqRsp_FailingLayerStackProvider_ServerReceiveReq()
        {
            // Fail after 3 successful GetLayerStack calls (2 on client, 1 on server)
            transportBuilder.SetLayerStackProvider(new TestLayerStackProvider_Fails(3));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(connections[0]);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            request.int_value = 101;
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
            Assert.AreEqual(Errors.InternalErrorMessage, error.message);
        }

        [Test]
        public async Task MethodCall_Event_With_StatefulLayers()
        {
            var layerProvider = new TestLayerProvider_StatefulAppend("Layer");
            var layerStackProvider = new LayerStackProvider<Dummy>(layerProvider);
            transportBuilder.SetLayerStackProvider(layerStackProvider);
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(connections[0]);
            var theEvent = new Dummy { int_value = 100 };

            layerProvider.Layers.Clear();

            ManualResetEventSlim waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");
            testService.ClearResetEvent();

            Assert.AreEqual(1, testService.EventCount);
            Assert.AreEqual(theEvent.int_value, testService.LastEventReceived.int_value);

            Assert.AreEqual(2, layerProvider.Layers.Count);
            Assert.AreEqual("Layer0Send", layerProvider.Layers[0].State);
            Assert.AreEqual("Layer1Receive", layerProvider.Layers[1].State);
        }

        [Test]
        public async Task MethodCall_Event_WithLayerStackErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            transportBuilder.SetLayerStackProvider(new LayerStackProvider<Dummy>(errorLayer));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(connections[0]);
            var theEvent = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.EVENT, errorOnSend: false, errorOnReceive: true);

            ManualResetEventSlim waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire 1");
            testService.ClearResetEvent();

            Assert.AreEqual(0, testService.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.EVENT, errorOnSend: true, errorOnReceive: false);
            theEvent.int_value = 101;

            waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire 2");
            testService.ClearResetEvent();

            Assert.AreEqual(0, testService.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testService.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.EVENT, errorOnSend: false, errorOnReceive: false);
            theEvent.int_value = 102;

            waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testService.EventCount);
            Assert.AreEqual(theEvent.int_value, testService.LastEventReceived.int_value);
        }

        [Test]
        public async Task MethodCall_Event_FailingLayerStackProvider_ClientSendEvent()
        {
            // Fail after 2 successful GetLayerStack calls (1 on client, 1 on server)
            transportBuilder.SetLayerStackProvider(new TestLayerStackProvider_Fails(2));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(connections[0]);
            var theEvent = new Dummy { int_value = 100 };

            ManualResetEventSlim waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testService.EventCount);
            Assert.AreEqual(theEvent.int_value, testService.LastEventReceived.int_value);

            waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire 2");

            Assert.AreEqual(1, testService.EventCount);
        }

        [Test]
        public async Task MethodCall_Event_FailingLayerStackProvider_ServerReceiveEvent()
        {
            // Fail after 3 successful GetLayerStack calls (2 on client, 1 on server)
            transportBuilder.SetLayerStackProvider(new TestLayerStackProvider_Fails(3));
            var testService = new DummyTestService();
            await DefaultSetup(testService, 1);

            var proxy = new DummyTestProxy<SimpleInMemConnection>(connections[0]);
            var theEvent = new Dummy { int_value = 100 };

            ManualResetEventSlim waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testService.EventCount);
            Assert.AreEqual(theEvent.int_value, testService.LastEventReceived.int_value);

            waitForEvent = testService.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire 2");

            Assert.AreEqual(1, testService.EventCount);
        }

        [TearDown]
        public async void Cleanup()
        {
            if (connections != null)
            {
                for (int connectionIndex = 0; connectionIndex < connections.Length; connectionIndex++)
                {
                    await connections[connectionIndex].StopAsync();
                    Assert.IsFalse(connections[connectionIndex].IsConnected);
                    Assert.IsFalse(connections[connectionIndex].IsPaired);
                }
            }
            connections = null;
            await transport.StopAsync();
        }
    }
}
