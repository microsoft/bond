// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.Comm.Layers;
    using NUnit.Framework;
    using UnitTest.Comm;
    using UnitTest.Interfaces;
    using UnitTest.Layers;

    [TestFixture]
    public class EpoxyTransportTests : EpoxyTestBase
    {
        private const string AnyIpAddressString = "10.1.2.3";
        private const int AnyPort = 12345;
        private static readonly IPAddress AnyIpAddress = new IPAddress(new byte[] { 10, 1, 2, 3 });
        private static readonly Message<Bond.Void> EmptyMessage = new Message<Bond.Void>(new Bond.Void());

        [Test]
        public void DefaultPorts_AreExpected()
        {
            Assert.AreEqual(25188, EpoxyTransport.DefaultPort);
        }

        [Test]
        public void Endpoint_Ctors()
        {
            var ipEndPoint = new IPEndPoint(IPAddress.Parse(AnyIpAddressString), AnyPort);

            var endpointFromPieces = new EpoxyTransport.Endpoint(AnyIpAddressString, AnyPort);
            Assert.AreEqual(AnyIpAddressString, endpointFromPieces.Host);
            Assert.AreEqual(AnyPort, endpointFromPieces.Port);

            var endpointFromIpEndPoint = new EpoxyTransport.Endpoint(ipEndPoint);
            Assert.AreEqual(AnyIpAddressString, endpointFromIpEndPoint.Host);
            Assert.AreEqual(AnyPort, endpointFromIpEndPoint.Port);

            Assert.AreEqual(endpointFromPieces, endpointFromIpEndPoint);
        }

        [Test]
        public void ParseStringAddress_NullOrEmpty_Throws()
        {
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress(null));
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress(string.Empty));
        }

        [Test]
        public void ParseStringAddress_ValidIpNoPort_ReturnsIpEndpoint()
        {
            var result = EpoxyTransport.ParseStringAddress(AnyIpAddressString);
            Assert.AreEqual(new IPEndPoint(AnyIpAddress, EpoxyTransport.DefaultPort), result);
        }

        [Test]
        public void ParseStringAddress_ValidIpWithPort_ReturnsIpEndpoint()
        {
            var result = EpoxyTransport.ParseStringAddress(AnyIpAddressString + ":" + AnyPort);
            Assert.AreEqual(new IPEndPoint(AnyIpAddress, AnyPort), result);
        }

        [Test]
        public void ParseStringAddress_InvalidIpAddress_Throws()
        {
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress("not an ip"));
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress("not an ip:12345"));
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress("not an ip:no a port"));
        }

        [Test]
        public void ParseStringAddress_InvalidPortAddress_Throws()
        {
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress("10.1.2.3:"));
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress("10.1.2.3::"));
            Assert.Throws<ArgumentException>(() => EpoxyTransport.ParseStringAddress("10.1.2.3:not a port"));
        }

        [Test]
        public void Parse_InvalidUris()
        {
            Assert.Null(EpoxyTransport.Parse(null, LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("127.0.0.1", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("cows", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse(":12", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("epoxy://127.0.0.1:1000:1000", LoggerTests.BlackHole));
        }

        [Test]
        public void Parse_SchemeMustBeEpoxy()
        {
            Assert.Null(EpoxyTransport.Parse("http://127.0.0.1", LoggerTests.BlackHole));
        }

        [Test]
        public void Parse_NoResourceBesidesRootOrParamsAllowed()
        {
            Assert.Null(EpoxyTransport.Parse("epoxy://127.0.0.1/cows", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("epoxy://127.0.0.1?cows=cows", LoggerTests.BlackHole));
        }

        [Test]
        public void Parse_AcceptableUris()
        {
            var endpoint = EpoxyTransport.Parse("epoxy://127.0.0.1", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("127.0.0.1", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultPort, endpoint.Value.Port);

            endpoint = EpoxyTransport.Parse("epoxy://127.0.0.1/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("127.0.0.1", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultPort, endpoint.Value.Port);

            endpoint = EpoxyTransport.Parse("epoxy://127.0.0.1:10000", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("127.0.0.1", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);

            endpoint = EpoxyTransport.Parse("epoxy://127.0.0.1:10000/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("127.0.0.1", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);

            endpoint = EpoxyTransport.Parse("epoxy://localhost", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultPort, endpoint.Value.Port);

            endpoint = EpoxyTransport.Parse("epoxy://localhost/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultPort, endpoint.Value.Port);

            endpoint = EpoxyTransport.Parse("epoxy://localhost:10000", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);

            endpoint = EpoxyTransport.Parse("epoxy://localhost:10000/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);
        }

        [Test]
        public void Builder_Construct_NoArgs_Succeeds()
        {
            var builder = new EpoxyTransportBuilder();
            Assert.NotNull(builder.Construct());
        }

        [Test]
        public async Task SetupListener_RequestReply_PayloadResponse()
        {
            TestClientServer<TestService> testClientServer = await SetupTestClientServer<TestService>();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService.RespondWithEmpty", EmptyMessage, CancellationToken.None);

            Assert.IsFalse(response.IsError);
            Assert.IsNotNull(response.Payload);
            Assert.IsNull(response.Error);

            Assert.AreEqual(1, testClientServer.Service.RespondWithEmpty_CallCount);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task SetupListener_RequestError_ErrorResponse()
        {
            TestClientServer<TestService> testClientServer = await SetupTestClientServer<TestService>();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService.RespondWithError", EmptyMessage, CancellationToken.None);

            Assert.IsTrue(response.IsError);
            Assert.IsNotNull(response.Error);

            var error = response.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task SetupListenerWithErrorHandler_RequestThatThrows_ErrorResponse()
        {
            TestClientServer<TestService> testClientServer = await SetupTestClientServer<TestService>();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService.ThrowInsteadOfResponding", EmptyMessage, CancellationToken.None);

            Assert.IsTrue(response.IsError);
            Assert.IsNotNull(response.Error);

            var error = response.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.That(error.message, Is.StringContaining(Errors.InternalErrorMessage));

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task TransportWithCustomResolver_UsesResolver()
        {
            await SetupTestClientServer<DummyTestService>();

            Func<string, Task<IPAddress>> customResolver = host => Task.FromResult(IPAddress.Loopback);
            var clientTransport = new EpoxyTransportBuilder().SetResolver(customResolver).Construct();
            EpoxyConnection clientConnection = await clientTransport.ConnectToAsync("epoxy://resolve-this-to-localhost/");
            var proxy = new DummyTestProxy<EpoxyConnection>(clientConnection);

            var request = new Dummy {int_value = 100};
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);

            Assert.IsFalse(response.IsError);
            var result = response.Payload.Deserialize();
            Assert.AreEqual(101, result.int_value);

            await clientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse()
        {
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>();
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            Assert.AreEqual(1, testClientServer.Service.RequestCount);
            Assert.AreEqual(0, testClientServer.Service.EventCount);
            Assert.AreEqual(request.int_value, testClientServer.Service.LastRequestReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event()
        {
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>();
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var theEvent = new Dummy { int_value = 100 };

            ManualResetEventSlim waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(5));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(0, testClientServer.Service.RequestCount);
            Assert.AreEqual(1, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
         }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_LayerData()
        {
            var layerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, new TestLayer_CheckPassedValue(1234));
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(layerStackProvider, layerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            Assert.AreEqual(1, testClientServer.Service.RequestCount);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
         }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_ClientLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var clientLayerStack = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(null, clientLayerStack);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Request, errorOnSend: true, errorOnReceive: false);
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, response.Error.Deserialize().error_code);

            Assert.AreEqual(0, testClientServer.Service.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.Response, errorOnSend: false, errorOnReceive: true);
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, response.Error.Deserialize().error_code);

            Assert.AreEqual(1, testClientServer.Service.RequestCount);
            Assert.AreEqual(request.int_value, testClientServer.Service.LastRequestReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_StatefulLayers()
        {
            var clientLayerProvider = new TestLayerProvider_StatefulAppend("Client");
            var clientLayerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, clientLayerProvider);
            var serverLayerProvider = new TestLayerProvider_StatefulAppend("Server");
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, serverLayerProvider);
            TestClientServer<DummyTestService> testClientServer =
                await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, clientLayerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            clientLayerProvider.Layers.Clear();
            serverLayerProvider.Layers.Clear();

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            Assert.AreEqual(1, clientLayerProvider.Layers.Count);
            Assert.AreEqual(1, serverLayerProvider.Layers.Count);
            Assert.AreEqual("Client0SendClient0Receive", clientLayerProvider.Layers[0].State);
            Assert.AreEqual("Server0ReceiveServer0Send", serverLayerProvider.Layers[0].State);

            request.int_value = 101;
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(102, response.Payload.Deserialize().int_value);

            Assert.AreEqual(2, clientLayerProvider.Layers.Count);
            Assert.AreEqual(2, serverLayerProvider.Layers.Count);
            Assert.AreEqual("Client1SendClient1Receive", clientLayerProvider.Layers[1].State);
            Assert.AreEqual("Server1ReceiveServer1Send", serverLayerProvider.Layers[1].State);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_ServerLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, null);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Request, errorOnSend: false, errorOnReceive: true);

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, error.error_code);

            Assert.AreEqual(0, testClientServer.Service.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.Response, errorOnSend: true, errorOnReceive: false);
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, error.error_code);

            Assert.AreEqual(1, testClientServer.Service.RequestCount);
            Assert.AreEqual(request.int_value, testClientServer.Service.LastRequestReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event_ClientLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var clientLayerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(null, clientLayerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var theEvent = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Event, errorOnSend: true, errorOnReceive: false);

            ManualResetEventSlim waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire");
            testClientServer.Service.ClearResetEvent();

            Assert.AreEqual(0, testClientServer.Service.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.Event, errorOnSend: false, errorOnReceive: true);

            theEvent.int_value = 101;

            waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event_ServerLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, null);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var theEvent = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Event, errorOnSend: false, errorOnReceive: true);

            ManualResetEventSlim waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire");
            testClientServer.Service.ClearResetEvent();

            Assert.AreEqual(0, testClientServer.Service.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.Event, errorOnSend: true, errorOnReceive: false);
            theEvent.int_value = 101;

            waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event_StatefulLayers()
        {
            var clientLayerProvider = new TestLayerProvider_StatefulAppend("Client");
            var clientLayerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, clientLayerProvider);
            var serverLayerProvider = new TestLayerProvider_StatefulAppend("Server");
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(LoggerTests.BlackHole, serverLayerProvider);
            TestClientServer<DummyTestService> testClientServer =
                await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, clientLayerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var theEvent = new Dummy { int_value = 100 };

            clientLayerProvider.Layers.Clear();
            serverLayerProvider.Layers.Clear();

            ManualResetEventSlim waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);

            Assert.AreEqual(1, clientLayerProvider.Layers.Count);
            Assert.AreEqual(1, serverLayerProvider.Layers.Count);
            Assert.AreEqual("Client0Send", clientLayerProvider.Layers[0].State);
            Assert.AreEqual("Server0Receive", serverLayerProvider.Layers[0].State);

            theEvent.int_value = 101;

            waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(2, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);

            Assert.AreEqual(2, clientLayerProvider.Layers.Count);
            Assert.AreEqual(2, serverLayerProvider.Layers.Count);
            Assert.AreEqual("Client1Send", clientLayerProvider.Layers[1].State);
            Assert.AreEqual("Server1Receive", serverLayerProvider.Layers[1].State);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_FailingLayerProvider_ClientSendReq()
        {
            // Fail after 1 successful GetLayerStack calls on client side
            var clientLayerStackProvider = new TestLayerStackProvider_Fails(1);
            TestClientServer<DummyTestService> testClientServer =
                await SetupTestClientServer<DummyTestService>(null, clientLayerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            request.int_value = 101;
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.AreEqual(TestLayerStackProvider_Fails.InternalDetails, error.message);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_FailingLayerProvider_ServerReceiveReq()
        {
            // Fail after 1 successful GetLayerStack calls on server side
            var serverLayerStackProvider = new TestLayerStackProvider_Fails(1);
            TestClientServer<DummyTestService> testClientServer =
                await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, null);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            request.int_value = 101;
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.AreEqual(Errors.InternalErrorMessage, error.message);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedGenericService_GeneratedGenericProxy_PayloadResponse()
        {
            TestClientServer<GenericDummyTestService> testClientServer = await SetupTestClientServer<GenericDummyTestService>();
            var proxy = new GenericTestProxy<Dummy, EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();

            Assert.AreEqual(1, testClientServer.Service.RequestCount);
            Assert.AreEqual(0, testClientServer.Service.EventCount);
            Assert.AreEqual(request.int_value, testClientServer.Service.LastRequestReceived.int_value);
        }

        [Test]
        public async Task GeneratedGenericService_GeneratedGenericProxy_Event()
        {
            TestClientServer<GenericDummyTestService> testClientServer = await SetupTestClientServer<GenericDummyTestService>();
            var proxy = new GenericTestProxy<Dummy, EpoxyConnection>(testClientServer.ClientConnection);
            var theEvent = new Dummy { int_value = 100 };

            ManualResetEventSlim waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(0, testClientServer.Service.RequestCount);
            Assert.AreEqual(1, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public void TestServiceMethodTypeValidation_Throws()
        {
            var exception = Assert.Throws<ArgumentException>(async () => await SetupTestClientServer<TestServiceEventMismatch>());
            Assert.That(exception.Message, Is.StringContaining("registered as Event but callback not implemented as such"));
            exception = Assert.Throws<ArgumentException>(async () => await SetupTestClientServer<TestServiceReqResMismatch>());
            Assert.That(exception.Message, Is.StringContaining("registered as RequestResponse but callback not implemented as such"));
            exception = Assert.Throws<ArgumentException>(async () => await SetupTestClientServer<TestServiceUnsupported>());
            Assert.That(exception.Message, Is.StringContaining("registered as invalid type"));
        }

        [Test]
        public async Task IPv6Listener_RequestReply_PayloadResponse()
        {
            var transport = new EpoxyTransportBuilder().Construct();
            listener = transport.MakeListener(new IPEndPoint(IPAddress.IPv6Loopback, EpoxyTransport.DefaultPort));
            listener.AddService(new DummyTestService());
            await listener.StartAsync();

            EpoxyConnection conn = await transport.ConnectToAsync("epoxy://[::1]");
            var proxy = new DummyTestProxy<EpoxyConnection>(conn);
            var request = new Dummy { int_value = 100 };

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            await transport.StopAsync();
        }
    }
}
