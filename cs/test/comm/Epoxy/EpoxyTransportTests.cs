// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Diagnostics;
    using System.Net;
    using System.Net.Security;
    using System.Security.Authentication;
    using System.Security.Cryptography.X509Certificates;
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

        X509Certificate2 testRootCert;
        const string TestRootThumbprint = "29D6B3199BE91CB38D94FD1F2883A9FD2126C91D";
        X509Certificate2 testServerCert;
        X509Certificate2 testClientCert;

        [TestFixtureSetUp]
        public void Init()
        {
            const string TestCertificatePassword = "bond";
            testRootCert = new X509Certificate2(@"Epoxy\certs\bond-test-root.pfx", TestCertificatePassword);
            testServerCert = new X509Certificate2(@"Epoxy\certs\bond-test-server1.pfx", TestCertificatePassword);
            testClientCert = new X509Certificate2(@"Epoxy\certs\bond-test-client1.pfx", TestCertificatePassword);
        }

        [Test]
        public void DefaultPorts_AreExpected()
        {
            Assert.AreEqual(25188, EpoxyTransport.DefaultInsecurePort);
            Assert.AreEqual(25156, EpoxyTransport.DefaultSecurePort);
    }

        [Test]
        public void Endpoint_Ctors()
        {
            var ipEndPoint = new IPEndPoint(IPAddress.Parse(AnyIpAddressString), AnyPort);

            var endpointFromPieces = new EpoxyTransport.Endpoint(AnyIpAddressString, AnyPort, useTls: true);
            Assert.AreEqual(AnyIpAddressString, endpointFromPieces.Host);
            Assert.AreEqual(AnyPort, endpointFromPieces.Port);
            Assert.True(endpointFromPieces.UseTls);

            var endpointFromIpEndPoint = new EpoxyTransport.Endpoint(ipEndPoint, useTls: true);
            Assert.AreEqual(AnyIpAddressString, endpointFromIpEndPoint.Host);
            Assert.AreEqual(AnyPort, endpointFromIpEndPoint.Port);
            Assert.True(endpointFromIpEndPoint.UseTls);

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
            Assert.AreEqual(new IPEndPoint(AnyIpAddress, EpoxyTransport.DefaultInsecurePort), result);
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
            Assert.Null(EpoxyTransport.Parse(string.Empty, LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("127.0.0.1", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("cows", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse(":12", LoggerTests.BlackHole));
            Assert.Null(EpoxyTransport.Parse("epoxy://127.0.0.1:1000:1000", LoggerTests.BlackHole));
        }

        [Test]
        public void Parse_SchemeMustBeEpoxyish()
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
            Assert.AreEqual(EpoxyTransport.DefaultInsecurePort, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxy://127.0.0.1/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("127.0.0.1", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultInsecurePort, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxy://127.0.0.1:10000", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("127.0.0.1", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxy://127.0.0.1:10000/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("127.0.0.1", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxy://localhost", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultInsecurePort, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxy://localhost/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultInsecurePort, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxy://localhost:10000", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxy://localhost:10000/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("localhost", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);
            Assert.False(endpoint.Value.UseTls);
        }

        [Test]
        public void Parse_EpoxysUris()
        {
            var endpoint = EpoxyTransport.Parse("epoxys://use-default-secure-port", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("use-default-secure-port", endpoint.Value.Host);
            Assert.AreEqual(EpoxyTransport.DefaultSecurePort, endpoint.Value.Port);
            Assert.True(endpoint.Value.UseTls);

            endpoint = EpoxyTransport.Parse("epoxys://use-custom-port:10000/", LoggerTests.BlackHole);
            Assert.NotNull(endpoint);
            Assert.AreEqual("use-custom-port", endpoint.Value.Host);
            Assert.AreEqual(10000, endpoint.Value.Port);
            Assert.True(endpoint.Value.UseTls);
        }

        [Test]
        public void Builder_Construct_NoArgs_Succeeds()
        {
            var builder = new EpoxyTransportBuilder();
            EpoxyTransport result = builder.Construct();
            Assert.NotNull(result);
            transports.Add(result);
        }

        [Test]
        public async Task SetupListener_RequestReply_PayloadResponse()
        {
            TestClientServer<TestService> testClientServer = await SetupTestClientServer<TestService>();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService", "RespondWithEmpty", EmptyMessage, CancellationToken.None);

            Assert.IsFalse(response.IsError);
            Assert.IsNotNull(response.Payload);
            Assert.IsNull(response.Error);

            Assert.AreEqual(1, testClientServer.Service.RespondWithEmpty_CallCount);
        }

        [Test]
        public async Task SetupListener_RequestError_ErrorResponse()
        {
            TestClientServer<TestService> testClientServer = await SetupTestClientServer<TestService>();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService", "RespondWithError", EmptyMessage, CancellationToken.None);

            Assert.IsTrue(response.IsError);
            Assert.IsNotNull(response.Error);

            var error = response.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
        }

        [Test]
        public async Task SetupListenerWithErrorHandler_RequestThatThrows_ErrorResponse()
        {
            TestClientServer<TestService> testClientServer = await SetupTestClientServer<TestService>();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService", "ThrowInsteadOfResponding", EmptyMessage, CancellationToken.None);

            Assert.IsTrue(response.IsError);
            Assert.IsNotNull(response.Error);

            var error = response.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
            Assert.That(error.message, Is.StringContaining(Errors.InternalErrorMessage));
        }

        [Test]
        public async Task TransportWithCustomResolver_UsesResolver()
        {
            await SetupTestClientServer<DummyTestService>();

            var clientTransport = new EpoxyTransportBuilder().SetResolver(ResolveEverythingToLocalhost).Construct();
            transports.Add(clientTransport);
            EpoxyConnection clientConnection = await clientTransport.ConnectToAsync("epoxy://resolve-this-to-localhost/");
            var proxy = new DummyTestProxy<EpoxyConnection>(clientConnection);

            await AssertRequestResponseWorksAsync(proxy);

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
         }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_LayerData()
        {
            var layerStackProvider = new LayerStackProvider<Dummy>(new TestLayer_CheckPassedValue(1234));
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(layerStackProvider, layerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);

            await AssertRequestResponseWorksAsync(proxy);
            Assert.AreEqual(1, testClientServer.Service.RequestCount);
         }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_ClientLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var clientLayerStack = new LayerStackProvider<Dummy>(errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(null, clientLayerStack);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.REQUEST, errorOnSend: true, errorOnReceive: false);
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, response.Error.Deserialize().error_code);

            Assert.AreEqual(0, testClientServer.Service.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.RESPONSE, errorOnSend: false, errorOnReceive: true);
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, response.Error.Deserialize().error_code);

            Assert.AreEqual(1, testClientServer.Service.RequestCount);
            Assert.AreEqual(request.int_value, testClientServer.Service.LastRequestReceived.int_value);
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_StatefulLayers()
        {
            var clientLayerProvider = new TestLayerProvider_StatefulAppend("Client");
            var clientLayerStackProvider = new LayerStackProvider<Dummy>(clientLayerProvider);
            var serverLayerProvider = new TestLayerProvider_StatefulAppend("Server");
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(serverLayerProvider);
            TestClientServer<DummyTestService> testClientServer =
                await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, clientLayerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);

            clientLayerProvider.Layers.Clear();
            serverLayerProvider.Layers.Clear();

            await AssertRequestResponseWorksAsync(proxy);
            Assert.AreEqual(1, clientLayerProvider.Layers.Count);
            Assert.AreEqual(1, serverLayerProvider.Layers.Count);
            Assert.AreEqual("Client0SendClient0Receive", clientLayerProvider.Layers[0].State);
            Assert.AreEqual("Server0ReceiveServer0Send", serverLayerProvider.Layers[0].State);

            await AssertRequestResponseWorksAsync(proxy);
            Assert.AreEqual(2, clientLayerProvider.Layers.Count);
            Assert.AreEqual(2, serverLayerProvider.Layers.Count);
            Assert.AreEqual("Client1SendClient1Receive", clientLayerProvider.Layers[1].State);
            Assert.AreEqual("Server1ReceiveServer1Send", serverLayerProvider.Layers[1].State);
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_ServerLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, null);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.REQUEST, errorOnSend: false, errorOnReceive: true);

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, error.error_code);

            Assert.AreEqual(0, testClientServer.Service.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.RESPONSE, errorOnSend: true, errorOnReceive: false);
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            error = response.Error.Deserialize();
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, error.error_code);

            Assert.AreEqual(1, testClientServer.Service.RequestCount);
            Assert.AreEqual(request.int_value, testClientServer.Service.LastRequestReceived.int_value);
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event_ClientLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var clientLayerStackProvider = new LayerStackProvider<Dummy>(errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(null, clientLayerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var theEvent = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.EVENT, errorOnSend: true, errorOnReceive: false);

            ManualResetEventSlim waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire");
            testClientServer.Service.ClearResetEvent();

            Assert.AreEqual(0, testClientServer.Service.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.EVENT, errorOnSend: false, errorOnReceive: true);

            theEvent.int_value = 101;

            waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event_ServerLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, null);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var theEvent = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.EVENT, errorOnSend: false, errorOnReceive: true);

            ManualResetEventSlim waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            bool wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsFalse(wasSignaled, "Event should not fire");
            testClientServer.Service.ClearResetEvent();

            Assert.AreEqual(0, testClientServer.Service.EventCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastEventReceived.int_value);

            errorLayer.SetState(MessageType.EVENT, errorOnSend: true, errorOnReceive: false);
            theEvent.int_value = 101;

            waitForEvent = testClientServer.Service.CreateResetEvent();
            proxy.EventMethodAsync(theEvent);
            wasSignaled = waitForEvent.Wait(TimeSpan.FromSeconds(1));
            Assert.IsTrue(wasSignaled, "Timed out waiting for event to fire");

            Assert.AreEqual(1, testClientServer.Service.EventCount);
            Assert.AreEqual(theEvent.int_value, testClientServer.Service.LastEventReceived.int_value);
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event_StatefulLayers()
        {
            var clientLayerProvider = new TestLayerProvider_StatefulAppend("Client");
            var clientLayerStackProvider = new LayerStackProvider<Dummy>(clientLayerProvider);
            var serverLayerProvider = new TestLayerProvider_StatefulAppend("Server");
            var serverLayerStackProvider = new LayerStackProvider<Dummy>(serverLayerProvider);
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
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_FailingLayerProvider_ClientSendReq()
        {
            // Fail after 1 successful GetLayerStack calls on client side
            var clientLayerStackProvider = new TestLayerStackProvider_Fails(1);
            TestClientServer<DummyTestService> testClientServer =
                await SetupTestClientServer<DummyTestService>(null, clientLayerStackProvider);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);

            await AssertRequestResponseWorksAsync(proxy);

            var request = new Dummy { int_value = 101 };
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
            Assert.AreEqual(TestLayerStackProvider_Fails.InternalDetails, error.message);
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_FailingLayerProvider_ServerReceiveReq()
        {
            // Fail after 1 successful GetLayerStack calls on server side
            var serverLayerStackProvider = new TestLayerStackProvider_Fails(1);
            TestClientServer<DummyTestService> testClientServer =
                await SetupTestClientServer<DummyTestService>(serverLayerStackProvider, null);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);

            await AssertRequestResponseWorksAsync(proxy);

            var request = new Dummy { int_value = 101 };
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Error error = response.Error.Deserialize();
            Assert.AreEqual((int)ErrorCode.INTERNAL_SERVER_ERROR, error.error_code);
            Assert.AreEqual(Errors.InternalErrorMessage, error.message);
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
            transports.Add(transport);
            var listener = transport.MakeListener(new IPEndPoint(IPAddress.IPv6Loopback, EpoxyTransport.DefaultInsecurePort));
            listener.AddService(new DummyTestService());
            await listener.StartAsync();

            EpoxyConnection conn = await transport.ConnectToAsync("epoxy://[::1]");
            var proxy = new DummyTestProxy<EpoxyConnection>(conn);
            var request = new Dummy { int_value = 100 };

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);
        }

        [Test]
        public async Task Tls_ServerOnly_CanAuthenticate()
        {
            var serverTlsConfig = new EpoxyServerTlsConfig(testServerCert, checkCertificateRevocation: false);

            var serverTransport = new EpoxyTransportBuilder().SetServerTlsConfig(serverTlsConfig).Construct();
            transports.Add(serverTransport);
            var listener = serverTransport.MakeListener(new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultSecurePort));
            listener.AddService(new DummyTestService());
            await listener.StartAsync();

            var clientTlsConfig = new EpoxyClientTlsConfig(
                checkCertificateRevocation: false,
                remoteCertificateValidationCallback: EnsureRootedWithTestCertificate);

            var clientTransport = new EpoxyTransportBuilder()
                .SetResolver(ResolveEverythingToLocalhost)
                .SetClientTlsConfig(clientTlsConfig)
                .Construct();
            transports.Add(clientTransport);
            EpoxyConnection clientConnection = await clientTransport.ConnectToAsync("epoxys://bond-test-server1");

            var proxy = new DummyTestProxy<EpoxyConnection>(clientConnection);

            await AssertRequestResponseWorksAsync(proxy);
        }

        [Test]
        public async Task Tls_ServerBadCert_ConnectionFails()
        {
            var serverTlsConfig = new EpoxyServerTlsConfig(testServerCert, checkCertificateRevocation: false);

            var serverTransport =
                new EpoxyTransportBuilder().SetServerTlsConfig(serverTlsConfig)
                    .Construct();
            transports.Add(serverTransport);
            var listener = serverTransport.MakeListener(new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultSecurePort));
            listener.AddService(new DummyTestService());
            await listener.StartAsync();

            var clientTlsConfig = new EpoxyClientTlsConfig(
                checkCertificateRevocation: false,
                // Intentionally set this to null so that the client gets an
                // invalid certificate. If this test passes on your machine,
                // it's probably because you've installed the Bond test root
                // certificate as a trusted root. This certificate cannot be
                // trusted, so you should uninstall it.
                remoteCertificateValidationCallback: null);

            var clientTransport = new EpoxyTransportBuilder()
                .SetResolver(ResolveEverythingToLocalhost)
                .SetClientTlsConfig(clientTlsConfig)
                .Construct();
            transports.Add(clientTransport);

            Assert.Throws<AuthenticationException>(async () => await clientTransport.ConnectToAsync("epoxys://bond-test-server1"));
        }

        [Test]
        public async Task Tls_Mutual_CanAuthenticate()
        {
            var serverTlsConfig = new EpoxyServerTlsConfig(
                testServerCert,
                checkCertificateRevocation: false,
                clientCertificateRequired: true,
                remoteCertificateValidationCallback: EnsureRootedWithTestCertificate
            );

            var clientTlsConfig = new EpoxyClientTlsConfig(
                certificate: testClientCert,
                checkCertificateRevocation: false,
                remoteCertificateValidationCallback: EnsureRootedWithTestCertificate);

            var transport = new EpoxyTransportBuilder()
                .SetResolver(ResolveEverythingToLocalhost)
                .SetServerTlsConfig(serverTlsConfig)
                .SetClientTlsConfig(clientTlsConfig)
                .Construct();
            transports.Add(transport);

            var listener = transport.MakeListener(new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultSecurePort));
            listener.AddService(new DummyTestService());
            await listener.StartAsync();

            EpoxyConnection clientConnection = await transport.ConnectToAsync("epoxys://bond-test-server1");

            var proxy = new DummyTestProxy<EpoxyConnection>(clientConnection);

            await AssertRequestResponseWorksAsync(proxy);
        }

        [Test]
        public async Task Tls_MutualNoClientCert_ProxyDoesNotWork()
        {
            var serverTlsConfig = new EpoxyServerTlsConfig(
                testServerCert,
                checkCertificateRevocation: false,
                clientCertificateRequired: true,
                remoteCertificateValidationCallback: EnsureRootedWithTestCertificate
            );

            var clientTlsConfig = new EpoxyClientTlsConfig(certificate: null,
                checkCertificateRevocation: false,
                remoteCertificateValidationCallback: EnsureRootedWithTestCertificate);

            var transport = new EpoxyTransportBuilder()
                .SetResolver(ResolveEverythingToLocalhost)
                .SetServerTlsConfig(serverTlsConfig)
                .SetClientTlsConfig(clientTlsConfig)
                .Construct();
            transports.Add(transport);

            var listener = transport.MakeListener(new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultSecurePort));
            listener.AddService(new DummyTestService());
            await listener.StartAsync();

            try
            {
                // The .NET SslStream implementation currently does not give us
                // a way to signal during TLS handshaking that the server is
                // rejecting the connection. Instead, we have to RST the
                // underlying socket. With Epoxy's current implementation, this
                // can't reliably be detected at connection time. So we attempt
                // to exercise the connection using a proxy and expect that to fail.
                EpoxyConnection clientConnection = await transport.ConnectToAsync("epoxys://bond-test-server1");
                var proxy = new DummyTestProxy<EpoxyConnection>(clientConnection);
                await AssertRequestResponseWorksAsync(proxy);
            }
            catch (Exception ex) when (ex is InvalidOperationException || ex is AuthenticationException)
            {
                // An expected exception type, depending on timing, so pass the
                // test.
            }
            catch (Exception ex)
            {
                Assert.Fail("Unexpected exception of type {0}: {1}", ex.GetType(), ex);
            }
        }

        private static Task<IPAddress> ResolveEverythingToLocalhost(string host)
        {
            return Task.FromResult(IPAddress.Loopback);
        }

        bool EnsureRootedWithTestCertificate(
            object sender,
            X509Certificate certificate,
            X509Chain chain,
            SslPolicyErrors sslPolicyErrors)
        {
            if (sslPolicyErrors == SslPolicyErrors.None)
            {
                // The machine's policy allowed the certificate. Don't second
                // guess that policy.
                return true;
            }

            // No certificate to validate, so it isn't rooted properly.
            if (certificate == null)
            {
                return false;
            }

            // Otherwise, we validate that the certificate chain is valid and
            // rooted with the expected root.
            var customChain = new X509Chain();
            customChain.ChainPolicy.ExtraStore.Add(testRootCert);
            customChain.ChainPolicy.RevocationMode = X509RevocationMode.NoCheck;

            if (customChain.Build(new X509Certificate2(certificate)))
            {
                return true;
            }

            if (customChain.ChainStatus.Length > 1
                || customChain.ChainStatus[0].Status != X509ChainStatusFlags.UntrustedRoot)
            {
                // Some error other than untrusted root, which we were
                // expecting has occured. Fail this request.
                return false;
            }

            // Now, require exact thumbprint match for expected root certificate.
            X509ChainElement root = customChain.ChainElements[customChain.ChainElements.Count - 1];
            return root.Certificate.Thumbprint == TestRootThumbprint;
        }

        async Task AssertRequestResponseWorksAsync(DummyTestProxy<EpoxyConnection> proxy)
        {
            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(new Dummy { int_value = 100 });
            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);
        }
    }
}
