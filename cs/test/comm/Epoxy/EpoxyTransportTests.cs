// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System;
    using System.Collections.Generic;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.Comm.Layers;
    using NUnit.Framework;
    using UnitTest.Comm;
    using UnitTest.Layers;

    [TestFixture]
    public class EpoxyTransportTests
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
            Assert.That(error.message, Is.StringContaining(Transport.InternalErrorMessage));

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
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
            LayerStack<Dummy> layerStack = new LayerStack<Dummy>(new TestLayer_CheckPassedValue(1234));
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(layerStack, layerStack);
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
            LayerStack<Dummy> clientLayerStack = new LayerStack<Dummy>(errorLayer);
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
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_ServerLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            LayerStack<Dummy> serverLayerStack = new LayerStack<Dummy>(errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(serverLayerStack, null);
            var proxy = new DummyTestProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Request, errorOnSend: false, errorOnReceive: true);

            IMessage<Dummy> response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.ReceiveError, response.Error.Deserialize().error_code);

            Assert.AreEqual(0, testClientServer.Service.RequestCount);
            Assert.AreEqual(Dummy.Empty.int_value, testClientServer.Service.LastRequestReceived.int_value);

            errorLayer.SetState(MessageType.Response, errorOnSend: true, errorOnReceive: false);
            response = await proxy.ReqRspMethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(TestLayer_ReturnErrors.SendError, response.Error.Deserialize().error_code);

            Assert.AreEqual(1, testClientServer.Service.RequestCount);
            Assert.AreEqual(request.int_value, testClientServer.Service.LastRequestReceived.int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_Event_ClientLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            LayerStack<Dummy> clientLayerStack = new LayerStack<Dummy>(errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(null, clientLayerStack);
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
            LayerStack<Dummy> serverLayerStack = new LayerStack<Dummy>(errorLayer);
            TestClientServer<DummyTestService> testClientServer = await SetupTestClientServer<DummyTestService>(serverLayerStack, null);
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

        public class TestClientServer<TService>
        {
            public TService Service;
            public EpoxyTransport ServiceTransport;
            public EpoxyListener Listener;
            public EpoxyConnection ClientConnection;
            public EpoxyTransport ClientTransport;
        }

        public static async Task<TestClientServer<TService>> SetupTestClientServer<TService>(ILayerStack serviceLayerStack = null,
                                                                                              ILayerStack clientLayerStack = null) where TService : class, IService, new()
        {
            var testService = new TService();

            EpoxyTransport serviceTransport = new EpoxyTransportBuilder()
                .SetLayerStack(serviceLayerStack)
                .Construct();
            EpoxyListener listener = serviceTransport.MakeListener(new IPEndPoint(IPAddress.Loopback, 0));
            listener.AddService(testService);
            await listener.StartAsync();

            EpoxyTransport clientTransport = new EpoxyTransportBuilder()
                // some tests rely on the use of DebugExceptionHandler to assert things about the error message
                .SetLayerStack(clientLayerStack)
                .Construct();
            EpoxyConnection clientConnection = await clientTransport.ConnectToAsync(listener.ListenEndpoint);

            return new TestClientServer<TService>
            {
                Service = testService,
                ServiceTransport = serviceTransport,
                Listener = listener,
                ClientConnection = clientConnection,
                ClientTransport = clientTransport
            };
        }

        public class TestService : IService
        {
            private int respondWithEmpty_callCount = 0;

            public IEnumerable<ServiceMethodInfo> Methods
            {
                get
                {
                    return new[]
                    {
                        new ServiceMethodInfo
                        {
                            MethodName = "TestService.RespondWithEmpty",
                            Callback = RespondWithEmpty
                        },
                        new ServiceMethodInfo
                        {
                            MethodName = "TestService.RespondWithError",
                            Callback = RespondWithError,
                        },
                        new ServiceMethodInfo
                        {
                            MethodName = "TestService.ThrowInsteadOfResponding",
                            Callback = ThrowInsteadOfResponding,
                        },
                    };
                }
            }

            public int RespondWithEmpty_CallCount
            {
                get
                {
                    return respondWithEmpty_callCount;
                }
            }

            private Task<IMessage> RespondWithEmpty(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                Interlocked.Increment(ref respondWithEmpty_callCount);
                var emptyMessage = Message.FromPayload(new Bond.Void());
                return Task.FromResult<IMessage>(emptyMessage);
            }

            private Task<IMessage> RespondWithError(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                var error = new Error
                {
                    error_code = (int) ErrorCode.InternalServerError,
                };

                return Task.FromResult<IMessage>(Message.FromError(error));
            }

            private Task<IMessage> ThrowInsteadOfResponding(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                throw new InvalidOperationException();
            }
        }
        private class TestServiceEventMismatch : IService
        {
            public IEnumerable<ServiceMethodInfo> Methods
            {
                get
                {
                    return new[]
                    {
                        new ServiceMethodInfo
                        {
                            MethodName = "TestService.RespondWithEmpty",
                            Callback = RespondWithEmpty,
                            CallbackType = ServiceCallbackType.Event
                        },
                    };
                }
            }

            private Task<IMessage> RespondWithEmpty(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                var emptyMessage = Message.FromPayload(new Bond.Void());
                return Task.FromResult<IMessage>(emptyMessage);
            }
        }
        private class TestServiceReqResMismatch : IService
        {
            public IEnumerable<ServiceMethodInfo> Methods
            {
                get
                {
                    return new[]
                    {
                        new ServiceMethodInfo
                        {
                            MethodName = "TestService.DoBeep",
                            Callback = DoBeep,
                            CallbackType = ServiceCallbackType.RequestResponse
                        },
                    };
                }
            }

            private Task DoBeep(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                return CodegenHelpers.CompletedTask;
            }
        }
        private class TestServiceUnsupported : IService
        {
            public IEnumerable<ServiceMethodInfo> Methods
            {
                get
                {
                    return new[]
                    {
                        new ServiceMethodInfo
                        {
                            MethodName = "TestService.DoBeep",
                            Callback = DoBeep,
                            CallbackType = (ServiceCallbackType)(-100)
                        },
                    };
                }
            }

            private Task DoBeep(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                return CodegenHelpers.CompletedTask;
            }
        }
    }
}
