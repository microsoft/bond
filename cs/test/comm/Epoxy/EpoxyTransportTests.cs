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
    using NUnit.Framework;
    using UnitTest.Comm;

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
        public void Builder_SetUnhandledExceptionHandler_Null_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new EpoxyTransportBuilder().SetUnhandledExceptionHandler(null));
        }

        [Test]
        public void Builder_Construct_DidntSetUnhandledExceptionHandler_Throws()
        {
            var builder = new EpoxyTransportBuilder();
            Assert.Throws<InvalidOperationException>(() => builder.Construct());
        }

        [Test]
        public void Construct_InvalidArgs_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new EpoxyTransport(null, null));

            LayerStack<Dummy> layerStack = new LayerStack<Dummy>(null, new TestLayer_IntValue(0));
            Assert.Throws<ArgumentNullException>(() => new EpoxyTransport(null, layerStack));
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
            Assert.That(error.message, Is.StringContaining(TestService.ExpectedExceptionMessage));

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse()
        {
            TestClientServer<ReqRespService> testClientServer = await SetupTestClientServer<ReqRespService>();
            var proxy = new ReqRespProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.MethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_LayerData()
        {
            LayerStack<Dummy> layerStack = new LayerStack<Dummy>(null, new TestLayer_IntValue(1234));
            TestClientServer<ReqRespService> testClientServer = await SetupTestClientServer<ReqRespService>(layerStack, layerStack);
            var proxy = new ReqRespProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.MethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_ClientLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            LayerStack<Dummy> clientLayerStack = new LayerStack<Dummy>(null, errorLayer);
            TestClientServer<ReqRespService> testClientServer = await SetupTestClientServer<ReqRespService>(null, clientLayerStack);
            var proxy = new ReqRespProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Request, true, false);
            IMessage<Dummy> response = await proxy.MethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(1, response.Error.Deserialize().error_code);

            errorLayer.SetState(MessageType.Response, false, true);
            response = await proxy.MethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(2, response.Error.Deserialize().error_code);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }

        [Test]
        public async Task GeneratedService_GeneratedProxy_PayloadResponse_ServerLayerErrors()
        {
            var errorLayer = new TestLayer_ReturnErrors();
            LayerStack<Dummy> serverLayerStack = new LayerStack<Dummy>(null, errorLayer);
            TestClientServer<ReqRespService> testClientServer = await SetupTestClientServer<ReqRespService>(serverLayerStack, null);
            var proxy = new ReqRespProxy<EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };

            errorLayer.SetState(MessageType.Request, errorOnSend: false, errorOnReceive: true);
            IMessage<Dummy> response = await proxy.MethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(2, response.Error.Deserialize().error_code);

            errorLayer.SetState(MessageType.Response, errorOnSend: true, errorOnReceive: false);
            response = await proxy.MethodAsync(request);
            Assert.IsTrue(response.IsError);
            Assert.AreEqual(1, response.Error.Deserialize().error_code);

            await testClientServer.ServiceTransport.StopAsync();
            await testClientServer.ClientTransport.StopAsync();
        }


        [Test]
        public async Task GeneratedGenericService_GeneratedGenericProxy_PayloadResponse()
        {
            TestClientServer<GenericReqRespService> testClientServer = await SetupTestClientServer<GenericReqRespService>();
            var proxy = new GenericReqRespProxy<Dummy, EpoxyConnection>(testClientServer.ClientConnection);
            var request = new Dummy { int_value = 100 };
            IMessage<Dummy> response = await proxy.MethodAsync(request);

            Assert.IsFalse(response.IsError);
            Assert.AreEqual(101, response.Payload.Deserialize().int_value);

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

        class TestLayer_IntValue : ILayer<Dummy>
        {
            readonly int expectedValue;

            public TestLayer_IntValue(int value)
            {
                expectedValue = value;
            }

            public Error OnSend(MessageType messageType, SendContext context, Dummy layerData)
            {
                layerData.int_value = expectedValue;
                return null;
            }

            public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData)
            {
                if (layerData.int_value != expectedValue)
                {
                    throw new ArgumentException(String.Format("Bad layer data: expected {0}, got {1}",
                                                              expectedValue, layerData.int_value));
                }
                return null;
            }
        }

        class TestLayer_ReturnErrors : ILayer<Dummy>
        {
            MessageType badMessageType;
            bool errorOnSend;
            bool errorOnReceive;

            public void SetState(MessageType badMessageType, bool errorOnSend, bool errorOnReceive)
            {
                this.badMessageType = badMessageType;
                this.errorOnSend = errorOnSend;
                this.errorOnReceive = errorOnReceive;
            }

            public Error OnSend(MessageType messageType, SendContext context, Dummy layerData)
            {
                if (errorOnSend && messageType == badMessageType)
                {
                    return new Error { error_code = 1, message = String.Format("Send error {0}", messageType.ToString()) };
                }
                else
                {
                    return null;
                }
            }

            public Error OnReceive(MessageType messageType, ReceiveContext context, Dummy layerData)
            {
                if (errorOnReceive && messageType == badMessageType)
                {
                    return new Error { error_code = 2, message = String.Format("Receive error {0}", messageType.ToString()) };
                }
                else
                {
                    return null;
                }
            }
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
                // some tests rely on the use of DebugExceptionHandler to assert things about the error message
                .SetUnhandledExceptionHandler(Transport.DebugExceptionHandler)
                .SetLayerStack(serviceLayerStack)
                .Construct();
            EpoxyListener listener = serviceTransport.MakeListener(new IPEndPoint(IPAddress.Loopback, 0));
            listener.AddService(testService);
            await listener.StartAsync();

            EpoxyTransport clientTransport = new EpoxyTransportBuilder()
                // some tests rely on the use of DebugExceptionHandler to assert things about the error message
                .SetUnhandledExceptionHandler(Transport.DebugExceptionHandler)
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
            public const string ExpectedExceptionMessage = "This method is expected to throw.";

            private int m_respondWithEmpty_callCount = 0;

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
                    return m_respondWithEmpty_callCount;
                }
            }

            private Task<IMessage> RespondWithEmpty(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                Interlocked.Increment(ref m_respondWithEmpty_callCount);
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
                throw new InvalidOperationException(ExpectedExceptionMessage);
            }
        }

        private class ReqRespService : ReqRespServiceBase
        {
            public override Task<IMessage<Dummy>> MethodAsync(IMessage<Dummy> param, CancellationToken ct)
            {
                var request = param.Payload.Deserialize();
                var result = new Dummy { int_value = request.int_value + 1 };

                return Task.FromResult<IMessage<Dummy>>(Message.FromPayload(result));
            }
        }

        private class GenericReqRespService : GenericReqRespServiceBase<Dummy>
        {
            public override Task<IMessage<Dummy>> MethodAsync(IMessage<Dummy> param, CancellationToken ct)
            {
                var request = param.Payload.Deserialize();
                var result = new Dummy { int_value = request.int_value + 1 };

                return Task.FromResult<IMessage<Dummy>>(Message.FromPayload(result));
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
