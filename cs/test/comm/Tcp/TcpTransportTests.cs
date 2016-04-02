// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Tcp
{
    using System;
    using System.Collections.Generic;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Tcp;
    using NUnit.Framework;

    [TestFixture]
    public class TcpTransportTests
    {
        private const string AnyIpAddressString = "10.1.2.3";
        private const int AnyPort = 12345;
        private static readonly IPAddress AnyIpAddress = new IPAddress(new byte[] { 10, 1, 2, 3 });
        private static readonly Message<Bond.Void> EmptyMessage = new Message<Bond.Void>(new Bond.Void());

        [Test]
        public void DefaultPorts_AreExpected()
        {
            Assert.AreEqual(25188, TcpTransport.DefaultPort);
        }

        [Test]
        public void ParseStringAddress_NullOrEmpty_Throws()
        {
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress(null));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress(string.Empty));
        }

        [Test]
        public void ParseStringAddress_ValidIpNoPort_ReturnsIpEndpoint()
        {
            var result = TcpTransport.ParseStringAddress(AnyIpAddressString);
            Assert.AreEqual(new IPEndPoint(AnyIpAddress, TcpTransport.DefaultPort), result);
        }

        [Test]
        public void ParseStringAddress_ValidIpWithPort_ReturnsIpEndpoint()
        {
            var result = TcpTransport.ParseStringAddress(AnyIpAddressString + ":" + AnyPort);
            Assert.AreEqual(new IPEndPoint(AnyIpAddress, AnyPort), result);
        }

        [Test]
        public void ParseStringAddress_InvalidIpAddress_Throws()
        {
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("not an ip"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("not an ip:12345"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("not an ip:no a port"));
        }

        [Test]
        public void ParseStringAddress_InvalidPortAddress_Throws()
        {
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("10.1.2.3:"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("10.1.2.3::"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("10.1.2.3:not a port"));
        }

        [Test]
        public void Builder_SetUnhandledExceptionHandler_Null_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new TcpTransportBuilder().SetUnhandledExceptionHandler(null));
        }

        [Test]
        public void Builder_Construct_DidntSetUnhandledExceptionHandler_Throws()
        {
            var builder = new TcpTransportBuilder();
            Assert.Throws<InvalidOperationException>(() => builder.Construct());
        }

        [Test]
        public void Construct_InvalidArgs_Throws()
        {
            Assert.Throws<ArgumentNullException>(() => new TcpTransport(null));
        }

        [Test]
        public async Task SetupListener_RequestReply_PayloadResponse()
        {
            TestClientServer testClientServer = await SetupTestClientServer();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService.RespondWithEmpty", EmptyMessage, CancellationToken.None);

            Assert.IsFalse(response.IsError);
            Assert.IsNotNull(response.Payload);
            Assert.IsNull(response.Error);

            Assert.AreEqual(1, testClientServer.Service.RespondWithEmpty_CallCount);

            await testClientServer.Transport.StopAsync();
        }

        [Test]
        public async Task SetupListener_RequestError_ErrorResponse()
        {
            TestClientServer testClientServer = await SetupTestClientServer();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService.RespondWithError", EmptyMessage, CancellationToken.None);

            Assert.IsTrue(response.IsError);
            Assert.IsNotNull(response.Error);

            var error = response.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);

            await testClientServer.Transport.StopAsync();
        }

        [Test]
        public async Task SetupListenerWithErrorHandler_RequestThatThrows_ErrorResponse()
        {
            TestClientServer testClientServer = await SetupTestClientServer();

            var response = await testClientServer.ClientConnection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService.ThrowInsteadOfResponding", EmptyMessage, CancellationToken.None);

            Assert.IsTrue(response.IsError);
            Assert.IsNotNull(response.Error);

            var error = response.Error.Deserialize<Error>();
            Assert.AreEqual((int)ErrorCode.InternalServerError, error.error_code);
            Assert.That(error.message, Is.StringContaining(TestService.ExpectedExceptionMessage));

            await testClientServer.Transport.StopAsync();
        }


        private class TestClientServer
        {
            public TestService Service;
            public TcpTransport Transport;
            public TcpListener Listener;
            public TcpConnection ClientConnection;
        }

        private static async Task<TestClientServer> SetupTestClientServer()
        {
            var testService = new TestService();

            TcpTransport transport = new TcpTransportBuilder()
                // some tests rely on the use of DebugExceptionHandler to assert things about the error message
                .SetUnhandledExceptionHandler(Transport.DebugExceptionHandler)
                .Construct();
            TcpListener listener = transport.MakeListener(new IPEndPoint(IPAddress.Loopback, 0));
            listener.AddService(testService);
            await listener.StartAsync();

            TcpConnection clientConnection = await transport.ConnectToAsync(listener.ListenEndpoint);

            return new TestClientServer
            {
                Service = testService,
                Transport = transport,
                Listener = listener,
                ClientConnection = clientConnection,
            };
        }

        private class TestService : IService
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
    }
}
