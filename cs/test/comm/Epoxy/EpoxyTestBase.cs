// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information

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

    public class EpoxyTestBase
    {
        protected List<EpoxyTransport> transports = new List<EpoxyTransport>();

        [TearDown]
        public async void TearDown()
        {
            var stopTasks = new Task[transports.Count];
            int idx = 0;

            foreach (var transport in transports)
            {
                stopTasks[idx] = transport.StopAsync();
                ++idx;
            }

            var timeoutTask = Task.Delay(TimeSpan.FromSeconds(10));
            var stoppedAllTask = Task.WhenAll(stopTasks);

            var completedTask = await Task.WhenAny(timeoutTask, stoppedAllTask);

            Assert.AreNotSame(timeoutTask, completedTask, "Timed out waiting for transports to be shutdown.");
        }

        public async Task<TestClientServer<TService>> SetupTestClientServer<TService>(
            ILayerStackProvider serviceLayerStackProvider = null, ILayerStackProvider clientLayerStackProvider = null
            ) where TService : class, IService, new()
        {
            var testService = new TService();

            EpoxyTransport serviceTransport = new EpoxyTransportBuilder()
                .SetLayerStackProvider(serviceLayerStackProvider)
                .Construct();
            transports.Add(serviceTransport);

            var listener = serviceTransport.MakeListener(new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultInsecurePort));
            listener.AddService(testService);
            await listener.StartAsync();

            EpoxyTransport clientTransport = new EpoxyTransportBuilder()
                .SetLayerStackProvider(clientLayerStackProvider)
                .Construct();
            transports.Add(clientTransport);

            EpoxyConnection clientConnection = await clientTransport.ConnectToAsync("epoxy://127.0.0.1");

            return new TestClientServer<TService>
            {
                Service = testService,
                ServiceTransport = serviceTransport,
                Listener = listener,
                ClientConnection = clientConnection,
                ClientTransport = clientTransport
            };
        }

        public class TestClientServer<TService>
        {
            public TService Service;
            public EpoxyTransport ServiceTransport;
            public EpoxyListener Listener;
            public EpoxyConnection ClientConnection;
            public EpoxyTransport ClientTransport;
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
                    error_code = (int) ErrorCode.INTERNAL_SERVER_ERROR,
                };

                return Task.FromResult<IMessage>(Message.FromError(error));
            }

            private Task<IMessage> ThrowInsteadOfResponding(IMessage request, ReceiveContext context, CancellationToken ct)
            {
                throw new InvalidOperationException();
            }
        }

        protected class TestServiceEventMismatch : IService
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

        protected class TestServiceReqResMismatch : IService
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

        protected class TestServiceUnsupported : IService
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
