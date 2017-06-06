// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace PingPongServer
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;

    using Bond.Grpc;
    using Grpc.Core;

    using PingPongNS;

    public class PingPongService : PingPong.PingPongBase
    {
        static Server pingServer;

        static CountdownEvent Countdown = new CountdownEvent((int)PingConstants.NumRequests +
                                                             (int)PingConstants.NumEvents +
                                                             (int)PingConstants.NumErrors);

        static int NumRequestsReceived = 0;
        static int NumEventsReceived = 0;
        static int NumErrorsReceived = 0;

        public override Task<IMessage<PingResponse>> Ping(IMessage<PingRequest> param, ServerCallContext context)
        {
            PingRequest request = param.Payload.Deserialize();

            IMessage<PingResponse> message = null;

            switch (request.Action)
            {
                case PingAction.Identity:
                    Console.Out.WriteLine($"Received identity request \"{request.Payload}\"");
                    Console.Out.Flush();

                    var response = new PingResponse { Payload = request.Payload };
                    message = Message.From(response);
                    Interlocked.Increment(ref NumRequestsReceived);
                    Countdown.Signal();
                    break;

                case PingAction.Error:
                    Console.Out.WriteLine($"Received error request \"{request.Payload}\"");
                    Console.Out.Flush();
                    Interlocked.Increment(ref NumErrorsReceived);
                    Countdown.Signal();
                    throw new ApplicationException("Application Exception");

                default:
                    Countdown.Signal();
                    throw new NotImplementedException("Unknown PingAction");
            }

            return Task.FromResult(message);
        }

        public override Task PingEvent(IMessage<PingRequest> param, ServerCallContext context)
        {
            PingRequest request = param.Payload.Deserialize();

            Console.Out.WriteLine($"Received event \"{request.Payload}\"");
            Console.Out.Flush();

            Interlocked.Increment(ref NumEventsReceived);
            Countdown.Signal();

            return Task.FromResult(false);
        }


        private static void Setup()
        {
            pingServer = new Server
            {
                Services =
                {
                    PingPong.BindService(new PingPongService()),
                },
                Ports = { new ServerPort("127.0.0.1", (int)PingConstants.Port, ServerCredentials.Insecure) }
            };
            pingServer.Start();
        }

        static void Main(string[] args)
        {
            Setup();

            Console.Out.WriteLine("Server ready");
            Console.Out.Flush();

            bool countdownSet = Countdown.Wait(TimeSpan.FromSeconds(30));
            bool didShutdown = pingServer.ShutdownAsync().Wait(TimeSpan.FromSeconds(10));

            if (!didShutdown)
            {
                Console.Out.WriteLine("Server failed: Didn't shutdown in time");
                Console.Out.Flush();
                return;
            }
            else if (
                !countdownSet ||
                (NumRequestsReceived != (int)PingConstants.NumRequests) ||
                (NumEventsReceived != (int)PingConstants.NumEvents) ||
                (NumErrorsReceived != (int)PingConstants.NumErrors))
            {
                Console.Out.WriteLine("Server failed: Did not receive all expected messages");
                Console.Out.Flush();
                return;
            }

            Console.Out.WriteLine("Server completed");
            Console.Out.Flush();
        }
    }
}
