// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace PingPongServer
{
    using System;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;

    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.Comm.Layers;

    using PingPongNS;

    public class PingPongService : PingPongServiceBase
    {
        const int NumRequestsExpected = 10;
        const int NumEventsExpected = 9;
        const int NumErrorsExpected = 8;
        const int NumThrowsExpected = 7;
        static int NumRequests = 0;
        static int NumEvents = 0;
        static int NumErrors = 0;

        public override Task<IMessage<PingResponse>> PingAsync(IMessage<PingRequest> param, CancellationToken ct)
        {
            PingRequest request = param.Payload.Deserialize();

            IMessage<PingResponse> message = null;

            switch (request.Action)
            {
                case PingAction.Identity:
                    Console.Out.WriteLine($"Received mirror request \"{request.Payload}\"");
                    Console.Out.Flush();

                    var response = new PingResponse { Payload = request.Payload };
                    message = Message.FromPayload(response);
                    NumRequests++;
                    break;

                case PingAction.Error:
                    Console.Out.WriteLine($"Received error request \"{request.Payload}\"");
                    Console.Out.Flush();

                    var error = new Error { error_code = 1234, message = request.Payload };
                    message = Message.FromError<PingResponse>(error);
                    NumErrors++;
                    break;

                default:
                    throw new NotImplementedException("Unknown PingAction");
            }

            return Task.FromResult(message);
        }

        public override void PingEventAsync(IMessage<PingRequest> param)
        {
            PingRequest request = param.Payload.Deserialize();

            Console.Out.WriteLine($"Received event \"{request.Payload}\"");
            Console.Out.Flush();

            NumEvents++;
        }

        private static async Task SetupAsync(ILayerStackProvider layerStackProvider)
        {
            var endpoint = new IPEndPoint(IPAddress.Loopback, 25188);
            EpoxyTransport transport = new EpoxyTransportBuilder().SetLayerStackProvider(layerStackProvider).Construct();
            EpoxyListener pingPongListener = transport.MakeListener(endpoint);

            var pingPongService = new PingPongService();
            pingPongListener.AddService(pingPongService);

            await pingPongListener.StartAsync();
        }

        static void Main(string[] args)
        {
            var layer1 = new TestLayer(1);
            var layer2 = new TestLayer(2);
            var layerStackProvider = new LayerStackProvider<PingLayerData>(layer1, layer2);

            SetupAsync(layerStackProvider).GetAwaiter().GetResult();

            Console.Out.WriteLine("Server ready");
            Console.Out.Flush();

            Thread.Sleep(3000);

            if ((NumRequests != NumRequestsExpected) ||
                (NumEvents != NumEventsExpected) ||
                (NumErrors != NumErrorsExpected))
            {
                Console.Out.WriteLine("Server failed: Did not receive all expected messages");
                Console.Out.Flush();
                return;
            }

            if ((layer1.NumReached == 0) ||
                (layer1.NumError != 0) ||
                (layer2.NumReached == 0) ||
                (layer2.NumError != 0))
            {
                Console.Out.WriteLine("Server failed: Problem with layers");
                Console.Out.Flush();
                return;
            }

            Console.Out.WriteLine("Server completed");
            Console.Out.Flush();
            }
    }
}
