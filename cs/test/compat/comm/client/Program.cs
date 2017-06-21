// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace PingPongClient
{
    using System;
    using System.Diagnostics;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;

    using Bond.Comm;
    using Bond.Comm.Layers;
    using Bond.Comm.Epoxy;

    using PingPongNS;

    public class Program
    {
        private static async Task<PingPongProxy<EpoxyConnection>> SetupProxyAsync(ILayerStackProvider layerStack)
        {
            var transport = new EpoxyTransportBuilder().SetLayerStackProvider(layerStack).Construct();

            var endpoint = new EpoxyTransport.Endpoint(IPAddress.Loopback.ToString(), 25188, false);
            var conn = await transport.ConnectToAsync(endpoint, CancellationToken.None);

            return new PingPongProxy<EpoxyConnection>(conn);
        }

        private static async Task<IMessage<PingResponse>> DoPingPongAsync(PingPongProxy<EpoxyConnection> proxy, PingRequest request)
        {
            Console.Out.WriteLine("Sending request");
            Console.Out.Flush();

            IMessage<PingResponse> message = await proxy.PingAsync(request);
            return message;
        }

        private static void DoPingEvent(PingPongProxy<EpoxyConnection> proxy, PingRequest theEvent)
        {
            Console.Out.WriteLine("Sending event");
            Console.Out.Flush();

            proxy.PingEventAsync(theEvent);
        }

        static void Main(string[] args)
        {
            Thread.Sleep(1000);

            Console.Out.WriteLine("Start client");
            Console.Out.Flush();

            var layer1 = new TestLayer(1);
            var layer2 = new TestLayer(2);
            var layerStackProvider = new LayerStackProvider<PingLayerData>(layer1, layer2);

            try
            {
                var proxy = SetupProxyAsync(layerStackProvider).GetAwaiter().GetResult();

                for (int i = 0; i < (int)PingConstants.NumRequests; i++)
                {
                    var request = new PingRequest { Payload = "request" + i, Action = PingAction.Identity };
                    var message = DoPingPongAsync(proxy, request).GetAwaiter().GetResult();
                    if (message.IsError)
                    {
                        Console.Out.WriteLine("Error response received: " + message.Error.Deserialize().message);
                        Console.Out.WriteLine("Client failed");
                        Console.Out.Flush();
                        return;
                    }
                    else
                    {
                        string msg = message.Payload.Deserialize().Payload;
                        if (msg != request.Payload)
                        {
                            Console.Out.WriteLine($"Response message did not match request payload: expected \"{request.Payload}\", got \"{msg}\"");
                            Console.Out.WriteLine("Client failed");
                            Console.Out.Flush();
                            return;
                        }
                    }
                }

                for (int i = 0; i < (int)PingConstants.NumEvents; i++)
                {
                    var request = new PingRequest { Payload = "event" + i };
                    DoPingEvent(proxy, request);
                }

                for (int i = 0; i < (int)PingConstants.NumErrors; i++)
                {
                    var request = new PingRequest { Payload = "error" + i, Action = PingAction.Error };
                    var message = DoPingPongAsync(proxy, request).GetAwaiter().GetResult();
                    if (!message.IsError)
                    {
                        Console.Out.WriteLine("Non-error response received: " + message.Payload.Deserialize().Payload);
                        Console.Out.WriteLine("Client failed");
                        Console.Out.Flush();
                        return;
                    }
                    else
                    {
                        string msg = message.Error.Deserialize().message;
                        if (msg != request.Payload)
                        {
                            Console.Out.WriteLine($"Error message did not match request payload: expected \"{request.Payload}\", got \"{msg}\"");
                            Console.Out.WriteLine("Client failed");
                            Console.Out.Flush();
                            return;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                Console.Out.WriteLine("Exception caught: " + ex.Message);
                Console.Out.Flush();
                return;
            }

            if ((layer1.NumReached == 0) ||
                (layer1.NumError != 0) ||
                (layer2.NumReached == 0) ||
                (layer2.NumError != 0))
            {
                Console.Out.WriteLine("Client failed: Problem with layers");
                Console.Out.Flush();
                return;
            }

            Console.Out.WriteLine("Client succeeded");
            Console.Out.Flush();
        }
    }
}
