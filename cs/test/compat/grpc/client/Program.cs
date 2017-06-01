// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace PingPongClient
{
    using System;
    using System.Threading;
    using System.Threading.Tasks;

    using Bond.Grpc;
    using Grpc.Core;

    using PingPongNS;

    public class Program
    {

        private static async Task<IMessage<PingResponse>> DoPingPongAsync(PingPong.PingPongClient client, PingRequest request)
        {
            Console.Out.WriteLine("Sending request");
            Console.Out.Flush();

            IMessage<PingResponse> response = await client.PingAsync(request);

            return response;
        }


        private static void DoPingPongEventAsync(PingPong.PingPongClient client, PingRequest request)
        {
            Console.Out.WriteLine("Sending event");
            Console.Out.Flush();

            client.PingEventAsync(request);
        }

        static void Main(string[] args)
        {
            Thread.Sleep(1000);

            Console.Out.WriteLine("Start client");
            Console.Out.Flush();

            try
            {
                Channel pingChannel = new Channel("127.0.0.1", (int)PingConstants.Port, ChannelCredentials.Insecure);
                var pingClient = new PingPong.PingPongClient(pingChannel);

                for (int i = 0; i < (int)PingConstants.NumRequests; i++)
                {
                    var request = new PingRequest { Payload = "request" + i, Action = PingAction.Identity };
                    var message = DoPingPongAsync(pingClient, request).GetAwaiter().GetResult();
                    if (message.Payload.Deserialize().Payload != request.Payload)
                    {
                        Console.Out.WriteLine("Response payload did not match request");
                        Console.Out.WriteLine("Client failed");
                        Console.Out.Flush();
                        return;
                    }
                }

                for (int i = 0; i < (int)PingConstants.NumEvents; i++)
                {
                    var request = new PingRequest { Payload = "event" + i, Action = PingAction.Identity };
                    DoPingPongEventAsync(pingClient, request);
                }

                for (int i = 0; i < (int)PingConstants.NumErrors; i++)
                {
                    var request = new PingRequest { Payload = "error" + i, Action = PingAction.Error };
                    try
                    {
                        var message = DoPingPongAsync(pingClient, request).GetAwaiter().GetResult();
                        Console.Out.WriteLine("Non-error response received: " + message.Payload.Deserialize().Payload);
                        Console.Out.WriteLine("Client failed");
                        Console.Out.Flush();
                        return;
                    }
                    catch (Exception ex)
                    {
                        Console.Out.WriteLine("Error caught: " + ex.Message);
                        Console.Out.Flush();
                    }
                }
            }
            catch (Exception ex)
            {
                Console.Out.WriteLine("Exception caught: " + ex.Message);
                Console.Out.Flush();
                return;
            }

            Console.Out.WriteLine("Client succeeded");
            Console.Out.Flush();
        }
    }
}
