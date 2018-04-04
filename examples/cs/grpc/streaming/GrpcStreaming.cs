// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcStreaming
{
    using System;
    using System.Collections.Generic;
    using System.Threading.Tasks;
    using Bond;
    using Bond.Grpc;
    using Grpc.Core;

    public static class GrpcStreaming
    {
        const string Address = "127.0.0.1";
        const int Port = 50051;

        public static void Main()
        {
            var server = new Server
            {
                Services =
                {
                    StreamingMethods.BindService(new StreamingService())
                },
                Ports = { new ServerPort(Address, Port, ServerCredentials.Insecure) }
            };
            server.Start();

            var channel = new Channel(Address, Port, ChannelCredentials.Insecure);
            var client = new StreamingMethods.StreamingMethodsClient(channel);

            MakeSumRequestAsync(client).Wait();
            MakeCountdownRequestAsync(client).Wait();
            MakeDuplexRequestAsync(client).Wait();

            Task.WaitAll(channel.ShutdownAsync(), server.ShutdownAsync());

            Console.WriteLine("\nDone with all requests.");
        }

        private static async Task MakeSumRequestAsync(StreamingMethods.StreamingMethodsClient client)
        {
            Console.WriteLine("Starting Sum");

            using (var clientStream = client.SumAsync())
            {

                for (uint i = 1; i <= 5; ++i)
                {

                    await clientStream.RequestStream.WriteAsync(Message.From(Box.Create(i)));
                }

                await clientStream.RequestStream.CompleteAsync();

                var response = await clientStream.ResponseAsync;
                Box<uint> result = response.Payload.Deserialize();
                if (result.value != 15)
                {
                    throw new Exception($"Expected '15' but got '{result.value}'");
                }

                Console.WriteLine($"sum: correct response {result.value}");
            }

            Console.WriteLine("Sum done");
            Console.WriteLine();
        }

        private static async Task MakeCountdownRequestAsync(StreamingMethods.StreamingMethodsClient client)
        {
            Console.WriteLine("Starting Countdown");

            using (var responseStream = client.CountdownAsync(Box.Create(7u)))
            {

                while (await responseStream.ResponseStream.MoveNext())
                {
                    uint response = responseStream.ResponseStream.Current.Payload.Deserialize().value;
                    Console.WriteLine(response);
                }
            }

            Console.WriteLine("Countdown done");
            Console.WriteLine();
        }

        private static async Task MakeDuplexRequestAsync(StreamingMethods.StreamingMethodsClient client)
        {
            Console.WriteLine("Starting Reverse");

            using (var stream = client.ReverseAsync())
            {
                foreach (var s in new[] { "how", "now", "brown", "cow"})
                {
                    await stream.RequestStream.WriteAsync(Message.From(Box.Create(s)));

                    if (await stream.ResponseStream.MoveNext())
                    {
                        var resp = stream.ResponseStream.Current.Payload.Deserialize().value;
                        Console.WriteLine($"For '{s}' got '{resp}'");
                    }
                }

                await stream.RequestStream.CompleteAsync();

                while (await stream.ResponseStream.MoveNext())
                {
                    var resp = stream.ResponseStream.Current.Payload.Deserialize().value;
                    Console.WriteLine($"Extra: '{resp}'");
                }
            }

            Console.WriteLine("Reverse done");
            Console.WriteLine();
        }
    }
}
