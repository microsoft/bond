// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcGenerics
{
    using System;
    using System.Collections.Generic;
    using System.Threading.Tasks;

    using Bond;
    using Bond.Grpc;
    using Grpc.Core;

    public static class GrpcGenerics
    {
        const string Address = "127.0.0.1";
        const int Int32Port = 50051;
        const int StringPort = 50052;

        public static void Main()
        {
            // As of the current version of Bond-over-gRPC, all
            // instantiations of a generic service have the same service
            // name, precluding them from being hosted by the same Server.
            // We, therefore, host the int32 and the string instantiations
            // in different Servers on different ports.
            var int32Server = new Server
            {
                Services =
                {
                    Inverter<Box<int>>.BindService(new Int32Inverter()),
                },
                Ports = { new ServerPort(Address, Int32Port, ServerCredentials.Insecure) }
            };
            int32Server.Start();

            var int32Channel = new Channel(Address, Int32Port, ChannelCredentials.Insecure);
            var int32InverterClient = new Inverter<Box<Int32>>.InverterClient(int32Channel);

            var stringServer = new Server
            {
                Services =
                {
                    Inverter<Box<string>>.BindService(new StringInverter()),
                },
                Ports = { new ServerPort(Address, StringPort, ServerCredentials.Insecure) }
            };
            stringServer.Start();

            var stringChannel = new Channel(Address, StringPort, ChannelCredentials.Insecure);
            var stringInverterClient = new Inverter<Box<string>>.InverterClient(stringChannel);

            MakeInvertRequestAsync(int32InverterClient, 1, 2, 3, 4, 5).Wait();
            MakeInvertRequestAsync(stringInverterClient, "abc", "def", "ghi").Wait();

            Task.WaitAll(
                int32Channel.ShutdownAsync(),
                int32Server.ShutdownAsync(),
                stringChannel.ShutdownAsync(),
                stringServer.ShutdownAsync());

            Console.WriteLine("\nDone with all requests.");
        }

        private static async Task MakeInvertRequestAsync<T>(Inverter<Box<T>>.InverterClient client, params T[] requests)
        {
            Console.WriteLine($"Starting Invert for type {typeof(T).Name}.");

            using (var clientStream = client.InvertAsync())
            {
                foreach (var request in requests)
                {
                    await clientStream.RequestStream.WriteAsync(Message.From(Box.Create(request)));
                    if (await clientStream.ResponseStream.MoveNext())
                    {
                        var result = clientStream.ResponseStream.Current.Payload.Deserialize().value;
                        Console.WriteLine($"{request} inverted is {result}");
                    }
                    else
                    {
                        throw new Exception($"Unexpected end of response stream after request {request}");
                    }
                }
            }

            Console.WriteLine($"Invert for type {typeof(T).Name} done.");
            Console.WriteLine();
        }
    }
}
