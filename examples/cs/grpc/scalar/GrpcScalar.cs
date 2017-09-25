// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.GrpcScalar
{
    using System;
    using System.Collections.Generic;
    using System.Threading.Tasks;
    using Bond;
    using Bond.Grpc;
    using Grpc.Core;

    public static class GrpcPingPong
    {
        const string Address = "127.0.0.1";
        const int Port = 50051;

        public static void Main()
        {
            var server = new Server
            {
                Services =
                {
                    ScalarMethods.BindService(new ScalarService())
                },
                Ports = { new ServerPort(Address, Port, ServerCredentials.Insecure) }
            };
            server.Start();

            var channel = new Channel(Address, Port, ChannelCredentials.Insecure);
            var client = new ScalarMethods.ScalarMethodsClient(channel);

            var negateTask = MakeNegateRequestAsync(client);
            var sumTask = MakeSumRequestAsync(client);

            Task.WaitAll(negateTask, sumTask);

            Task.WaitAll(channel.ShutdownAsync(), server.ShutdownAsync());

            Console.WriteLine("\n\n\nDone with all requests.");
        }

        private static async Task MakeNegateRequestAsync(ScalarMethods.ScalarMethodsClient client)
        {
            IMessage<Box<int>> response = await client.NegateAsync(Box.Create(10));
            Box<int> result = response.Payload.Deserialize();
            if (result.value != -10)
            {
                throw new Exception($"Expected '-10' but got '{result.value}'");
            }

            Console.WriteLine($"negate: correct response {result.value}");
        }

        private static async Task MakeSumRequestAsync(ScalarMethods.ScalarMethodsClient client)
        {
            IMessage<Box<ulong>> response = await client.SumAsync(Box.Create(new List<ulong> {1, 2, 3, 4, 5}));
            Box<ulong> result = response.Payload.Deserialize();
            if (result.value != 15)
            {
                throw new Exception($"Expected '15' but got '{result.value}'");
            }

            Console.WriteLine($"sum: correct response {result.value}");
        }
    }
}
