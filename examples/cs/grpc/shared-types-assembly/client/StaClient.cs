// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.SharedTypes
{
    using System;
    using Grpc.Core;

    public static class Client
    {
        const string Address = "127.0.0.1";
        const int Port = 50051;

        public static void Main()
        {
            var channel = new Channel(Address, Port, ChannelCredentials.Insecure);
            var client = new Calc.CalcClient(channel);

            var request = new Request
            {
                Num1 = 40,
                Num2 = 2
            };

            var response = client.AddAsync(request).GetAwaiter().GetResult();

            Console.WriteLine($"Addition result: {response.Payload.Deserialize().Result}");

            channel.ShutdownAsync().GetAwaiter().GetResult();
        }
    }
}
