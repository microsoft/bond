// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Examples.SharedTypes
{
    using System;
    using Grpc.Core;

    public static class Server
    {
        const string Address = "127.0.0.1";
        const int Port = 50051;

        public static void Main()
        {
            var server = new Grpc.Core.Server
            {
                Services =
                {
                    Calc.BindService(new CalcService())
                },
                Ports = { new ServerPort(Address, Port, ServerCredentials.Insecure) }
            };
            server.Start();

            Console.WriteLine("Press ENTER to stop the service.");
            Console.ReadLine();

            server.ShutdownAsync().GetAwaiter().GetResult();
        }
    }
}
