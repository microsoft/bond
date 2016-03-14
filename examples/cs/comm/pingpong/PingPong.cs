// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.PingPong
{
    using System;
    using System.Linq;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;

    using Bond.Comm;
    using Bond.Comm.Tcp;

    public static class PingPong
    {
        private static TcpConnection s_connection;

        public static void Main()
        {
            var transport = SetupAsync().Result;

            var tasks = MakeRequestsAndPrintAsync(5);

            Task.WaitAll(tasks);

            Console.WriteLine("Done with all requests.");

            // TODO: Shutdown not yet implemented.
            // transport.StopAsync().Wait();

            Console.ReadLine();
        }

        private async static Task<TcpTransport> SetupAsync()
        {
            var pingPongService = new PingPongService();
            var reversePingPongService = new ReversePingPongService();

            var listenEndpoint = new IPEndPoint(IPAddress.Loopback, 0);

            var transport = new TcpTransportBuilder().Construct();

            var listener = transport.MakeListener(listenEndpoint);
            listener.AddService(pingPongService);
            listener.AddService(reversePingPongService);
            await listener.StartAsync();

            s_connection = await transport.ConnectToAsync(listener.ListenEndpoint, CancellationToken.None);

            return transport;
        }

        private static Task[] MakeRequestsAndPrintAsync(int numRequests)
        {
            var tasks = new Task[2 * numRequests];

            var rnd = new Random();

            foreach (var requestNum in Enumerable.Range(0, numRequests))
            {
                UInt16 delay = (UInt16)rnd.Next(2000);
                tasks[(2 * requestNum)] = DoPingPong(s_connection, requestNum, "ping" + requestNum.ToString(), delay);

                delay = (UInt16)rnd.Next(2000);
                tasks[(2 * requestNum) + 1] = DoReversePingPong(s_connection, requestNum, "gnipr" + requestNum.ToString(), delay);
            }

            return tasks;
        }

        private static async Task DoPingPong(IRequestResponseConnection connection, int requestNum, string message, UInt16 delay)
        {
            var proxy = new Proxy_PingPong(connection);

            var request = new PingRequest { Payload = message, DelayMilliseconds = delay };
            var response = await proxy.PingAsync(request);

            Console.WriteLine($"P Request #{requestNum} response: \"{response.Payload}\". Delay: {delay}");
        }

        private static async Task DoReversePingPong(IRequestResponseConnection connection, int requestNum, string message, UInt16 delay)
        {
            var proxy = new Proxy_ReversePingPong(connection);

            var request = new PingRequest { Payload = message, DelayMilliseconds = delay };
            var response = await proxy.PingAsync(request);

            Console.WriteLine($"R Request #{requestNum} response: \"{response.Payload}\". Delay: {delay}");
        }
    }
}
