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
    using Bond.Examples.Logging;

    public static class PingPong
    {
        private static TcpConnection s_pingConnection;
        private static TcpConnection s_reverseConnection;

        public static void Main()
        {
            var transport = SetupAsync().Result;

            var tasks = MakeRequestsAndPrintAsync(5);

            Task.WaitAll(tasks);

            Console.WriteLine("Done with all requests.");

            // TODO: Shutdown not yet implemented.
            // transport.StopAsync().Wait();
        }

        private async static Task<TcpTransport> SetupAsync()
        {
            var handler = new ConsoleLogger();
            Log.AddHandler(handler);

            var transport = new TcpTransportBuilder()
                .SetUnhandledExceptionHandler(Transport.ToErrorExceptionHandler)
                .Construct();

            var assignAPortEndPoint = new IPEndPoint(IPAddress.Loopback, 0);

            var pingPongService = new PingPongService();
            TcpListener pingPongListener = transport.MakeListener(assignAPortEndPoint);
            pingPongListener.AddService(pingPongService);

            var reversePingPongService = new ReversePingPongService();
            TcpListener reversePingPongListener = transport.MakeListener(assignAPortEndPoint);
            reversePingPongListener.AddService(reversePingPongService);

            await Task.WhenAll(
                pingPongListener.StartAsync(),
                reversePingPongListener.StartAsync());

            s_pingConnection = await transport.ConnectToAsync(pingPongListener.ListenEndpoint, CancellationToken.None);
            s_reverseConnection = await transport.ConnectToAsync(reversePingPongListener.ListenEndpoint, CancellationToken.None);

            return transport;
        }

        private static Task[] MakeRequestsAndPrintAsync(int numRequests)
        {
            var pingPongProxy = new PingPongProxy<TcpConnection>(s_pingConnection);
            var reversePingPongProxy = new PingPongProxy<TcpConnection>(s_reverseConnection);

            var tasks = new Task[2 * numRequests];

            var rnd = new Random();

            foreach (var requestNum in Enumerable.Range(0, numRequests))
            {
                UInt16 delay = (UInt16)rnd.Next(2000);
                tasks[(2 * requestNum)] = DoPingPong(pingPongProxy, requestNum, "ping" + requestNum.ToString(), delay);

                delay = (UInt16)rnd.Next(2000);
                tasks[(2 * requestNum) + 1] = DoPingPong(reversePingPongProxy, requestNum, "gnipr" + requestNum.ToString(), delay);
            }

            return tasks;
        }

        private static async Task DoPingPong(PingPongProxy<TcpConnection> proxy, int requestNum, string payload, UInt16 delay)
        {
            var request = new PingRequest { Payload = payload, DelayMilliseconds = delay };
            IMessage<PingResponse> message = await proxy.PingAsync(request);

            if (message.IsError)
            {
                Error error = message.Error.Deserialize();
                Console.WriteLine($"Request #{requestNum} failed: {error.error_code}: {error.message}");
            }
            else
            {
                PingResponse response = message.Payload.Deserialize();
                Console.WriteLine($"Request #{requestNum} response: \"{response.Payload}\". Delay: {delay}");
            }
        }
    }
}
