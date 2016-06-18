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
    using Bond.Comm.Epoxy;
    using Bond.Examples.Logging;
    using Bond.Examples.Metrics;

    public static class PingPong
    {
        private const ushort pingPort = EpoxyTransport.DefaultPort;
        private const ushort reversePingPort = EpoxyTransport.DefaultPort + 1;

        private static EpoxyConnection s_pingConnection;
        private static EpoxyConnection s_reverseConnection;

        public static void Main()
        {
            var transport = SetupAsync().Result;

            var tasks = MakeRequestsAndPrintAsync(5);

            Task.WaitAll(tasks);

            Shutdown(transport);

            Console.WriteLine("Done with all requests.");
            Console.ReadLine();
        }

        private async static Task<EpoxyTransport> SetupAsync()
        {
            var metricsHandler = new ConsoleMetricsHandler();
            Metrics.SetHandler(metricsHandler);

            var transport = new EpoxyTransportBuilder()
                .SetLogSink(new ConsoleLogger())
                .Construct();

            var pingEndpoint = new IPEndPoint(IPAddress.Loopback, pingPort);
            var reversePingEndpoint = new IPEndPoint(IPAddress.Loopback, reversePingPort);

            var pingPongService = new PingPongService();
            EpoxyListener pingPongListener = transport.MakeListener(pingEndpoint);
            pingPongListener.AddService(pingPongService);

            var reversePingPongService = new ReversePingPongService();
            EpoxyListener reversePingPongListener = transport.MakeListener(reversePingEndpoint);
            reversePingPongListener.AddService(reversePingPongService);

            await Task.WhenAll(
                pingPongListener.StartAsync(),
                reversePingPongListener.StartAsync());

            s_pingConnection = await transport.ConnectToAsync(pingPongListener.ListenEndpoint, CancellationToken.None);
            s_reverseConnection = await transport.ConnectToAsync(reversePingPongListener.ListenEndpoint, CancellationToken.None);

            return transport;
        }

        private static void Shutdown(Transport transport)
        {
            Task.WaitAll(transport.StopAsync(), s_pingConnection.StopAsync(), s_reverseConnection.StopAsync());
        }

        private static Task[] MakeRequestsAndPrintAsync(int numRequests)
        {
            var pingPongProxy = new PingPongProxy<EpoxyConnection>(s_pingConnection);
            var reversePingPongProxy = new PingPongProxy<EpoxyConnection>(s_reverseConnection);

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

        private static async Task DoPingPong(PingPongProxy<EpoxyConnection> proxy, int requestNum, string payload, UInt16 delay)
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
