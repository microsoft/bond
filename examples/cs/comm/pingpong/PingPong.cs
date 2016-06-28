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
        private const string PingEndpoint = "epoxy://127.0.0.1";
        private static readonly string reversePingEndpoint = $"epoxy://127.0.0.1:{EpoxyTransport.DefaultPort + 1}";

        private static EpoxyConnection pingConnection;
        private static EpoxyConnection reverseConnection;

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
            var transport = new EpoxyTransportBuilder()
                .SetLogSink(new ConsoleLogger())
                .SetMetricsSink(new ConsoleMetricsSink())
                .Construct();

            var pingPongService = new PingPongService();
            EpoxyListener pingPongListener = (EpoxyListener) transport.MakeListener(PingEndpoint);
            pingPongListener.AddService(pingPongService);

            var reversePingPongService = new ReversePingPongService();
            EpoxyListener reversePingPongListener = (EpoxyListener) transport.MakeListener(reversePingEndpoint);
            reversePingPongListener.AddService(reversePingPongService);

            await Task.WhenAll(
                pingPongListener.StartAsync(),
                reversePingPongListener.StartAsync());

            pingConnection = (EpoxyConnection) await transport.ConnectToAsync(PingEndpoint);
            reverseConnection = (EpoxyConnection) await transport.ConnectToAsync(reversePingEndpoint);

            return transport;
        }

        private static void Shutdown(EpoxyTransport transport)
        {
            Task.WaitAll(transport.StopAsync(), pingConnection.StopAsync(), reverseConnection.StopAsync());
        }

        private static Task[] MakeRequestsAndPrintAsync(int numRequests)
        {
            var pingPongProxy = new PingPongProxy<EpoxyConnection>(pingConnection);
            var reversePingPongProxy = new PingPongProxy<EpoxyConnection>(reverseConnection);

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
