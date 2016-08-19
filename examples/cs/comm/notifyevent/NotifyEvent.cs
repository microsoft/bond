// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.NotifyEvent
{
    using System;
    using System.Linq;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.Examples.Logging;

    public static class NotifyEvent
    {
        private static EpoxyConnection s_connection;

        public static void Main()
        {
            var transport = SetupAsync().Result;

            MakeRequestsAndPrint(5);

            Console.WriteLine("Done with all requests.");

            // TODO: Shutdown not yet implemented.
            // transport.StopAsync().Wait();

            Console.ReadLine();
        }

        private async static Task<EpoxyTransport> SetupAsync()
        {
            var transport = new EpoxyTransportBuilder()
                .SetLogSink(new ConsoleLogger())
                .Construct();

            var assignAPortEndPoint = new IPEndPoint(IPAddress.Loopback, EpoxyTransport.DefaultInsecurePort);

            var notifyService = new NotifyEventService();
            EpoxyListener notifyListener = transport.MakeListener(assignAPortEndPoint);
            notifyListener.AddService(notifyService);

            await notifyListener.StartAsync();

            s_connection = await transport.ConnectToAsync("epoxy://127.0.0.1", CancellationToken.None);

            return transport;
        }

        private static void MakeRequestsAndPrint(int numRequests)
        {
            var notifyEventProxy = new NotifyEventProxy<EpoxyConnection>(s_connection);

            var rnd = new Random();

            foreach (var requestNum in Enumerable.Range(0, numRequests))
            {
                UInt16 delay = (UInt16)rnd.Next(2000);
                DoNotify(notifyEventProxy, requestNum, "notify" + requestNum.ToString(), delay);
            }
        }

        private static void DoNotify(NotifyEventProxy<EpoxyConnection> proxy, int requestNum, string payload, UInt16 delay)
        {
            var request = new PingRequest { Payload = payload, DelayMilliseconds = delay };
            proxy.NotifyAsync(request);

            Console.WriteLine($"P Event #{requestNum} Delay: {delay}");
        }

    }
}
