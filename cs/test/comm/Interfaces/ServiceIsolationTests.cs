// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using Bond.Comm.Service;
    using NUnit.Framework;
    using UnitTest.Comm;

    [TestFixture]
    class ServiceIsolationTests
    {
        // Metrics emission does not necessarily happen before the client gets control back from
        // a request or connection shutdown. There's nothing to block on that would guarantee this.
        internal static readonly Action WaitForLogsAndMetrics = () => Thread.Sleep(500);

        // For cleanup in a [TearDown].
        private List<EpoxyListener> listeners = new List<EpoxyListener>();

        private class InMemLogSink : ILogSink
        {
            public readonly List<string> Messages = new List<string>();

            public void Log(string message, LogSeverity severity, Exception exception)
            {
                Messages.Add(message);
            }
        }

        private class InMemMetricsSink : IMetricsSink
        {
            public readonly List<ConnectionMetrics> ConnectionMetricses = new List<ConnectionMetrics>();
            public readonly List<RequestMetrics> RequestMetricses = new List<RequestMetrics>();

            public void Emit(ConnectionMetrics metrics)
            {
                ConnectionMetricses.Add(metrics);
            }

            public void Emit(RequestMetrics metrics)
            {
                RequestMetricses.Add(metrics);
            }
        }

        private async Task<EpoxyTransport> Server(
            ILogSink logSink, IMetricsSink metricsSink,
            IPEndPoint endPoint)
        {
            var transport = new EpoxyTransportBuilder()
                .SetLogSink(logSink)
                .SetMetricsSink(metricsSink)
                .Construct();

            var service = new DummyTestService();
            var listener = transport.MakeListener(endPoint);
            listeners.Add(listener);
            listener.AddService(service);
            await listener.StartAsync();

            return transport;
        }

        private static Task<EpoxyConnection> ClientConn(string address)
        {
            var transport = new EpoxyTransportBuilder().Construct();
            return transport.ConnectToAsync(address);
        }

        [TearDown]
        public async void TearDown()
        {
            foreach (var listener in listeners)
            {
                await listener.StopAsync();
            }
            listeners.Clear();
        }

        [Test]
        public async Task IsolatedLoggingAndMetrics()
        {
            // Create two services on their own transports with separate ILogSinks and IMetricsSinks.
            // Create one client for each service on separate transports so they don't log or emit metrics.
            // Make one request for each client-server pair.
            //
            // Each logging sink should see exactly one message from a particular point in the code that processes
            // requests, and the messages seen by each sink should be different (because the Connections are
            // different). Each metrics sink should see exactly one ConnectionMetrics and one RequestMetrics, and their
            // identifying GUIDs should be different.

            // This can be any method that isn't in a transport implementation, will be called once per request, and
            // will be called zero times outside the request path.
            const string chosenMethodName = nameof(ServiceHost.DispatchRequest);

            var firstLogSink = new InMemLogSink();
            var firstMetricsSink = new InMemMetricsSink();
            var secondLogSink = new InMemLogSink();
            var secondMetricsSink = new InMemMetricsSink();

            var firstEndPoint = new IPEndPoint(IPAddress.Loopback, 10001);
            var firstAddress = "epoxy://127.0.0.1:10001";
            var secondEndPoint = new IPEndPoint(IPAddress.Loopback, 10002);
            var secondAddress = "epoxy://127.0.0.1:10002";

            var firstServer = await Server(firstLogSink, firstMetricsSink, firstEndPoint);
            var firstClientConn = await ClientConn(firstAddress);
            var firstProxy = new DummyTestProxy<EpoxyConnection>(firstClientConn);
            var secondServer = await Server(secondLogSink, secondMetricsSink, secondEndPoint);
            var secondClientConn = await ClientConn(secondAddress);
            var secondProxy = new DummyTestProxy<EpoxyConnection>(secondClientConn);

            await firstProxy.ReqRspMethodAsync(new Dummy());
            await secondProxy.ReqRspMethodAsync(new Dummy());

            await firstClientConn.StopAsync();
            await secondClientConn.StopAsync();
            await firstServer.StopAsync();
            await secondServer.StopAsync();
            WaitForLogsAndMetrics();

            // We're targeting a log line that looks something like this:
            // C:\...\bond\cs\src\comm\service\ServiceHost.cs:119 - DispatchRequest - Got request [unittest.comm.DummyTest.ReqRspMethod] from EpoxyConnection(local: 127.0.0.1:20000, remote: 127.0.0.1:26056).
            var firstSinkTargetMessages = firstLogSink.Messages.Where(l => l.Contains(chosenMethodName)).ToList();
            var secondSinkTargetMessages = secondLogSink.Messages.Where(l => l.Contains(chosenMethodName)).ToList();

            Assert.AreEqual(1, firstSinkTargetMessages.Count);
            Assert.AreEqual(1, secondSinkTargetMessages.Count);

            Assert.AreNotEqual(firstSinkTargetMessages.First(), secondSinkTargetMessages.First());

            // Each metrics sink should have seen one set of connection metrics and one set of request metrics.
            Assert.AreEqual(1, firstMetricsSink.ConnectionMetricses.Count);
            Assert.AreEqual(1, firstMetricsSink.RequestMetricses.Count);
            Assert.AreEqual(1, secondMetricsSink.ConnectionMetricses.Count);
            Assert.AreEqual(1, secondMetricsSink.RequestMetricses.Count);

            Assert.AreNotEqual(firstMetricsSink.ConnectionMetricses.First().connection_id,
                               secondMetricsSink.ConnectionMetricses.First().connection_id);
            Assert.AreNotEqual(firstMetricsSink.RequestMetricses.First().request_id,
                               secondMetricsSink.RequestMetricses.First().request_id);
        }
    }
}
