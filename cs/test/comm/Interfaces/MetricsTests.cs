// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;
    using UnitTest.Comm;

    [TestFixture]
    class MetricsTests
    {
        public static readonly Metrics BlackHole = new Metrics(null);

        // Metrics emission does not necessarily happen before the client gets control back from
        // a request or connection shutdown. There's nothing to block on that would guarantee this.
        internal static readonly Action WaitForMetrics = () => Thread.Sleep(500);

        private class TestMetricsSink : IMetricsSink
        {
            public ConnectionMetrics LastConnectionMetrics;
            public RequestMetrics LastRequestMetrics;
            public int ConnectionMetricsReceived;
            public int RequestMetricsReceived;

            public void Emit(ConnectionMetrics metrics)
            {
                LastConnectionMetrics = metrics;
                ConnectionMetricsReceived++;
            }

            public void Emit(RequestMetrics metrics)
            {
                LastRequestMetrics = metrics;
                RequestMetricsReceived++;
            }
        }

        internal void AssertValidId(string id)
        {
            Assert.NotNull(id);
            StringAssert.IsMatch("\\A[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\\z", id);
        }

        [Test]
        public void ValidIdFormat()
        {
            AssertValidId(Metrics.NewId());
        }

        [Test]
        public void StartAndFinishConnectionMetrics()
        {
            var stopwatch = Stopwatch.StartNew();
            var connectionMetrics = Metrics.StartConnectionMetrics();

            AssertValidId(connectionMetrics.connection_id);
            Assert.IsEmpty(connectionMetrics.local_endpoint);
            Assert.IsEmpty(connectionMetrics.remote_endpoint);
            Assert.AreEqual(0.0, connectionMetrics.duration_millis);
            Assert.AreEqual(ConnectionShutdownReason.UNKNOWN, connectionMetrics.shutdown_reason);

            Metrics.FinishConnectionMetrics(connectionMetrics, stopwatch);

            AssertValidId(connectionMetrics.connection_id);
            Assert.IsEmpty(connectionMetrics.local_endpoint);
            Assert.IsEmpty(connectionMetrics.remote_endpoint);
            Assert.Greater(connectionMetrics.duration_millis, 0.0);
            Assert.AreEqual(ConnectionShutdownReason.UNKNOWN, connectionMetrics.shutdown_reason);
        }

        [Test]
        public void StartAndFinishRequestMetrics()
        {
            var stopwatch = Stopwatch.StartNew();
            var connectionMetrics = Metrics.StartConnectionMetrics();
            var requestMetrics = Metrics.StartRequestMetrics(connectionMetrics);

            AssertValidId(requestMetrics.request_id);
            Assert.AreEqual(connectionMetrics.connection_id, requestMetrics.connection_id);
            Assert.IsEmpty(requestMetrics.local_endpoint);
            Assert.IsEmpty(requestMetrics.remote_endpoint);
            Assert.IsEmpty(requestMetrics.service_name);
            Assert.IsEmpty(requestMetrics.method_name);
            Assert.AreEqual(0.0, requestMetrics.service_method_time_millis);
            Assert.AreEqual(0.0, requestMetrics.total_time_millis);
            Assert.Null(requestMetrics.error);

            Metrics.FinishRequestMetrics(requestMetrics, stopwatch);

            AssertValidId(requestMetrics.request_id);
            Assert.AreEqual(connectionMetrics.connection_id, requestMetrics.connection_id);
            Assert.IsEmpty(requestMetrics.local_endpoint);
            Assert.IsEmpty(requestMetrics.remote_endpoint);
            Assert.IsEmpty(requestMetrics.service_name);
            Assert.IsEmpty(requestMetrics.method_name);
            Assert.AreEqual(0.0, requestMetrics.service_method_time_millis);
            Assert.Greater(requestMetrics.total_time_millis, 0.0);
            Assert.Null(requestMetrics.error);
        }

        [Test]
        public async Task Server_Request_MetricsAreEmitted()
        {
            await Server_MetricsAreEmitted(MessageType.REQUEST);
        }

        [Test]
        public async Task Server_Event_MetricsAreEmitted()
        {
            await Server_MetricsAreEmitted(MessageType.EVENT);
        }

        private async Task Server_MetricsAreEmitted(MessageType messageType)
        {
            // There are several invariants around metrics that involve cross-request and cross-connection state.
            // Bring up a service, connect to it several times, and make several requests each time.

            const string expectedServiceName = "unittest.comm.DummyTest";
            string expectedMethodName;
            Action<DummyTestProxy<EpoxyConnection>> doRpc;
            switch (messageType)
            {
                case MessageType.REQUEST:
                    expectedMethodName = "ReqRspMethod";
                    doRpc = async proxy => await proxy.ReqRspMethodAsync(new Dummy());
                    break;
                case MessageType.EVENT:
                    expectedMethodName = "EventMethod";
                    doRpc = proxy => proxy.EventMethodAsync(new Dummy());
                    break;
                default:
                    throw new ArgumentException(nameof(messageType));
            }

            var metricsSink = new TestMetricsSink();
            var serverTransport = new EpoxyTransportBuilder().SetMetricsSink(metricsSink).Construct();
            var listener = serverTransport.MakeListener("127.0.0.1");
            listener.AddService(new DummyTestService());
            await listener.StartAsync();
            var serverEndpoint = listener.ListenEndpoint.ToString();

            var connectionsSeen = 0;
            var requestsSeen = 0;
            var connectionIdsSeen = new HashSet<string>();
            var requestIdsSeen = new HashSet<string>();

            for (var i = 0; i < 3; i++)
            {
                var clientTransport = new EpoxyTransportBuilder().Construct();
                var clientConn = await clientTransport.ConnectToAsync("epoxy://127.0.0.1");
                var clientEndpoint = clientConn.LocalEndPoint.ToString();
                var proxy = new DummyTestProxy<EpoxyConnection>(clientConn);

                string currentConnectionId = null;
                for (var j = 0; j < 3; j++)
                {
                    doRpc(proxy);
                    WaitForMetrics();

                    Assert.AreEqual(requestsSeen + 1, metricsSink.RequestMetricsReceived,
                        "Did not get a RequestMetrics.");
                    Assert.NotNull(metricsSink.LastRequestMetrics);
                    var requestMetrics = metricsSink.LastRequestMetrics;

                    // The new RequestMetrics should have non-empty unique IDs. The request ID should be unique
                    // and the connection ID should match that of any requests previously made on this connection.
                    AssertValidId(requestMetrics.request_id);
                    AssertValidId(requestMetrics.connection_id);
                    CollectionAssert.DoesNotContain(requestIdsSeen, requestMetrics.request_id,
                        "Got two RequestMetrics with the same request ID.");
                    requestIdsSeen.Add(requestMetrics.request_id);
                    if (currentConnectionId == null)
                    {
                        currentConnectionId = requestMetrics.connection_id;
                    }
                    else
                    {
                        Assert.AreEqual(currentConnectionId, requestMetrics.connection_id,
                            "Got two different connection IDs in RequestMetrics for the same connection.");
                    }

                    Assert.AreEqual(serverEndpoint, requestMetrics.local_endpoint);
                    Assert.AreEqual(clientEndpoint, requestMetrics.remote_endpoint);
                    Assert.AreEqual(expectedServiceName, requestMetrics.service_name);
                    Assert.AreEqual(expectedMethodName, requestMetrics.method_name);
                    Assert.Null(requestMetrics.error);

                    Assert.Greater(requestMetrics.total_time_millis, 0.0);
                    Assert.Greater(requestMetrics.service_method_time_millis, 0.0);
                    Assert.Greater(requestMetrics.total_time_millis, requestMetrics.service_method_time_millis);

                    requestsSeen++;
                }

                // We're still connected, so there shouldn't be any new connection metrics.
                Assert.AreEqual(connectionsSeen, metricsSink.ConnectionMetricsReceived);

                await clientConn.StopAsync();
                WaitForMetrics();

                Assert.AreEqual(connectionsSeen + 1, metricsSink.ConnectionMetricsReceived);
                Assert.NotNull(metricsSink.LastConnectionMetrics);
                var connectionMetrics = metricsSink.LastConnectionMetrics;

                AssertValidId(connectionMetrics.connection_id);
                // The connection ID in the new ConnectionMetrics must match the one seen by the requests.
                Assert.AreEqual(currentConnectionId, connectionMetrics.connection_id,
                    "Got a different connection IDs in ConnectionMetrics and this connection's RequestMetrics.");
                CollectionAssert.DoesNotContain(connectionIdsSeen, connectionMetrics.connection_id,
                    "Got two different ConnectionMetrics with the same connection ID.");
                connectionIdsSeen.Add(connectionMetrics.connection_id);

                Assert.AreEqual(serverEndpoint, connectionMetrics.local_endpoint);
                Assert.AreEqual(clientEndpoint, connectionMetrics.remote_endpoint);
                Assert.Greater(connectionMetrics.duration_millis, 0.0);

                connectionsSeen++;
            }

            await listener.StopAsync();
        }
    }
}
