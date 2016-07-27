// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Diagnostics;

    /// <summary>
    /// Receives metrics emitted by Transports. Supply an instance of <c>IMetricsSink</c> to a
    /// <see cref="TransportBuilder{TTransport}.SetMetricsSink"/> to register it with the
    /// <see cref="Transport{TConnection,TListener}"/> returned by <see cref="TransportBuilder{TTransport}.Construct"/>.
    /// </summary>
    public interface IMetricsSink
    {
        /// <summary>
        /// Will be called once at the end of each connection.
        /// </summary>
        void Emit(ConnectionMetrics metrics);

        /// <summary>
        /// Will be called once at the end of each request or event.
        /// </summary>
        void Emit(RequestMetrics metrics);
    }

    public class Metrics
    {
        internal readonly IMetricsSink Sink;

        public Metrics(IMetricsSink metricsSink)
        {
            Sink = metricsSink;
        }

        public void Emit(ConnectionMetrics metrics)
        {
            Sink?.Emit(metrics);
        }

        public void Emit(RequestMetrics metrics)
        {
            Sink?.Emit(metrics);
        }

        public static string NewId()
        {
            return Guid.NewGuid().ToString();
        }

        public static ConnectionMetrics StartConnectionMetrics()
        {
            return new ConnectionMetrics
            {
                connection_id = NewId()
            };
        }

        public static void FinishConnectionMetrics(ConnectionMetrics connectionMetrics, Stopwatch duration)
        {
            connectionMetrics.duration_millis = duration.Elapsed.TotalMilliseconds;
        }

        public static RequestMetrics StartRequestMetrics(ConnectionMetrics connectionMetrics)
        {
            return new RequestMetrics
            {
                request_id = NewId(),
                connection_id = connectionMetrics.connection_id,
                local_endpoint = connectionMetrics.local_endpoint,
                remote_endpoint = connectionMetrics.remote_endpoint,
                error = null
            };
        }

        public static void FinishRequestMetrics(RequestMetrics requestMetrics, Stopwatch totalTime)
        {
            requestMetrics.total_time_millis = totalTime.Elapsed.TotalMilliseconds;
        }
    }
}
