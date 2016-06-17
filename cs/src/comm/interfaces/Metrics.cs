// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    /// <summary>
    /// Receives metrics emitted by Transports. Supply an instance of <c>IMetricsSink</c> to a
    /// <see cref="TransportBuilder{TTransport}.SetMetricsSink"/> to register it with the <see cref="Transport"/>
    /// returned by <see cref="TransportBuilder{TTransport}.Construct"/>.
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
    }
}
