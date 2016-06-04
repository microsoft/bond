using System;

namespace Bond.Comm
{
    /// <summary>
    /// Once passed to <see cref="Metrics.SetHandler"/>, will receive callbacks
    /// when new metrics are emitted by Bond.
    /// </summary>
    public interface IMetricsHandler
    {
        /// <summary>
        /// Will be called once at the end of each connection.
        /// </summary>
        void Handle(ConnectionMetrics metrics);

        /// <summary>
        /// Will be called once at the end of each request or event.
        /// </summary>
        void Handle(RequestMetrics metrics);
    }

    /// <summary>
    /// Implement a <see cref="IMetricsHandler"/> and pass it to
    /// <see cref="Metrics.SetHandler"/> to receive metrics objects showing
    /// what Bond is doing and where it's spending time.
    /// </summary>
    public static class Metrics
    {
        private static IMetricsHandler handler;

        /// <summary>
        /// Sets a <see cref="IMetricsHandler"/> to receive Bond metrics.
        /// </summary>
        /// <param name="newHandler">The handler to add.</param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when there is another handler registered.
        /// </exception>
        public static void SetHandler(IMetricsHandler newHandler)
        {
            if (newHandler == null)
            {
                throw new ArgumentException($"Attempted to set a null {nameof(IMetricsHandler)}");
            }

            if (handler != null)
            {
                throw new InvalidOperationException($"Attempted to set a {nameof(IMetricsHandler)} when there already was one");
            }

            handler = newHandler;
        }

        /// <summary>
        /// Removes the existing <see cref="IMetricsHandler"/>.
        /// </summary>
        /// <remarks>
        /// May be called even if there is no existing handler.
        /// </remarks>
        public static void RemoveHandler()
        {
            handler = null;
        }

        public static void Emit(ConnectionMetrics metrics)
        {
            handler?.Handle(metrics);
        }

        public static void Emit(RequestMetrics metrics)
        {
            handler?.Handle(metrics);
        }
    }
}
