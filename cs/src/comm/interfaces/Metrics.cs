using System;

namespace Bond.Comm
{
    /// <summary>
    /// Once passed to <see cref="Metrics.SetHandler"/>, will receive callbacks
    /// when new metrics are emitted by Bond.
    /// </summary>
    public interface MetricsHandler
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
    /// Implement a <see cref="MetricsHandler"/> and pass it to
    /// <see cref="Metrics.SetHandler"/> to receive metrics objects showing
    /// what Bond is doing and where it's spending time.
    /// </summary>
    public static class Metrics
    {
        private static MetricsHandler handler;

        /// <summary>
        /// Sets a <see cref="MetricsHandler"/> to receive Bond metrics.
        /// </summary>
        /// <param name="newHandler">The handler to add.</param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when there is another handler registered.
        /// </exception>
        public static void SetHandler(MetricsHandler newHandler)
        {
            if (newHandler == null)
            {
                throw new ArgumentException($"Attempted to set a null {nameof(MetricsHandler)}");
            }

            if (handler != null)
            {
                throw new InvalidOperationException($"Attempted to set a {nameof(MetricsHandler)} when there already was one");
            }

            handler = newHandler;
        }

        /// <summary>
        /// Removes the existing <see cref="MetricsHandler"/>.
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
