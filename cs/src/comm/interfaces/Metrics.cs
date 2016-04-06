using System;

namespace Bond.Comm
{
    using System.Diagnostics;

    public interface MetricsHandler
    {
        void Handle(ConnectionMetrics metrics);

        void Handle(RequestMetrics metrics);
    }

    public static class Metrics
    {
        private static MetricsHandler handler;

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
