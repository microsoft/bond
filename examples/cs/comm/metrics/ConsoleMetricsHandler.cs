using System;
using System.Text;

namespace Bond.Examples.Metrics
{
    using Bond.Comm;

    public class ConsoleMetricsHandler : IMetricsHandler
    {
        public void Handle(ConnectionMetrics metrics)
        {
            var sb = new StringBuilder(nameof(ConnectionMetrics) + " {\n");
            sb.Append($"\tconnection ID: {metrics.connection_id}\n");
            sb.Append($"\tendpoints: {metrics.local_endpoint} <-> {metrics.remote_endpoint}\n");
            sb.Append($"\tduration millis: {metrics.duration_millis}\n");
            sb.Append($"\tshutdown reason: {metrics.shutdown_reason}\n");
            sb.Append("}\n");
            Console.Write(sb);
        }

        public void Handle(RequestMetrics metrics)
        {
            var sb = new StringBuilder(nameof(RequestMetrics) + " {\n");
            sb.Append($"\trequest ID: {metrics.request_id}\n");
            sb.Append($"\tconnection ID: {metrics.connection_id}\n");
            sb.Append($"\tendpoints: {metrics.local_endpoint} <-> {metrics.remote_endpoint}\n");
            sb.Append($"\tmethod: {metrics.method_name}\n");
            sb.Append($"\ttotal millis: {metrics.total_time_millis}\n");
            sb.Append($"\tservice millis: {metrics.service_method_time_millis}\n");
            var errstr = metrics.error?.ToString() ?? "none";
            sb.Append($"\terror: {errstr}\n");
            sb.Append("}\n");
            Console.Write(sb);
        }
    }
}
