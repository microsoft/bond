// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;

    /// <summary>
    /// Represents the severity of a log message. Severities compare with &lt;
    /// and &gt;. e.g., Debug &lt; Information.
    /// </summary>
    public enum LogSeverity
    {
        Debug,
        Information,
        Warning,
        Error,
        Fatal
    }

    /// <summary>
    /// Once passed to <see cref="Log.AddHandler"/>, will receive callbacks for
    /// messages logged by Bond.
    /// </summary>
    public interface LogHandler
    {
        /// <summary>
        /// Invoked for each log message that Bond wants to log.
        /// </summary>
        /// <param name="severity">The severity of the log message.</param>
        /// <param name="exception">
        /// The exception that is associated with the log message. May be
        /// <c>null</c>.
        /// </param>
        /// <param name="format">The format string.</param>
        /// <param name="args">The format string arguments.</param>
        /// <remarks>
        /// It is the responsibility of the LogHandler to format the message.
        /// </remarks>
        void Handle(LogSeverity severity, Exception exception, string format, object[] args);
    }

    /// <summary>
    /// By default, Bond is silent. Implement a <see cref="LogHandler"/> and
    /// pass it to <see cref="AddHandler"/> to receive log messages.
    /// </summary>
    public static class Log
    {
        private static LogHandler handler;

        /// <summary>
        /// Adds a <see cref="LogHandler"/> to receive Bond log messages.
        /// </summary>
        /// <param name="newHandler">The handler to add.</param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when there is another handler registered.
        /// </exception>
        public static void AddHandler(LogHandler newHandler)
        {
            if (newHandler == null)
            {
                throw new ArgumentNullException(nameof(newHandler));
            }
            if (handler != null)
            {
                throw new InvalidOperationException(
                    $"Attempted to add a {nameof(LogHandler)} when there already was one");
            }
            handler = newHandler;
        }

        /// <summary>
        /// Removes the existing <see cref="LogHandler"/>.
        /// </summary>
        /// <remarks>
        /// May be called even if there is no longer handler.
        /// </remarks>
        public static void RemoveHandler()
        {
            handler = null;
        }

        public static void Fatal(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Fatal, exception, format, args);
        }

        public static void Fatal(string format, params object[] args)
        {
            LogMessage(LogSeverity.Fatal, null, format, args);
        }

        public static void Error(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Error, exception, format, args);
        }

        public static void Error(string format, params object[] args)
        {
            LogMessage(LogSeverity.Error, null, format, args);
        }

        public static void Warning(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Warning, exception, format, args);
        }

        public static void Warning(string format, params object[] args)
        {
            LogMessage(LogSeverity.Warning, null, format, args);
        }

        public static void Information(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Information, exception, format, args);
        }

        public static void Information(string format, params object[] args)
        {
            LogMessage(LogSeverity.Information, null, format, args);
        }

        public static void Debug(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Debug, exception, format, args);
        }

        public static void Debug(string format, params object[] args)
        {
            LogMessage(LogSeverity.Debug, null, format, args);
        }

        private static void LogMessage(LogSeverity severity, Exception exception, string format, object[] args)
        {
            handler?.Handle(severity, exception, format, args);
        }
    }

    public class LogUtil
    {
        public static string FatalAndReturnFormatted(Exception exception, string format, params object[] args)
        {
            Log.Fatal(exception, format, args);
            return string.Format(format, args);
        }

        public static string FatalAndReturnFormatted(string format, params object[] args)
        {
            Log.Fatal(null, format, args);
            return string.Format(format, args);
        }

        public static string ErrorAndReturnFormatted(Exception exception, string format, params object[] args)
        {
            Log.Error(exception, format, args);
            return string.Format(format, args);
        }

        public static string ErrorAndReturnFormatted(string format, params object[] args)
        {
            Log.Error(null, format, args);
            return string.Format(format, args);
        }
    }
}
