// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;

    /// <summary>
    /// Represents the severity of a message.
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
    /// Once passed to <see cref="Log.AddHandler"/>, will receive callbacks for messages logged by Bond.
    /// </summary>
    public interface LogHandler
    {
        void Handle(LogSeverity severity, String message, Exception exception = null);
    }

    /// <summary>
    /// By default, Bond is silent. Implement a <see cref="LogHandler"/> and pass it
    /// to <see cref="AddHandler"/> to receive log messages.
    /// </summary>
    public static class Log
    {
        private static LogHandler handler;

        public static void AddHandler(LogHandler newHandler)
        {
            if (newHandler == null)
            {
                throw new ArgumentException("Attempted to add a null LogHandler");
            }
            if (Log.handler != null)
            {
                throw new InvalidOperationException("Attempted to add a LogHandler when there already was one");
            }
            Log.handler = newHandler;
        }

        public static void RemoveHandler()
        {
            handler = null;
        }

        private static void LogMessage(LogSeverity severity, string message, Exception exception = null)
        {
            handler?.Handle(severity, message, exception);
        }

        public static void Fatal(string message, Exception exception = null)
        {
            LogMessage(LogSeverity.Fatal, message, exception);
        }

        public static void Error(string message, Exception exception = null)
        {
            LogMessage(LogSeverity.Error, message, exception);
        }

        public static void Warning(string message, Exception exception = null)
        {
            LogMessage(LogSeverity.Warning, message, exception);
        }

        public static void Information(string message, Exception exception = null)
        {
            LogMessage(LogSeverity.Information, message, exception);
        }

        public static void Debug(string message, Exception exception = null)
        {
            LogMessage(LogSeverity.Debug, message, exception);
        }
    }
}
