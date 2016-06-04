// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Runtime.CompilerServices;

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
    /// Once passed to <see cref="Log.SetHandler"/>, will receive callbacks for
    /// messages logged by Bond.
    /// </summary>
    public interface ILogHandler
    {
        /// <summary>
        /// Invoked for each log message that Bond generates. Messages below
        /// Bond's <see cref="Log.DropBelow"/> will not result in calls to this
        /// function.
        /// </summary>
        /// <param name="message">Bond's log message, including the location in
        /// the code that generated it.</param>
        /// <param name="severity">The level the message was logged at.</param>
        /// <param name="exception">
        /// The exception that is associated with the log message. May be
        /// <c>null</c>.
        /// </param>
        void Handle(string message, LogSeverity severity, Exception exception);
    }

    /// <summary>
    /// Implement a <see cref="ILogHandler"/> and pass it to
    /// <see cref="SetHandler"/> to receive log messages.
    /// </summary>
    public class Log
    {
        /// <summary>
        /// Messages below DropBelow will not be passed to the <see cref="ILogHandler"/>.
        /// </summary>
        public static LogSeverity DropBelow { get; set; } = LogSeverity.Information;

        private static ILogHandler handler;

        /// <summary>
        /// Sets a <see cref="ILogHandler"/> to receive Bond log messages.
        /// </summary>
        /// <param name="newHandler">The handler to add.</param>
        /// <exception cref="InvalidOperationException">
        /// Thrown when there is another handler registered.
        /// </exception>
        public static void SetHandler(ILogHandler newHandler)
        {
            if (newHandler == null)
            {
                throw new ArgumentNullException(nameof(newHandler));
            }
            if (handler != null)
            {
                throw new InvalidOperationException(
                    $"Attempted to add a {nameof(ILogHandler)} when there already was one");
            }
            handler = newHandler;
        }

        /// <summary>
        /// Removes the existing <see cref="ILogHandler"/>.
        /// </summary>
        /// <remarks>
        /// May be called even if there is no existing handler.
        /// </remarks>
        public static void RemoveHandler()
        {
            handler = null;
        }

        internal static string Format(
            string fileName, int lineNumber, string methodName,
            string format, object[] args)
        {
            return string.Concat(
                fileName, ":", lineNumber, " - ", methodName, " - ",
                string.Format(format, args));

        }

        const string Unknown = "<unknown>";
        string filePath;
        int lineNumber;
        string memberName;
        bool used;

        /// <summary>
        /// The only correct way to log is to call Log.Site().{Debug, Information, ...}. Site() returns an object that
        /// captures your callsite and exposes the different log levels. Do not use the object returned by Site() to log
        /// twice. (We detect this and throw an exception.) Do not call Site() anywhere but the line you want to log from.
        /// (We cannot detect this, so you will just get incorrect callsite information.)
        /// </summary>
        /// <returns>A Log instance that will let you log exactly one message.</returns>
        public static Log Site(
            [CallerFilePath] string filePath = Unknown,
            [CallerLineNumber] int lineNumber = 0,
            [CallerMemberName] string memberName = Unknown)
        {
            return new Log
            {
                filePath = filePath,
                lineNumber = lineNumber,
                memberName = memberName
            };
        }

        private void LogMessage(
            LogSeverity severity, Exception exception,
            string format, object[] args)
        {
            CheckReuse();
            if (severity < DropBelow) { return; }

            // It's relatively easy to make mistakes in the variadic logging functions that will throw a FormatException
            // inside Format(). This is not a good reason to interrupt normal control flow or kill a service.
            // If that happens, catch the exception, log the message format and params that caused the exception, and
            // log at high severity regardless.
            string message;
            try
            {
                message = Format(filePath, lineNumber, memberName, format, args);
            }
            catch (FormatException fe)
            {
                severity = severity < LogSeverity.Error ? LogSeverity.Error : severity;
                // nulls in args will be logged as empty strings.
                var argsStr = "[" + string.Join(", ", args) + "]";
                const string formatExceptionFormat = "Log call threw {0}({1}) with format \"{2}\" and args {3}";
                message = Format(filePath, lineNumber, memberName, formatExceptionFormat,
                    new object[] {nameof(FormatException), fe.Message, format, argsStr});
            }
            handler?.Handle(message, severity, exception);
        }

        private void CheckReuse()
        {
            if (!used)
            {
                used = true;
            }
            else
            {
                var message =
                    $"An instance of {nameof(Log)} was used twice. First use: {filePath}:{lineNumber} - {memberName}";
                throw new InvalidOperationException(message);
            }
        }

        public void Fatal(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Fatal, exception, format, args);
        }

        public void Fatal(string format, params object[] args)
        {
            Fatal(null, format, args);
        }

        public void Error(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Error, exception, format, args);
        }

        public void Error(string format, params object[] args)
        {
            Error(null, format, args);
        }

        public void Warning(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Warning, exception, format, args);
        }

        public void Warning(string format, params object[] args)
        {
            Warning(null, format, args);
        }

        public void Information(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Information, exception, format, args);
        }

        public void Information(string format, params object[] args)
        {
            Information(null, format, args);
        }

        public void Debug(Exception exception, string format, params object[] args)
        {
            LogMessage(LogSeverity.Debug, exception, format, args);
        }

        public void Debug(string format, params object[] args)
        {
            Debug(null, format, args);
        }

        public string FatalAndReturnFormatted(Exception exception, string format, params object[] args)
        {
            Fatal(exception, format, args);
            return string.Format(format, args);
        }

        public string FatalAndReturnFormatted(string format, params object[] args)
        {
            return FatalAndReturnFormatted(null, format, args);
        }

        public string ErrorAndReturnFormatted(Exception exception, string format, params object[] args)
        {
            Error(exception, format, args);
            return string.Format(format, args);
        }

        public string ErrorAndReturnFormatted(string format, params object[] args)
        {
            return ErrorAndReturnFormatted(null, format, args);
        }
    }
}
