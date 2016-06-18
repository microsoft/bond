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
    /// Pass an instance of <c>ILogSink</c> to <see cref="TransportBuilder{TTransport}.SetLogSink"/> to receive Bond's
    /// log messages.
    /// </summary>
    public interface ILogSink
    {
        /// <summary>
        /// Invoked for each log message that Bond generates.
        /// </summary>
        /// <param name="message">Bond's log message, including the location in
        /// the code that generated it.</param>
        /// <param name="severity">The level the message was logged at.</param>
        /// <param name="exception">
        /// The exception that is associated with the log message. May be
        /// <c>null</c>.
        /// </param>
        void Log(string message, LogSeverity severity, Exception exception);
    }

    public class Logger
    {
        internal readonly ILogSink Sink;
        internal readonly bool IncludeDebug;


        public Logger(ILogSink sink, bool includeDebug)
        {
            Sink = sink;
            IncludeDebug = includeDebug;
        }

        /// <summary>
        /// The only correct way to log is to call someLogger.Site().{Debug, Information, ...}. Site() returns an
        /// object that captures your callsite and exposes the different log levels. Do not use the object returned by
        /// Site() to log twice. (We detect this and throw an exception.) Do not call Site() anywhere but the line you
        /// want to log from. (We cannot detect this, so you will just get incorrect callsite information.)
        /// </summary>
        /// <returns>A LogSite instance that will let you log exactly one message.</returns>
        public LogSite Site(
            [CallerFilePath] string filePath = LogSite.Unknown,
            [CallerLineNumber] int lineNumber = 0,
            [CallerMemberName] string memberName = LogSite.Unknown)
        {
            return new LogSite
            {
                Logger = this,
                FilePath = filePath,
                LineNumber = lineNumber,
                MemberName = memberName
            };
        }
    }

    /// <summary>
    /// Represents a physical location in source. The log level methods of this class will incorporate it in the
    /// messages they produce.
    ///
    /// Each LogSite will permit exactly one call to one of its log level methods. Calling another will throw an
    /// exception.
    /// </summary>
    public class LogSite
    {
        internal Logger Logger;
        internal const string Unknown = "<unknown>";
        internal string FilePath;
        internal int LineNumber;
        internal string MemberName;
        internal bool Used;

        internal static string Format(
            string fileName, int lineNumber, string methodName,
            string format, object[] args)
        {
            return string.Concat(
                fileName, ":", lineNumber, " - ", methodName, " - ",
                string.Format(format, args));

        }

        private void LogMessage(
            LogSeverity severity, Exception exception,
            string format, object[] args)
        {
            CheckReuse();
            if (!Logger.IncludeDebug && severity == LogSeverity.Debug) { return; }

            // It's relatively easy to make mistakes in the variadic logging functions that will throw a FormatException
            // inside Format(). This is not a good reason to interrupt normal control flow or kill a service.
            // If that happens, catch the exception, log the message format and params that caused the exception, and
            // log at high severity regardless.
            string message;
            try
            {
                message = Format(FilePath, LineNumber, MemberName, format, args);
            }
            catch (FormatException fe)
            {
                severity = severity < LogSeverity.Error ? LogSeverity.Error : severity;
                // nulls in args will be logged as empty strings.
                var argsStr = "[" + string.Join(", ", args) + "]";
                const string formatExceptionFormat = "Log call threw {0}({1}) with format \"{2}\" and args {3}";
                message = Format(FilePath, LineNumber, MemberName, formatExceptionFormat,
                    new object[] {nameof(FormatException), fe.Message, format, argsStr});
            }
            Logger.Sink?.Log(message, severity, exception);
        }

        private void CheckReuse()
        {
            if (!Used)
            {
                Used = true;
            }
            else
            {
                var message =
                    $"An instance of {nameof(Comm.Logger)} was used twice. First use: {FilePath}:{LineNumber} - {MemberName}";
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
