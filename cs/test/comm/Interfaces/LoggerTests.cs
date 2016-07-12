// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;
    using System.Linq;
    using Bond.Comm;
    using NUnit.Framework;

    [TestFixture]
    public class LoggerTests
    {
        public static readonly Logger BlackHole = new Logger(null, includeDebug: false);

        private class TestLogSink : ILogSink
        {
            public LogSeverity? LastMessageSeverity;
            public string LastMessage;
            public Exception LastException;
            public int MessagesReceived;

            public void Log(string message, LogSeverity severity, Exception exception)
            {
                LastMessageSeverity = severity;
                LastMessage = message;
                LastException = exception;
                MessagesReceived++;
            }

            public void Clear()
            {
                LastMessageSeverity = null;
                LastMessage = null;
                LastException = null;
            }
        }
        private TestLogSink sink = new TestLogSink();
        private Logger logger;

        /// <summary>
        /// Carries a <see cref="LogSeverity"/> so we can tell which invocation produced it.
        /// </summary>
        private class TestException : Exception
        {
            public readonly LogSeverity Severity;

            public TestException(LogSeverity severity)
            {
                Severity = severity;
            }
        }

        private readonly List<LogSeverity> allSeverities = Enum.GetValues(typeof(LogSeverity)).Cast<LogSeverity>().OrderBy(x => x).ToList();

        private readonly Dictionary<LogSeverity, Action<Logger, string, object[]>> levelLoggers =
            new Dictionary<LogSeverity, Action<Logger, string, object[]>>
            {
                { LogSeverity.Debug, (logger, message, args) => logger.Site().Debug(message, args) },
                { LogSeverity.Information, (logger, message, args) => logger.Site().Information(message, args) },
                { LogSeverity.Warning, (logger, message, args) => logger.Site().Warning(message, args) },
                { LogSeverity.Error, (logger, message, args) => logger.Site().Error(message, args) },
                { LogSeverity.Fatal, (logger, message, args) => logger.Site().Fatal(message, args) },
        };

        private readonly Dictionary<LogSeverity, Action<Logger, Exception, string, object[]>> exceptionLevelLoggers =
            new Dictionary<LogSeverity, Action<Logger, Exception, string, object[]>>
            {
                { LogSeverity.Debug, (logger, ex, message, args) => logger.Site().Debug(ex, message, args) },
                { LogSeverity.Information, (logger, ex, message, args) => logger.Site().Information(ex, message, args) },
                { LogSeverity.Warning, (logger, ex, message, args) => logger.Site().Warning(ex, message, args) },
                { LogSeverity.Error, (logger, ex, message, args) => logger.Site().Error(ex, message, args) },
                { LogSeverity.Fatal, (logger, ex, message, args) => logger.Site().Fatal(ex, message, args) },
        };

        private static Tuple<string, object[]> MakeMessage(LogSeverity severity, bool withException)
        {
            var args = new object[] { severity, withException ? "" : "out" };
            return new Tuple<string, object[]>("logged at {0} with{1} an Exception", args);
        }

        [SetUp]
        public void SetUp()
        {
            sink = new TestLogSink();
            // Tests that exercise filtering enable it themselves.
            logger = new Logger(sink, includeDebug: true);
        }

        [TearDown]
        public void TearDown()
        {
            logger = null;
        }

        [Test]
        public void SeveritiesAreSorted()
        {
            // Assumes allSeverities is sorted.
            var numSeverities = allSeverities.Count;
            var lower = allSeverities.GetRange(0, numSeverities - 1);
            var higher = allSeverities.GetRange(1, numSeverities - 1);
            var pairs = lower.Zip(higher, (l, h) => new Tuple<LogSeverity, LogSeverity>(l, h));
            foreach (var pair in pairs)
            {
                Assert.Less(pair.Item1, pair.Item2);
            }
        }

        [Test]
        public void LoggingWithNullHandlerIsANoOp()
        {
            // Clear the Sink registered by SetUp().
            logger = new Logger(null, includeDebug: false);
            logger.Site().Information("no-op");
        }

        [Test]
        public void Levels()
        {
            // Make sure we're testing all severities.
            CollectionAssert.AreEquivalent(allSeverities, levelLoggers.Keys);
            CollectionAssert.AreEquivalent(allSeverities, exceptionLevelLoggers.Keys);

            var messagesLogged = 0;

            foreach (var severity in allSeverities)
            {
                var logIt = levelLoggers[severity];
                var logItEx = exceptionLevelLoggers[severity];
                var exception = new TestException(severity);

                var formatArgs = MakeMessage(severity, withException: false);
                var format = formatArgs.Item1;
                var args = formatArgs.Item2;
                var message = string.Format(format, args);
                logIt(logger, format, args);
                Assert.AreEqual(severity, sink.LastMessageSeverity);
                StringAssert.Contains(message, sink.LastMessage);
                Assert.IsNull(sink.LastException);
                Assert.AreEqual(messagesLogged + 1, sink.MessagesReceived);
                messagesLogged++;
                sink.Clear();

                formatArgs = MakeMessage(severity, withException: true);
                format = formatArgs.Item1;
                args = formatArgs.Item2;
                message = string.Format(format, args);
                logItEx(logger, exception, format, args);
                Assert.AreEqual(severity, sink.LastMessageSeverity);
                StringAssert.Contains(message, sink.LastMessage);
                Assert.NotNull(sink.LastException);
                Assert.AreEqual(severity, ((TestException)sink.LastException).Severity);
                Assert.AreEqual(messagesLogged + 1, sink.MessagesReceived);
                messagesLogged++;
                sink.Clear();
            }
        }

        [Test]
        public void FilterDebugging()
        {
            logger = new Logger(sink, includeDebug: false);

            var messagesHandled = sink.MessagesReceived;
            // Assumes allSeverities is sorted.
            foreach (var severity in allSeverities)
            {
                var message = "level " + severity;
                levelLoggers[severity](logger, message, new object[0]);
                if (severity == LogSeverity.Debug)
                {
                    Assert.Null(sink.LastMessage);
                    Assert.Null(sink.LastMessageSeverity);
                    Assert.AreEqual(messagesHandled, sink.MessagesReceived);
                }
                else
                {
                    StringAssert.Contains(message, sink.LastMessage);
                    Assert.AreEqual(severity, sink.LastMessageSeverity);
                    Assert.AreEqual(messagesHandled + 1, sink.MessagesReceived);
                    messagesHandled = sink.MessagesReceived;
                }
            }
        }

        [Test]
        public void LogContext()
        {
            // There are two statements on this line so that expectedLineNumber will be correct for the Warning().
            logger.Site().Warning("context"); var expectedLineNumber = new StackFrame(0, true).GetFileLineNumber();
            StringAssert.Contains(nameof(LoggerTests) + ".cs:" + expectedLineNumber, sink.LastMessage);
            StringAssert.Contains(nameof(LogContext), sink.LastMessage);
        }

        [Test]
        public void FatalWithFormatted()
        {
            var messagesLogged = 0;
            var exception = new TestException(LogSeverity.Fatal);

            var formatArgs = MakeMessage(LogSeverity.Fatal, withException: false);
            var format = formatArgs.Item1;
            var args = formatArgs.Item2;
            var formatted = string.Format(format, args);
            var messageReturned = logger.Site().FatalAndReturnFormatted(format, args);
            Assert.AreEqual(LogSeverity.Fatal, sink.LastMessageSeverity);
            StringAssert.Contains(formatted, sink.LastMessage);
            StringAssert.Contains(messageReturned, sink.LastMessage);
            Assert.IsNull(sink.LastException);
            Assert.AreEqual(messagesLogged + 1, sink.MessagesReceived);
            messagesLogged++;
            sink.Clear();

            formatArgs = MakeMessage(LogSeverity.Fatal, withException: true);
            format = formatArgs.Item1;
            args = formatArgs.Item2;
            formatted = string.Format(format, args);
            messageReturned = logger.Site().FatalAndReturnFormatted(exception, format, args);
            Assert.AreEqual(LogSeverity.Fatal, sink.LastMessageSeverity);
            StringAssert.Contains(formatted, sink.LastMessage);
            StringAssert.Contains(messageReturned, sink.LastMessage);
            Assert.NotNull(sink.LastException);
            Assert.AreEqual(LogSeverity.Fatal, ((TestException)sink.LastException).Severity);
            Assert.AreEqual(messagesLogged + 1, sink.MessagesReceived);
        }

        [Test]
        public void FormatExceptionIsSuppressedAndLogged()
        {
            const string format = "{0}, but there's nothing to put here: {1}";
            const string somestr = "any string goes here";

            // Messages below Error should be raised to Error.
            logger.Site().Debug(format, somestr);

            Assert.AreEqual(LogSeverity.Error, sink.LastMessageSeverity);
            Assert.Null(sink.LastException);
            StringAssert.Contains(format, sink.LastMessage);
            StringAssert.Contains(somestr, sink.LastMessage);
            StringAssert.Contains("Log call threw", sink.LastMessage);

            // Messages above Error should be left at their original severity.
            logger.Site().Fatal(format, somestr);

            Assert.AreEqual(LogSeverity.Fatal, sink.LastMessageSeverity);
            Assert.Null(sink.LastException);
            StringAssert.Contains(format, sink.LastMessage);
            StringAssert.Contains(somestr, sink.LastMessage);
            StringAssert.Contains("Log call threw", sink.LastMessage);
        }

        [Test]
        public void LogInstanceReuseThrows()
        {
            var logInstance = logger.Site();
            logInstance.Warning("This should be fine.");
            Assert.Throws<InvalidOperationException>(() => logInstance.Warning("This should throw."));
        }
    }
}
