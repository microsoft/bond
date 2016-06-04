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
    public class LogTests
    {
        private class TestLogHandler : ILogHandler
        {
            public LogSeverity? LastMessageSeverity;
            public string LastMessage;
            public Exception LastException;
            public int MessagesHandled;

            public void Handle(string message, LogSeverity severity, Exception exception)
            {
                LastMessageSeverity = severity;
                LastMessage = message;
                LastException = exception;
                MessagesHandled++;
            }

            public void Clear()
            {
                LastMessageSeverity = null;
                LastMessage = null;
                LastException = null;
            }
        }
        private TestLogHandler handler = new TestLogHandler();

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

        private readonly Dictionary<LogSeverity, Action<string, object[]>> levelLoggers =
            new Dictionary<LogSeverity, Action<string, object[]>>
            {
                { LogSeverity.Debug, (message, args) => Log.Site().Debug(message, args) },
                { LogSeverity.Information, (message, args) => Log.Site().Information(message, args) },
                { LogSeverity.Warning, (message, args) => Log.Site().Warning(message, args) },
                { LogSeverity.Error, (message, args) => Log.Site().Error(message, args) },
                { LogSeverity.Fatal, (message, args) => Log.Site().Fatal(message, args) },
        };

        private readonly Dictionary<LogSeverity, Action<Exception, string, object[]>> exceptionLevelLoggers =
            new Dictionary<LogSeverity, Action<Exception, string, object[]>>
            {
                { LogSeverity.Debug, (ex, message, args) => Log.Site().Debug(ex, message, args) },
                { LogSeverity.Information, (ex, message, args) => Log.Site().Information(ex, message, args) },
                { LogSeverity.Warning, (ex, message, args) => Log.Site().Warning(ex, message, args) },
                { LogSeverity.Error, (ex, message, args) => Log.Site().Error(ex, message, args) },
                { LogSeverity.Fatal, (ex, message, args) => Log.Site().Fatal(ex, message, args) },
        };

        private static Tuple<string, object[]> MakeMessage(LogSeverity severity, bool withException)
        {
            var args = new object[] {severity, withException ? "" : "out" };
            return new Tuple<string, object[]>("logged at {0} with{1} an Exception", args);
        }

        [SetUp]
        public void SetUp()
        {
            // Tests that exercise filtering enable it themselves.
            Log.DropBelow = LogSeverity.Debug;
            handler = new TestLogHandler();
            Log.SetHandler(handler);
        }

        [TearDown]
        public void TearDown()
        {
            Log.RemoveHandler();
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
        public void DuplicateHandlersAreRejected()
        {
            Assert.Throws<InvalidOperationException>(() => Log.SetHandler(handler));
        }

        [Test]
        public void NullHandlersAreRejected()
        {
            Assert.Throws<ArgumentNullException>(() => Log.SetHandler(null));
        }

        [Test]
        public void LoggingWithNullHandlerIsANoOp()
        {
            // Clear the handler registered by SetUp().
            Log.RemoveHandler();
            Log.Site().Information("no-op");
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
                var logger = levelLoggers[severity];
                var exceptionLogger = exceptionLevelLoggers[severity];
                var exception = new TestException(severity);

                var formatArgs = MakeMessage(severity, withException: false);
                var format = formatArgs.Item1;
                var args = formatArgs.Item2;
                var message = string.Format(format, args);
                logger(format, args);
                Assert.AreEqual(severity, handler.LastMessageSeverity);
                StringAssert.Contains(message, handler.LastMessage);
                Assert.IsNull(handler.LastException);
                Assert.AreEqual(messagesLogged + 1, handler.MessagesHandled);
                messagesLogged++;
                handler.Clear();

                formatArgs = MakeMessage(severity, withException: true);
                format = formatArgs.Item1;
                args = formatArgs.Item2;
                message = string.Format(format, args);
                exceptionLogger(exception, format, args);
                Assert.AreEqual(severity, handler.LastMessageSeverity);
                StringAssert.Contains(message, handler.LastMessage);
                Assert.NotNull(handler.LastException);
                Assert.AreEqual(severity, ((TestException) handler.LastException).Severity);
                Assert.AreEqual(messagesLogged + 1, handler.MessagesHandled);
                messagesLogged++;
                handler.Clear();
            }
        }

        [Test]
        public void DropBelow()
        {
            const LogSeverity dropBelow = LogSeverity.Warning;
            Log.DropBelow = dropBelow;

            var messagesHandled = handler.MessagesHandled;
            // Assumes allSeverities is sorted.
            foreach (var severity in allSeverities)
            {
                var message = "level " + severity;
                levelLoggers[severity](message, new object[0]);
                if (severity < dropBelow)
                {
                    Assert.Null(handler.LastMessage);
                    Assert.Null(handler.LastMessageSeverity);
                    Assert.AreEqual(messagesHandled, handler.MessagesHandled);
                }
                else
                {
                    StringAssert.Contains(message, handler.LastMessage);
                    Assert.AreEqual(severity, handler.LastMessageSeverity);
                    Assert.AreEqual(messagesHandled + 1, handler.MessagesHandled);
                    messagesHandled = handler.MessagesHandled;
                }
            }
        }

        [Test]
        public void LogContext()
        {
            // There are two statements on this line so that expectedLineNumber will be correct for the Warning().
            Log.Site().Warning("context"); var expectedLineNumber = new StackFrame(0, true).GetFileLineNumber();
            StringAssert.Contains(nameof(LogTests) + ".cs:" + expectedLineNumber, handler.LastMessage);
            StringAssert.Contains(nameof(LogContext), handler.LastMessage);
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
            var messageReturned = Log.Site().FatalAndReturnFormatted(format, args);
            Assert.AreEqual(LogSeverity.Fatal, handler.LastMessageSeverity);
            StringAssert.Contains(formatted, handler.LastMessage);
            StringAssert.Contains(messageReturned, handler.LastMessage);
            Assert.IsNull(handler.LastException);
            Assert.AreEqual(messagesLogged + 1, handler.MessagesHandled);
            messagesLogged++;
            handler.Clear();

            formatArgs = MakeMessage(LogSeverity.Fatal, withException: true);
            format = formatArgs.Item1;
            args = formatArgs.Item2;
            formatted = string.Format(format, args);
            messageReturned = Log.Site().FatalAndReturnFormatted(exception, format, args);
            Assert.AreEqual(LogSeverity.Fatal, handler.LastMessageSeverity);
            StringAssert.Contains(formatted, handler.LastMessage);
            StringAssert.Contains(messageReturned, handler.LastMessage);
            Assert.NotNull(handler.LastException);
            Assert.AreEqual(LogSeverity.Fatal, ((TestException)handler.LastException).Severity);
            Assert.AreEqual(messagesLogged + 1, handler.MessagesHandled);
        }

        [Test]
        public void FormatExceptionIsSuppressedAndLogged()
        {
            const string format = "{0}, but there's nothing to put here: {1}";
            const string somestr = "any string goes here";

            // Messages below Error should be raised to Error.
            Log.Site().Debug(format, somestr);

            Assert.AreEqual(LogSeverity.Error, handler.LastMessageSeverity);
            Assert.Null(handler.LastException);
            StringAssert.Contains(format, handler.LastMessage);
            StringAssert.Contains(somestr, handler.LastMessage);
            StringAssert.Contains("Log call threw", handler.LastMessage);

            // Messages above Error should be left at their original severity.
            Log.Site().Fatal(format, somestr);

            Assert.AreEqual(LogSeverity.Fatal, handler.LastMessageSeverity);
            Assert.Null(handler.LastException);
            StringAssert.Contains(format, handler.LastMessage);
            StringAssert.Contains(somestr, handler.LastMessage);
            StringAssert.Contains("Log call threw", handler.LastMessage);
        }

        [Test]
        public void LogInstanceReuseThrows()
        {
            var logInstance = Log.Site();
            logInstance.Warning("This should be fine.");
            Assert.Throws<InvalidOperationException>(() => logInstance.Warning("This should throw."));
        }
    }
}
