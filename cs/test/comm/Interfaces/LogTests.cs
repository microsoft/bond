// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using Bond.Comm;
    using NUnit.Framework;

    [TestFixture]
    public class LogTests
    {
        private class TestLogHandler : LogHandler
        {
            public LogSeverity? LastMessageSeverity;
            public String LastMessage;
            public Exception LastException;
            public int MessagesHandled;

            public void Handle(LogSeverity severity, string message, Exception exception)
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
            public LogSeverity Severity;

            public TestException(LogSeverity severity)
            {
                Severity = severity;
            }
        }

        private readonly Dictionary<LogSeverity, Action<string, Exception>> levelLoggers =
            new Dictionary<LogSeverity, Action<string, Exception>>()
        {
                { LogSeverity.Fatal, Log.Fatal },
                { LogSeverity.Error, Log.Error },
                { LogSeverity.Warning, Log.Warning },
                { LogSeverity.Information, Log.Information },
                { LogSeverity.Debug, Log.Debug },
        };

        private String MakeMessage(LogSeverity severity, bool withException)
        {
            return String.Format("logged at {0} with{1} an Exception", severity, withException ? "" : "out");
        }

        [SetUp]
        public void SetUp()
        {
            handler = new TestLogHandler();
            Log.AddHandler(handler);
        }

        [TearDown]
        public void TearDown()
        {
            Log.RemoveHandler();
        }

        [Test]
        public void AllSeveritiesCovered()
        {
            var allSeverities = Enum.GetValues(typeof(LogSeverity)).Cast<LogSeverity>().ToList();
            CollectionAssert.AreEquivalent(allSeverities, levelLoggers.Keys);
        }

        [Test]
        public void Levels()
        {
            var messagesLogged = 0;

            foreach (var entry in levelLoggers)
            {
                var severity = entry.Key;
                var logger = entry.Value;
                var exception = new TestException(severity);

                var message = MakeMessage(severity, withException: false);
                logger(message, null);
                Assert.AreEqual(severity, handler.LastMessageSeverity);
                Assert.AreEqual(message, handler.LastMessage);
                Assert.IsNull(handler.LastException);
                Assert.AreEqual(messagesLogged + 1, handler.MessagesHandled);
                messagesLogged++;
                handler.Clear();

                message = MakeMessage(severity, withException: true);
                logger(message, exception);
                Assert.AreEqual(severity, handler.LastMessageSeverity);
                Assert.AreEqual(message, handler.LastMessage);
                Assert.NotNull(handler.LastException);
                Assert.AreEqual(severity, ((TestException) handler.LastException).Severity);
                Assert.AreEqual(messagesLogged + 1, handler.MessagesHandled);
                messagesLogged++;
                handler.Clear();
            }
        }

        [Test]
        public void DuplicateHandlersAreRejected()
        {
            Assert.Throws<InvalidOperationException>(delegate { Log.AddHandler(handler); });
        }

        [Test]
        public void NullHandlersAreRejected()
        {
            Assert.Throws<ArgumentException>(delegate { Log.AddHandler(null); });
        }

        [Test]
        public void LoggingWithNullHandlerIsANoOp()
        {
            // Clear the handler registered by SetUp().
            Log.RemoveHandler();
            Log.Information("no-op");
        }
    }
}
