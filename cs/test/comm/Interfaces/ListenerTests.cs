// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using System.Threading.Tasks;
    using Bond.Comm;
    using NUnit.Framework;

    [TestFixture]
    public class ListenerTests
    {
        [Test]
        public void Connected_NoSubscribedEvents_Works()
        {
            new TestListener(LoggerTests.BlackHole).Test_OnConnected(new ConnectedEventArgs(null));
        }

        [Test]
        public void Connected_MultipleSubscribedEvents_CalledInOrderUntilOneSetsDisconnectErrorToNonNull()
        {
            bool event1Called = false;
            bool event2Called = false;
            var event2Error = new Error();
            bool event3Called = false;
            var event3Error = new Error();

            var listener = new TestListener(LoggerTests.BlackHole);
            listener.Connected += (sender, args) =>
            {
                Assert.IsFalse(event1Called);
                Assert.IsFalse(event2Called);
                Assert.IsFalse(event3Called);
                event1Called = true;
            };
            listener.Connected += (sender, args) =>
            {
                Assert.IsTrue(event1Called);
                Assert.IsFalse(event2Called);
                Assert.IsFalse(event3Called);
                event2Called = true;
                args.DisconnectError = event2Error;
            };
            listener.Connected += (sender, args) =>
            {
                Assert.IsTrue(event1Called);
                Assert.IsTrue(event2Called);
                Assert.IsFalse(event3Called);
                event3Called = true;
                args.DisconnectError = event3Error;
            };

            var cea = new ConnectedEventArgs(null);
            listener.Test_OnConnected(cea);

            Assert.IsTrue(event1Called);
            Assert.IsTrue(event2Called);
            Assert.IsFalse(event3Called);
            Assert.AreSame(event2Error, cea.DisconnectError);
        }

        [Test]
        public void Disconnected_NoSubscribedEvents_Works()
        {
            new TestListener(LoggerTests.BlackHole).Test_OnDisconnected(new DisconnectedEventArgs(null, null));
        }

        [Test]
        public void Disconnected_MultipleSubscribedEvents_AllCalledAndInOrder()
        {
            bool event1Called = false;
            bool event2Called = false;

            var listener = new TestListener(LoggerTests.BlackHole);
            listener.Disconnected += (sender, eventArgs) =>
            {
                Assert.IsFalse(event1Called);
                Assert.IsFalse(event2Called);
                event1Called = true;
            };
            listener.Disconnected += (sender, eventArgs) =>
            {
                Assert.IsTrue(event1Called);
                Assert.IsFalse(event2Called);
                event2Called = true;
            };

            listener.Test_OnDisconnected(new DisconnectedEventArgs(null, null));

            Assert.IsTrue(event1Called);
            Assert.IsTrue(event2Called);
        }

        private class TestListener : Listener
        {
            public TestListener(Logger logger) : base(logger, MetricsTests.BlackHole) {}

            public override void AddService<T>(T service) { throw new NotImplementedException(); }
            public override void RemoveService<T>(T service) { throw new NotImplementedException(); }
            public override bool IsRegistered(string serviceMethodName) { throw new NotImplementedException(); }
            public override Task StartAsync() { throw new NotImplementedException(); }
            public override Task StopAsync() { throw new NotImplementedException(); }

            public Error Test_OnConnected(ConnectedEventArgs args)
            {
                return OnConnected(args);
            }

            public void Test_OnDisconnected(DisconnectedEventArgs args)
            {
                OnDisconnected(args);
            }
        }
    }
}
