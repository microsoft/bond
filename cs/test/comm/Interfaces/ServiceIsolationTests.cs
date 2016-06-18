// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Interfaces
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Service;
    using Bond.Comm.SimpleInMem;
    using NUnit.Framework;
    using UnitTest.Comm;

    [TestFixture]
    class ServiceIsolationTests
    {
        private class InMemLogSink : ILogSink
        {
            public readonly List<string> Messages = new List<string>();

            public void Log(string message, LogSeverity severity, Exception exception)
            {
                Messages.Add(message);
            }
        }

        private static async Task<SimpleInMemTransport> SetupTransport(ILogSink logSink, string address)
        {
            var transport = new SimpleInMemTransportBuilder()
                .SetLogSink(logSink)
                .Construct();

            var service = new DummyTestService();
            var listener = transport.MakeListener(address);
            listener.AddService(service);
            await listener.StartAsync();

            return transport;
        }

        [Test]
        public async void IsolatedLogging()
        {
            // Create two services on separate transports with separate ILogSinks. Make one request to each.
            //
            // Each sink should see exactly one message from a particular point in the code that processes requests,
            // and the messages seen by each sink should be different (because the Connections are different).

            // This can be any method that will be called once per request and zero times outside the request path.
            const string chosenMethodName = nameof(ServiceHost.DispatchRequest);

            var firstSink = new InMemLogSink();
            var secondSink = new InMemLogSink();
            const string firstAddress = "first";
            const string secondAddress = "second";

            var firstTransport = await SetupTransport(firstSink, firstAddress);
            var secondTransport = await SetupTransport(secondSink, secondAddress);
            var firstProxy = new DummyTestProxy<SimpleInMemConnection>(
                (SimpleInMemConnection) await firstTransport.ConnectToAsync(firstAddress));
            var secondProxy = new DummyTestProxy<SimpleInMemConnection>(
                (SimpleInMemConnection) await secondTransport.ConnectToAsync(secondAddress));
            await firstProxy.ReqRspMethodAsync(new Dummy());
            await secondProxy.ReqRspMethodAsync(new Dummy());

            // We're targeting a log line that looks something like this:
            // C:\...\bond\cs\src\comm\service\ServiceHost.cs:119 - DispatchRequest - Got request [unittest.comm.DummyTest.ReqRspMethod] from EpoxyConnection(local: 127.0.0.1:20000, remote: 127.0.0.1:26056).
            var firstSinkTargetMessages = firstSink.Messages.Where(l => l.Contains(chosenMethodName)).ToList();
            var secondSinkTargetMessages = secondSink.Messages.Where(l => l.Contains(chosenMethodName)).ToList();

            Assert.AreEqual(1, firstSinkTargetMessages.Count);
            Assert.AreEqual(1, secondSinkTargetMessages.Count);

            Assert.AreNotEqual(firstSinkTargetMessages.First(), secondSinkTargetMessages.First());
        }
    }
}
