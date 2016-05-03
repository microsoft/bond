// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Epoxy
{
    using System.Net;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Bond.Comm.Epoxy;
    using NUnit.Framework;

    [TestFixture]
    public class EpoxyListenerTests
    {
        [Test]
        public async Task ListenOnPortZero_ActuallyListensOnSomeOtherPort()
        {
            var localhostEndpoint = new IPEndPoint(IPAddress.Loopback, 0);
            var transport = new EpoxyTransportBuilder()
                .SetUnhandledExceptionHandler(Transport.ToErrorExceptionHandler)
                .Construct();
            var listener = transport.MakeListener(localhostEndpoint);

            await listener.StartAsync();

            Assert.AreNotEqual(0, listener.ListenEndpoint.Port);

            await listener.StopAsync();
        }
    }
}
