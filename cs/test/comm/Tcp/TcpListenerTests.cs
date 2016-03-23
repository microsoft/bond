// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.Tcp
{
    using Bond.Comm.Tcp;
    using NUnit.Framework;
    using System.Net;
    using System.Threading.Tasks;

    [TestFixture]
    public class TcpListenerTests
    {
        [Test]
        public async Task ListenOnPortZero_ActuallyListensOnSomeOtherPort()
        {
            var localhostEndpoint = new IPEndPoint(IPAddress.Loopback, 0);
            var transport = new TcpTransportBuilder().Construct();
            var listener = transport.MakeListener(localhostEndpoint);

            await listener.StartAsync();

            Assert.AreNotEqual(0, listener.ListenEndpoint.Port);

            await listener.StopAsync();
        }
    }
}
