namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.Net;
    using System.Threading;
    using System.Threading.Tasks;

    using Bond;
    using Bond.Comm;
    using Bond.Comm.Tcp;
    using NUnit.Framework;

    [TestFixture]
    public class TcpTransportTests
    {
        private const string AnyIpAddressString = "10.1.2.3";
        private const int AnyPort = 12345;
        private readonly IPAddress AnyIpAddress = new IPAddress(new byte[] { 10, 1, 2, 3 });

        [Test]
        public void DefaultPorts_AreExpected()
        {
            Assert.AreEqual(25188, TcpTransport.DefaultPort);
        }

        [Test]
        public void ParseStringAddress_NullOrEmpty_Throws()
        {
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress(null));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress(string.Empty));
        }

        [Test]
        public void ParseStringAddress_ValidIpNoPort_ReturnsIpEndpoint()
        {
            var result = TcpTransport.ParseStringAddress(AnyIpAddressString);
            Assert.AreEqual(new IPEndPoint(AnyIpAddress, TcpTransport.DefaultPort), result);
        }

        [Test]
        public void ParseStringAddress_ValidIpWithPort_ReturnsIpEndpoint()
        {
            var result = TcpTransport.ParseStringAddress(AnyIpAddressString + ":" + AnyPort);
            Assert.AreEqual(new IPEndPoint(AnyIpAddress, AnyPort), result);
        }

        [Test]
        public void ParseStringAddress_InvalidIpAddress_Throws()
        {
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("not an ip"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("not an ip:12345"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("not an ip:no a port"));
        }

        [Test]
        public void ParseStringAddress_InvalidPortAddress_Throws()
        {
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("10.1.2.3:"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("10.1.2.3::"));
            Assert.Throws<ArgumentException>(() => TcpTransport.ParseStringAddress("10.1.2.3:not a port"));
        }

        [Test]
        public async Task SetupListener_RequestReply_Works()
        {
            var testService = new TestService();

            TcpTransport transport = new TcpTransportBuilder().Construct();
            TcpListener listener = transport.MakeListener(new IPEndPoint(IPAddress.Loopback, 0));
            listener.AddService(testService);
            await listener.StartAsync();

            TcpConnection connection = await transport.ConnectToAsync(listener.ListenEndpoint);

            var message = new Message<Bond.Void>(new Bond.Void());
            var response = await connection.RequestResponseAsync<Bond.Void, Bond.Void>("TestService.RespondWithEmpty", message, CancellationToken.None);

            Assert.IsFalse(response.IsError);
            Assert.IsNotNull(response.Payload);
            Assert.IsNull(response.Error);

            Assert.AreEqual(1, testService.CallCount);

            var responseVoid = response.Payload.Deserialize<Bond.Void>();

            await transport.StopAsync();
        }

        private class TestService : IService
        {
            private int m_callCount = 0;

            public IEnumerable<ServiceMethodInfo> Methods
            {
                get
                {
                    return new[]
                    {
                        new ServiceMethodInfo
                        {
                            MethodName = "TestService.RespondWithEmpty",
                            Callback = RespondWithEmpty
                        }
                    };
                }
            }

            public int CallCount
            {
                get
                {
                    return m_callCount;
                }
            }

            private Task<IBonded> RespondWithEmpty(IBonded request, ReceiveContext context)
            {
                Interlocked.Increment(ref m_callCount);
                return Task.FromResult<IBonded>(new Bonded<Bond.Void>(new Bond.Void()));
            }
        }
    }
}
