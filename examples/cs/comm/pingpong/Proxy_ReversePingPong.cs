// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// hand-written proxy. This will be generated eventually, either via gbc or reflection or some combination of.
namespace Bond.Examples.PingPong
{
    using System.Threading;
    using System.Threading.Tasks;

    using Bond.Comm;

    public class Proxy_ReversePingPong : IPingPongService
    {
        private readonly IRequestResponseConnection m_connection;

        public Proxy_ReversePingPong(IRequestResponseConnection connection)
        {
            m_connection = connection;
        }

        public Task<IMessage<PingResponse>> PingAsync(PingRequest request)
        {
            var message = new Message<PingRequest>(request);
            return PingAsync(message);
        }

        public Task<IMessage<PingResponse>> PingAsync(IMessage<PingRequest> message)
        {
            return m_connection.RequestResponseAsync<PingRequest, PingResponse>(
                "Bond.Examples.ReversePingPong.Ping",
                message,
                CancellationToken.None);
        }
    }
}
