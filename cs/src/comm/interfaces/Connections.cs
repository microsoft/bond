// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System.Threading;
    using System.Threading.Tasks;

    public interface IRequestResponseConnection
    {
        Task<IMessage<TResponse>> RequestResponseAsync<TRequest, TResponse>(string methodName, IMessage<TRequest> message, CancellationToken ct);
    }

    public interface IEventConnection
    {
        Task FireEventAsync<TPayload>(string methodName, IMessage<TPayload> message, CancellationToken ct);
    }

    public abstract class Connection
    {
        // connection-specific TransportArgs. If null, will default to the transport's settings.
        public TransportArgs DefaultTransportArgs { get; set; }

        public abstract Task StopAsync();

        // register/remove per-connections services, like for server-to-client callbacks
        // won't be implemented in first version
        public abstract void AddService<T>(T server);
        public abstract void RemoveService<T>(T service);
    }
}
