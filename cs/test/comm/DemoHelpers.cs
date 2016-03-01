namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond;
    using Bond.Comm.Interfaces;

    class SomeLayer : ILayer<Bond.Void, Bond.Void>
    {
        public void OnReceive(Message<Bond.Void, Bond.Void> message)
        {
            throw new NotImplementedException();
        }

        public void OnSend(Message<Bond.Void, Bond.Void> message)
        {
            throw new NotImplementedException();
        }
    }

    class TcpTransport : Transport
    {
        public override TransportArgs DefaultMessageArgs
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public override Task<Connection> ConnectToAsync(string address)
        {
            throw new NotImplementedException();
        }

        public override Task<Connection> ConnectToAsync(string address, CancellationToken ct)
        {
            throw new NotImplementedException();
        }

        public override Listener MakeListener(string address)
        {
            throw new NotImplementedException();
        }

        public override Task StopAsync()
        {
            throw new NotImplementedException();
        }
    }

    class TcpTransportBuilder : TransportBuilder<TcpTransport>
    {
        public override TransportBuilder<TcpTransport> AddDeserializer<TReader>(Type type, Bond.Deserializer<TReader> deserializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddDeserializers<TReader>(Dictionary<Type, Bond.Deserializer<TReader>> deserializers)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddLayer<TPayload, TLayer>(ILayer<TPayload, TLayer> layer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddSerializer<TWriter>(Type type, Bond.Serializer<TWriter> serializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddSerializers<TWriter>(Dictionary<Type, Bond.Serializer<TWriter>> serializers)
        {
            throw new NotImplementedException();
        }

        public override TcpTransport Construct()
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> SetDefaultMessageArgs(TransportArgs defaults)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> SetUnhandledExceptionHandler(UnhandledExceptionHandler handler)
        {
            throw new NotImplementedException();
        }
    }

    public class TcpConnection : Connection, IRequestResponseConnection, IEventConnection
    {
        public override void AddService<T>(T server)
        {
            throw new NotImplementedException();
        }

        public Task FireEventAsync<TPayload, TLayer>(Message<TPayload, TLayer> message, CancellationToken ct)
        {
            throw new NotImplementedException();
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public Task<Message<TResponse, TLayer>> RequestResponseAsync<TRequest, TResponse, TLayer>(Message<TRequest, TLayer> message, CancellationToken ct)
        {
            throw new NotImplementedException();
        }

        public override Task StopAsync()
        {
            throw new NotImplementedException();
        }
    }
}
