namespace UnitTest
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond;
    using Bond.Comm.Interfaces;

    class SomeOtherLayer : ILayer<TracingState>
    {
        public void OnReceive(Message<Bond.Void> message, ReceiveContext context, ref TracingState layerData)
        {
            throw new NotImplementedException();
        }

        public void OnSend(Message<Bond.Void> message, SendContext context, ref TracingState layerData)
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

    class TcpTransportArgs : TransportArgs
    {
    }

    class TcpTransportBuilder : TransportBuilder<TcpTransport>
    {
        public override TransportBuilder<TcpTransport> AddDeserializer<TReader>(Type type, Deserializer<TReader> deserializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddDeserializers<TReader>(Dictionary<Type, Deserializer<TReader>> deserializers)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddSerializer<TWriter>(Type type, Serializer<TWriter> serializer)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> AddSerializers<TWriter>(Dictionary<Type, Serializer<TWriter>> serializers)
        {
            throw new NotImplementedException();
        }

        public override TcpTransport Construct()
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> SetDefaultTransportArgs(TransportArgs defaults)
        {
            throw new NotImplementedException();
        }

        public override TransportBuilder<TcpTransport> SetLayerStack<TLayerData>(LayerStack<TLayerData> layerStack)
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

        public Task FireEventAsync<TPayload>(Message<TPayload> message, CancellationToken ct)
        {
            throw new NotImplementedException();
        }

        public override void RemoveService<T>(T service)
        {
            throw new NotImplementedException();
        }

        public Task<Message<TResponse>> RequestResponseAsync<TRequest, TResponse>(Message<TRequest> message, CancellationToken ct)
        {
            throw new NotImplementedException();
        }

        public override Task StopAsync()
        {
            throw new NotImplementedException();
        }
    }
}
