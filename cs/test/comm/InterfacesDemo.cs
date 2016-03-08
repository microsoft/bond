namespace UnitTest
{
    using Bond.Comm.Interfaces;
    using System.Threading;
    using System.Threading.Tasks;
    using System;
    using Bond;
    using System.Runtime.Remoting.Messaging;
    public enum Operation
    {
        Add = 1,
        Sub = 2,
        Mul = 3,
        Div = 4,
    }

    [Bond.Schema]
    public class Request
    {
        [Bond.Id(0)]
        public int x;

        [Bond.Id(1)]
        public int y;

        [Bond.Id(2)]
        public Operation op;
    }

    [Bond.Schema]
    public class Response
    {
        [Bond.Id(0)]
        public int result;
    }

    [Bond.Schema]
    public class TracingState
    {
        [Bond.Id(0)]
        public string requestId;
    }

    // this could also be generated from a .bond file like
    // service Calc
    // {
    //     Response Compute(Request);
    // }
    [Service]
    public interface ICalc
    {
        [ServiceMethod]
        Task<Message<Response>> ComputeAsync(Message<Request> message, CancellationToken ct);
    }

    // This would be similar to the genrated proxy
    // Notice that the requirements of the service dictate what kind of
    // connection is needed.
    public class Calc_Proxy<TConnection> : ICalc where TConnection : IRequestResponseConnection
    {
        public Calc_Proxy(TConnection connection)
        { }

        // helper method so that caller's don't have to always pass a cancellation token
        public Task<Message<Response>> ComputeAsync(Message<Request> message)
        {
            return ComputeAsync(message, CancellationToken.None);
        }

        public Task<Message<Response>> ComputeAsync(Message<Request> message, CancellationToken ct)
        {
            throw new NotImplementedException();
        }
    }

    public static class CommDemo
    {
        public static async void ServerDemo()
        {
            var calcImpl = new CalcImpl();

            var layerStack = new LayerStack<TracingState>()
                .AddLayer(new SomeOtherLayer())
                .AddLayer(new InjectTracingStateLayer());

            var tcpTransport = new TcpTransportBuilder()
                .SetLayerStack(layerStack)
                .SetDefaultTransportArgs(GetDefaultArgsFromConfigFile())
                .Construct();

            var listener = tcpTransport.MakeListener("0.0.0.0:0");
            listener.AddService(calcImpl);
            await listener.StartAsync();

            await DelayUntilShutdownAsync();

            await tcpTransport.StopAsync();
        }

        public static async void ClientDemo()
        {
            var layerStack = new LayerStack<TracingState>()
                .AddLayer(new SomeOtherLayer())
                .AddLayer(new InjectTracingStateLayer());

            var tcpTransport = new TcpTransportBuilder()
                .SetLayerStack(layerStack)
                .SetDefaultTransportArgs(GetDefaultArgsFromConfigFile())
                .Construct();

            var connection = (TcpConnection)(await tcpTransport.ConnectToAsync("127.0.0.1:12345"));

            var proxy = new Calc_Proxy<TcpConnection>(connection);

            var request = new Request { op = Operation.Add, x = 10, y = 20 };
            var resultMessage = await proxy.ComputeAsync(new Message<Request>(request));

            if (resultMessage.IsError)
            {
                // handle error
            }
            else
            {
                var result = resultMessage.Payload.Deserialize<Response>();
                // do cool stuff with result
            }

            await tcpTransport.StopAsync();
        }

        private static TcpTransportArgs GetDefaultArgsFromConfigFile()
        {
            return null;
        }

        private static Task DelayUntilShutdownAsync()
        {
            return null;
        }
    }

    class CalcImpl : ICalc
    {
        public Task<Message<Response>> ComputeAsync(Message<Request> message, CancellationToken ct)
        {
            throw new NotImplementedException();
        }
    }

    class InjectTracingStateLayer : ILayer<TracingState>
    {
        private const string ContextKeyName = "com.microsoft.azure.tracing";

        public void OnReceive(Message<Bond.Void> message, ReceiveContext context, ref TracingState layerData)
        {
            if (layerData == null)
            {
                layerData = new TracingState { requestId = Guid.NewGuid().ToString() };
            }

            CallContext.LogicalSetData(ContextKeyName, layerData);
        }

        public void OnSend(Message<Bond.Void> message, SendContext context, ref TracingState layerData)
        {
            if (layerData == null)
            {
                layerData = CallContext.LogicalGetData(ContextKeyName) as TracingState;
            }

            if (layerData == null)
            {
                // there never was an ID, so assign one now
                layerData = new TracingState { requestId = Guid.NewGuid().ToString() };
            }
            else
            {
                // create a child request ID
                layerData.requestId = ComputeCorrelatedRequestId(layerData.requestId);
            }
        }

        private static string ComputeCorrelatedRequestId(string incommingRequest)
        {
            return incommingRequest.ToUpperInvariant();
        }
    }
}
