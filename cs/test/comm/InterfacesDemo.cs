namespace UnitTest
{
    using Bond.Comm.Interfaces;
    using System.Threading;
    using System.Threading.Tasks;
    using System;

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

            var firstLayer = new SomeLayer();
            var secondLayer = new SomeLayer();

            var tcpTransport = new TcpTransportBuilder()
                .AddLayer(firstLayer)
                .AddLayer(secondLayer)
                .Construct();

            var listener = tcpTransport.MakeListener("0.0.0.0:0");
            listener.AddService(calcImpl);
            await listener.StartAsync();

            // wait for shutdown
            await Task.Delay(500);

            await tcpTransport.StopAsync();
        }

        public static async void ClientDemo()
        {
            var firstLayer = new SomeLayer();
            var secondLayer = new SomeLayer();

            var tcpTransport = new TcpTransportBuilder()
                .AddLayer(firstLayer)
                .AddLayer(secondLayer)
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
    }

    class CalcImpl : ICalc
    {
        public Task<Message<Response>> ComputeAsync(Message<Request> message, CancellationToken ct)
        {
            throw new NotImplementedException();
        }
    }
}
