

// suppress "Missing XML comment for publicly visible type or member"
#pragma warning disable 1591


#region ReSharper warnings
// ReSharper disable PartialTypeWithSinglePart
// ReSharper disable RedundantNameQualifier
// ReSharper disable InconsistentNaming
// ReSharper disable CheckNamespace
// ReSharper disable UnusedParameter.Local
// ReSharper disable RedundantUsingDirective
#endregion

namespace tests
{
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.9.0.0")]
    public class FooProxy<Payload, TConnection> : IFoo<Payload> where TConnection : global::Bond.Comm.IRequestResponseConnection
    {
        private readonly TConnection m_connection;

        public FooProxy(TConnection connection)
        {
            m_connection = connection;
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo31Async(Payload param)
        {
            var message = new global::Bond.Comm.Message<Payload>(param);
            return foo31Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo31Async(global::Bond.Comm.IMessage<Payload> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<Payload, global::Bond.Void>(
                "tests.Foo",
                "foo31",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo32Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            return foo32Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo32Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<global::Bond.Void, Payload>(
                "tests.Foo",
                "foo32",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo33Async(Payload param)
        {
            var message = new global::Bond.Comm.Message<Payload>(param);
            return foo33Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo33Async(global::Bond.Comm.IMessage<Payload> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<Payload, Payload>(
                "tests.Foo",
                "foo33",
                param,
                ct);
        }
    }
} // tests
