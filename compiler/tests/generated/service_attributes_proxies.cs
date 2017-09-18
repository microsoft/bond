

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.10.0.0")]
    public class FooProxy<TConnection> : IFoo where TConnection : global::Bond.Comm.IRequestResponseConnection
    {
        private readonly TConnection m_connection;

        public FooProxy(TConnection connection)
        {
            m_connection = connection;
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Result>> fooAsync(Param param)
        {
            var message = new global::Bond.Comm.Message<Param>(param);
            return fooAsync(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Result>> fooAsync(global::Bond.Comm.IMessage<Param> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<Param, Result>(
                "tests.Foo",
                "foo",
                param,
                ct);
        }
    }
} // tests
