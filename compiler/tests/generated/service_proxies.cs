

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.4.1.0")]
    public class FooProxy<TConnection> : IFoo where TConnection : global::Bond.Comm.IEventConnection, global::Bond.Comm.IRequestResponseConnection
    {
        private readonly TConnection m_connection;

        public FooProxy(TConnection connection)
        {
            m_connection = connection;
        }

        public void foo11Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            foo11Async(message);
        }

        public void foo11Async(global::Bond.Comm.IMessage<global::Bond.Void> param)
        {
            m_connection.FireEventAsync<global::Bond.Void>(
                "tests.Foo.foo11",
                param);
        }

        public void foo12Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            foo12Async(message);
        }

        public void foo12Async(global::Bond.Comm.IMessage<global::Bond.Void> param)
        {
            m_connection.FireEventAsync<global::Bond.Void>(
                "tests.Foo.foo12",
                param);
        }

        public void foo13Async(BasicTypes param)
        {
            var message = new global::Bond.Comm.Message<BasicTypes>(param);
            foo13Async(message);
        }

        public void foo13Async(global::Bond.Comm.IMessage<BasicTypes> param)
        {
            m_connection.FireEventAsync<BasicTypes>(
                "tests.Foo.foo13",
                param);
        }

        public void foo14Async(dummy param)
        {
            var message = new global::Bond.Comm.Message<dummy>(param);
            foo14Async(message);
        }

        public void foo14Async(global::Bond.Comm.IMessage<dummy> param)
        {
            m_connection.FireEventAsync<dummy>(
                "tests.Foo.foo14",
                param);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo21Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            return foo21Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo21Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<global::Bond.Void, global::Bond.Void>(
                "tests.Foo.foo21",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo22Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            return foo22Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo22Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<global::Bond.Void, global::Bond.Void>(
                "tests.Foo.foo22",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo23Async(BasicTypes param)
        {
            var message = new global::Bond.Comm.Message<BasicTypes>(param);
            return foo23Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo23Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<BasicTypes, global::Bond.Void>(
                "tests.Foo.foo23",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo24Async(dummy param)
        {
            var message = new global::Bond.Comm.Message<dummy>(param);
            return foo24Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo24Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<dummy, global::Bond.Void>(
                "tests.Foo.foo24",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo31Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            return foo31Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo31Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<global::Bond.Void, BasicTypes>(
                "tests.Foo.foo31",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo32Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            return foo32Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo32Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<global::Bond.Void, BasicTypes>(
                "tests.Foo.foo32",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo33Async(BasicTypes param)
        {
            var message = new global::Bond.Comm.Message<BasicTypes>(param);
            return foo33Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo33Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<BasicTypes, BasicTypes>(
                "tests.Foo.foo33",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo34Async(dummy param)
        {
            var message = new global::Bond.Comm.Message<dummy>(param);
            return foo34Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo34Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<dummy, BasicTypes>(
                "tests.Foo.foo34",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo41Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            return foo41Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo41Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<global::Bond.Void, dummy>(
                "tests.Foo.foo41",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo42Async()
        {
            var message = new global::Bond.Comm.Message<global::Bond.Void>(new global::Bond.Void());
            return foo42Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo42Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<global::Bond.Void, dummy>(
                "tests.Foo.foo42",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo43Async(BasicTypes param)
        {
            var message = new global::Bond.Comm.Message<BasicTypes>(param);
            return foo43Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo43Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<BasicTypes, dummy>(
                "tests.Foo.foo43",
                param,
                ct);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo44Async(dummy param)
        {
            var message = new global::Bond.Comm.Message<dummy>(param);
            return foo44Async(message, global::System.Threading.CancellationToken.None);
        }

        public global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo44Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct)
        {
            return m_connection.RequestResponseAsync<dummy, dummy>(
                "tests.Foo.foo44",
                param,
                ct);
        }
    }
} // tests
