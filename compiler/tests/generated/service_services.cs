

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.4.0.2")]
    public abstract class FooService : IFoo, global::Bond.Comm.IService
    {
        public global::System.Collections.Generic.IEnumerable<global::Bond.Comm.ServiceMethodInfo> Methods
        {
            get
            {
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo21", Callback = foo21Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo22", Callback = foo22Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo23", Callback = foo23Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo24", Callback = foo24Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo31", Callback = foo31Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo32", Callback = foo32Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo33", Callback = foo33Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo34", Callback = foo34Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo41", Callback = foo41Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo42", Callback = foo42Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo43", Callback = foo43Async_Glue};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo44", Callback = foo44Async_Glue};
            }
        }

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo21Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo22Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo23Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo24Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo31Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo32Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo33Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo34Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo41Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo42Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo43Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo44Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo21Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo21Async(param.Convert<global::Bond.Void>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo22Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo22Async(param.Convert<global::Bond.Void>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo23Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo23Async(param.Convert<BasicTypes>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo24Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo24Async(param.Convert<dummy>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo31Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo31Async(param.Convert<global::Bond.Void>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo32Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo32Async(param.Convert<global::Bond.Void>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo33Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo33Async(param.Convert<BasicTypes>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo34Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo34Async(param.Convert<dummy>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo41Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo41Async(param.Convert<global::Bond.Void>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo42Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo42Async(param.Convert<global::Bond.Void>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo43Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo43Async(param.Convert<BasicTypes>(), ct);
        }

        private async global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo44Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return await foo44Async(param.Convert<dummy>(), ct);
        }
    }
    
} // tests
