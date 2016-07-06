

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.5.0.0")]
    public abstract class FooServiceBase<Payload> : IFoo<Payload>, global::Bond.Comm.IService
    {
        public global::System.Collections.Generic.IEnumerable<global::Bond.Comm.ServiceMethodInfo> Methods
        {
            get
            {
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo31", Callback = foo31Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo32", Callback = foo32Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo33", Callback = foo33Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
            }
        }

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo31Async(global::Bond.Comm.IMessage<Payload> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo32Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo33Async(global::Bond.Comm.IMessage<Payload> param, global::System.Threading.CancellationToken ct);

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo31Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<global::Bond.Void>,
                                                           global::Bond.Comm.IMessage>(
                foo31Async(param.Convert<Payload>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo32Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<Payload>,
                                                           global::Bond.Comm.IMessage>(
                foo32Async(param.Convert<global::Bond.Void>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo33Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<Payload>,
                                                           global::Bond.Comm.IMessage>(
                foo33Async(param.Convert<Payload>(), ct));
        }
    }
    
} // tests
