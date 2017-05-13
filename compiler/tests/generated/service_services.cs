

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.8.0.0")]
    public abstract class FooServiceBase : IFoo, global::Bond.Comm.IService
    {
        public global::System.Collections.Generic.IEnumerable<global::Bond.Comm.ServiceMethodInfo> Methods
        {
            get
            {
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo11", Callback = foo11Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.Event};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo12", Callback = foo12Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.Event};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo12_impl", Callback = foo12_implAsync_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.Event};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo13", Callback = foo13Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.Event};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo14", Callback = foo14Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.Event};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo15", Callback = foo15Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.Event};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo21", Callback = foo21Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo22", Callback = foo22Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo23", Callback = foo23Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo24", Callback = foo24Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo31", Callback = foo31Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo32", Callback = foo32Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo33", Callback = foo33Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo._rd_foo33", Callback = _rd_foo33Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo34", Callback = foo34Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo41", Callback = foo41Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo42", Callback = foo42Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo43", Callback = foo43Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo44", Callback = foo44Async_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.cq", Callback = cqAsync_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
            }
        }

        public abstract void foo11Async(global::Bond.Comm.IMessage<global::Bond.Void> param);

        public abstract void foo12Async(global::Bond.Comm.IMessage<global::Bond.Void> param);

        public abstract void foo12_implAsync(global::Bond.Comm.IMessage<global::Bond.Void> param);

        public abstract void foo13Async(global::Bond.Comm.IMessage<BasicTypes> param);

        public abstract void foo14Async(global::Bond.Comm.IMessage<dummy> param);

        public abstract void foo15Async(global::Bond.Comm.IMessage<global::tests2.OtherBasicTypes> param);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo21Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo22Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo23Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo24Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo31Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo32Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo33Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> _rd_foo33Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo34Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo41Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo42Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo43Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo44Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> cqAsync(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        private global::System.Threading.Tasks.Task foo11Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            foo11Async(param.Convert<global::Bond.Void>());
            return global::Bond.Comm.CodegenHelpers.CompletedTask;
        }

        private global::System.Threading.Tasks.Task foo12Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            foo12Async(param.Convert<global::Bond.Void>());
            return global::Bond.Comm.CodegenHelpers.CompletedTask;
        }

        private global::System.Threading.Tasks.Task foo12_implAsync_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            foo12_implAsync(param.Convert<global::Bond.Void>());
            return global::Bond.Comm.CodegenHelpers.CompletedTask;
        }

        private global::System.Threading.Tasks.Task foo13Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            foo13Async(param.Convert<BasicTypes>());
            return global::Bond.Comm.CodegenHelpers.CompletedTask;
        }

        private global::System.Threading.Tasks.Task foo14Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            foo14Async(param.Convert<dummy>());
            return global::Bond.Comm.CodegenHelpers.CompletedTask;
        }

        private global::System.Threading.Tasks.Task foo15Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            foo15Async(param.Convert<global::tests2.OtherBasicTypes>());
            return global::Bond.Comm.CodegenHelpers.CompletedTask;
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo21Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<global::Bond.Void>,
                                                           global::Bond.Comm.IMessage>(
                foo21Async(param.Convert<global::Bond.Void>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo22Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<global::Bond.Void>,
                                                           global::Bond.Comm.IMessage>(
                foo22Async(param.Convert<global::Bond.Void>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo23Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<global::Bond.Void>,
                                                           global::Bond.Comm.IMessage>(
                foo23Async(param.Convert<BasicTypes>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo24Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<global::Bond.Void>,
                                                           global::Bond.Comm.IMessage>(
                foo24Async(param.Convert<dummy>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo31Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<BasicTypes>,
                                                           global::Bond.Comm.IMessage>(
                foo31Async(param.Convert<global::Bond.Void>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo32Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<BasicTypes>,
                                                           global::Bond.Comm.IMessage>(
                foo32Async(param.Convert<global::Bond.Void>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo33Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<BasicTypes>,
                                                           global::Bond.Comm.IMessage>(
                foo33Async(param.Convert<BasicTypes>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> _rd_foo33Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<BasicTypes>,
                                                           global::Bond.Comm.IMessage>(
                _rd_foo33Async(param.Convert<BasicTypes>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo34Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<BasicTypes>,
                                                           global::Bond.Comm.IMessage>(
                foo34Async(param.Convert<dummy>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo41Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<dummy>,
                                                           global::Bond.Comm.IMessage>(
                foo41Async(param.Convert<global::Bond.Void>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo42Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<dummy>,
                                                           global::Bond.Comm.IMessage>(
                foo42Async(param.Convert<global::Bond.Void>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo43Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<dummy>,
                                                           global::Bond.Comm.IMessage>(
                foo43Async(param.Convert<BasicTypes>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> foo44Async_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<dummy>,
                                                           global::Bond.Comm.IMessage>(
                foo44Async(param.Convert<dummy>(), ct));
        }

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> cqAsync_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<BasicTypes>,
                                                           global::Bond.Comm.IMessage>(
                cqAsync(param.Convert<global::Bond.Void>(), ct));
        }
    }
    
} // tests
