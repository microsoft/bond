

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
    using System.Collections.Generic;

    [global::Bond.Attribute("FooAttribute", "Bar")]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.10.1.0")]
    public abstract class FooServiceBase : IFoo, global::Bond.Comm.IService
    {
        public global::System.Collections.Generic.IEnumerable<global::Bond.Comm.ServiceMethodInfo> Methods
        {
            get
            {
                yield return new global::Bond.Comm.ServiceMethodInfo {MethodName="tests.Foo.foo", Callback = fooAsync_Glue, CallbackType = global::Bond.Comm.ServiceCallbackType.RequestResponse};
            }
        }

        [global::Bond.Attribute("foo", "method")]
        [global::Bond.Attribute("method", "")]
        public abstract global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Result>> fooAsync(global::Bond.Comm.IMessage<Param> param, global::System.Threading.CancellationToken ct);

        private global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage> fooAsync_Glue(global::Bond.Comm.IMessage param, global::Bond.Comm.ReceiveContext context, global::System.Threading.CancellationToken ct)
        {
            return global::Bond.Comm.CodegenHelpers.Upcast<global::Bond.Comm.IMessage<Result>,
                                                           global::Bond.Comm.IMessage>(
                fooAsync(param.Convert<Param>(), ct));
        }
    }
    
} // tests
