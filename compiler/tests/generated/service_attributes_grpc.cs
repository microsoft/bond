

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.11.0.3")]
    public static class Foo 
    {
        static readonly string ServiceName = "tests.Foo";

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<Param>, global::Bond.Grpc.IMessage<Result>> Method_foo = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<Param>, global::Bond.Grpc.IMessage<Result>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo",
            global::Bond.Grpc.Marshaller<Param>.Instance,
            global::Bond.Grpc.Marshaller<Result>.Instance);

        public abstract class FooBase
        {
            [global::Bond.Attribute("foo", "method")]
            [global::Bond.Attribute("method", "")]
            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<Result>> foo(global::Bond.Grpc.IMessage<Param> request, global::Grpc.Core.ServerCallContext context);
        }

        public class FooClient : global::Grpc.Core.ClientBase<FooClient>
        {
            public FooClient(global::Grpc.Core.Channel channel) : base(channel)
            {
            }

            public FooClient(global::Grpc.Core.CallInvoker callInvoker) : base(callInvoker)
            {
            }

            protected FooClient() : base()
            {
            }

            protected FooClient(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration) : base(configuration)
            {
            }

            [global::Bond.Attribute("foo", "method")]
            [global::Bond.Attribute("method", "")]
            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<Result>> fooAsync(Param request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return fooAsync(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            [global::Bond.Attribute("foo", "method")]
            [global::Bond.Attribute("method", "")]
            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<Result>> fooAsync(global::Bond.Grpc.IMessage<Param> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo, null, options, request);
            }

            protected override FooClient NewInstance(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration)
            {
                return new FooClient(configuration);
            }
        }

        public static global::Grpc.Core.ServerServiceDefinition BindService(FooBase serviceImpl)
        {
            return global::Grpc.Core.ServerServiceDefinition.CreateBuilder()
                    .AddMethod(Method_foo, serviceImpl.foo)
                    .Build();
        }
    }

} // tests
