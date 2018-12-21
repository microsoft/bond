

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

    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.11.0.3")]
    public static class Foo<Payload> where Payload : class
    {
        static readonly string ServiceName = "tests.Foo";

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<Payload>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo31 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<Payload>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo31",
            global::Bond.Grpc.Marshaller<Payload>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<Payload>> Method_foo32 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<Payload>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo32",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<Payload>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<Payload>, global::Bond.Grpc.IMessage<Payload>> Method_foo33 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<Payload>, global::Bond.Grpc.IMessage<Payload>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo33",
            global::Bond.Grpc.Marshaller<Payload>.Instance,
            global::Bond.Grpc.Marshaller<Payload>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<SomeBox<int>>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_ConsumesGeneric1 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<SomeBox<int>>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "ConsumesGeneric1",
            global::Bond.Grpc.Marshaller<SomeBox<int>>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<SomeBox<List<int>>>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_ConsumesGeneric2 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<SomeBox<List<int>>>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "ConsumesGeneric2",
            global::Bond.Grpc.Marshaller<SomeBox<List<int>>>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        public abstract class FooBase
        {
            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo31(global::Bond.Grpc.IMessage<Payload> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<Payload>> foo32(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<Payload>> foo33(global::Bond.Grpc.IMessage<Payload> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> ConsumesGeneric1(global::Bond.Grpc.IMessage<SomeBox<int>> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> ConsumesGeneric2(global::Bond.Grpc.IMessage<SomeBox<List<int>>> request, global::Grpc.Core.ServerCallContext context);
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

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo31Async(Payload request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo31Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo31Async(global::Bond.Grpc.IMessage<Payload> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo31, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<Payload>> foo32Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo32Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<Payload>> foo32Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo32, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<Payload>> foo33Async(Payload request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo33Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<Payload>> foo33Async(global::Bond.Grpc.IMessage<Payload> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo33, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> ConsumesGeneric1Async(SomeBox<int> request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return ConsumesGeneric1Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> ConsumesGeneric1Async(global::Bond.Grpc.IMessage<SomeBox<int>> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_ConsumesGeneric1, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> ConsumesGeneric2Async(SomeBox<List<int>> request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return ConsumesGeneric2Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> ConsumesGeneric2Async(global::Bond.Grpc.IMessage<SomeBox<List<int>>> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_ConsumesGeneric2, null, options, request);
            }

            protected override FooClient NewInstance(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration)
            {
                return new FooClient(configuration);
            }
        }

        public static global::Grpc.Core.ServerServiceDefinition BindService(FooBase serviceImpl)
        {
            return global::Grpc.Core.ServerServiceDefinition.CreateBuilder()
                    .AddMethod(Method_foo31, serviceImpl.foo31)
                    .AddMethod(Method_foo32, serviceImpl.foo32)
                    .AddMethod(Method_foo33, serviceImpl.foo33)
                    .AddMethod(Method_ConsumesGeneric1, serviceImpl.ConsumesGeneric1)
                    .AddMethod(Method_ConsumesGeneric2, serviceImpl.ConsumesGeneric2)
                    .Build();
        }
    }

} // tests
