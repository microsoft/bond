

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
    public static class Foo 
    {
        static readonly string ServiceName = "tests.Foo";

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo31 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.ServerStreaming,
            ServiceName,
            "foo31",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo32 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.ServerStreaming,
            ServiceName,
            "foo32",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo33 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.ClientStreaming,
            ServiceName,
            "foo33",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo34 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.ClientStreaming,
            ServiceName,
            "foo34",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo35 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.DuplexStreaming,
            ServiceName,
            "foo35",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<stream>, global::Bond.Grpc.IMessage<stream>> Method_shouldBeUnary = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<stream>, global::Bond.Grpc.IMessage<stream>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "shouldBeUnary",
            global::Bond.Grpc.Marshaller<stream>.Instance,
            global::Bond.Grpc.Marshaller<stream>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<stream>, global::Bond.Grpc.IMessage<stream>> Method_shouldBeStreaming = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<stream>, global::Bond.Grpc.IMessage<stream>>(
            global::Grpc.Core.MethodType.DuplexStreaming,
            ServiceName,
            "shouldBeStreaming",
            global::Bond.Grpc.Marshaller<stream>.Instance,
            global::Bond.Grpc.Marshaller<stream>.Instance);

        public abstract class FooBase
        {
            public abstract global::System.Threading.Tasks.Task foo31(global::Bond.Grpc.IMessage<global::Bond.Void> request, Grpc.Core.IAsyncStreamWriter<global::Bond.Grpc.IMessage<BasicTypes>> responses, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task foo32(global::Bond.Grpc.IMessage<global::Bond.Void> request, Grpc.Core.IAsyncStreamWriter<global::Bond.Grpc.IMessage<BasicTypes>> responses, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo33(Grpc.Core.IAsyncStreamReader<global::Bond.Grpc.IMessage<BasicTypes>> requests, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> foo34(Grpc.Core.IAsyncStreamReader<global::Bond.Grpc.IMessage<BasicTypes>> requests, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task foo35(Grpc.Core.IAsyncStreamReader<global::Bond.Grpc.IMessage<BasicTypes>> requests, Grpc.Core.IAsyncStreamWriter<global::Bond.Grpc.IMessage<BasicTypes>> responses, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<stream>> shouldBeUnary(global::Bond.Grpc.IMessage<stream> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task shouldBeStreaming(Grpc.Core.IAsyncStreamReader<global::Bond.Grpc.IMessage<stream>> requests, Grpc.Core.IAsyncStreamWriter<global::Bond.Grpc.IMessage<stream>> responses, global::Grpc.Core.ServerCallContext context);
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

            public virtual global::Grpc.Core.AsyncServerStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>> foo31Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo31Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncServerStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>> foo31Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo31, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncServerStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>> foo32Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo32Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncServerStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>> foo32Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo32, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo33Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo33Async(new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo33Async(global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo33, null, options);
            }

            public virtual global::Grpc.Core.AsyncClientStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> foo34Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo34Async(new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncClientStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> foo34Async(global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncClientStreamingCall(Method_foo34, null, options);
            }

            public virtual global::Grpc.Core.AsyncDuplexStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> foo35Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo35Async(new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncDuplexStreamingCall<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> foo35Async(global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncDuplexStreamingCall(Method_foo35, null, options);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<stream>> shouldBeUnaryAsync(stream request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return shouldBeUnaryAsync(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<stream>> shouldBeUnaryAsync(global::Bond.Grpc.IMessage<stream> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_shouldBeUnary, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncDuplexStreamingCall<global::Bond.Grpc.IMessage<stream>, global::Bond.Grpc.IMessage<stream>> shouldBeStreamingAsync(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return shouldBeStreamingAsync(new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncDuplexStreamingCall<global::Bond.Grpc.IMessage<stream>, global::Bond.Grpc.IMessage<stream>> shouldBeStreamingAsync(global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncDuplexStreamingCall(Method_shouldBeStreaming, null, options);
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
                    .AddMethod(Method_foo34, serviceImpl.foo34)
                    .AddMethod(Method_foo35, serviceImpl.foo35)
                    .AddMethod(Method_shouldBeUnary, serviceImpl.shouldBeUnary)
                    .AddMethod(Method_shouldBeStreaming, serviceImpl.shouldBeStreaming)
                    .Build();
        }
    }


    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.11.0.3")]
    public static class Bar<T> where T : class
    {
        static readonly string ServiceName = "tests.Bar";

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<BasicTypes>> Method_ClientStreaming = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.ClientStreaming,
            ServiceName,
            "ClientStreaming",
            global::Bond.Grpc.Marshaller<T>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<T>> Method_ServerStreaming = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<T>>(
            global::Grpc.Core.MethodType.ServerStreaming,
            ServiceName,
            "ServerStreaming",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<T>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<T>> Method_DuplexStreaming = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<T>>(
            global::Grpc.Core.MethodType.DuplexStreaming,
            ServiceName,
            "DuplexStreaming",
            global::Bond.Grpc.Marshaller<T>.Instance,
            global::Bond.Grpc.Marshaller<T>.Instance);

        public abstract class BarBase
        {
            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> ClientStreaming(Grpc.Core.IAsyncStreamReader<global::Bond.Grpc.IMessage<T>> requests, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task ServerStreaming(global::Bond.Grpc.IMessage<BasicTypes> request, Grpc.Core.IAsyncStreamWriter<global::Bond.Grpc.IMessage<T>> responses, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task DuplexStreaming(Grpc.Core.IAsyncStreamReader<global::Bond.Grpc.IMessage<T>> requests, Grpc.Core.IAsyncStreamWriter<global::Bond.Grpc.IMessage<T>> responses, global::Grpc.Core.ServerCallContext context);
        }

        public class BarClient : global::Grpc.Core.ClientBase<BarClient>
        {
            public BarClient(global::Grpc.Core.Channel channel) : base(channel)
            {
            }

            public BarClient(global::Grpc.Core.CallInvoker callInvoker) : base(callInvoker)
            {
            }

            protected BarClient() : base()
            {
            }

            protected BarClient(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration) : base(configuration)
            {
            }

            public virtual global::Grpc.Core.AsyncClientStreamingCall<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<BasicTypes>> ClientStreamingAsync(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return ClientStreamingAsync(new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncClientStreamingCall<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<BasicTypes>> ClientStreamingAsync(global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncClientStreamingCall(Method_ClientStreaming, null, options);
            }

            public virtual global::Grpc.Core.AsyncServerStreamingCall<global::Bond.Grpc.IMessage<T>> ServerStreamingAsync(BasicTypes request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return ServerStreamingAsync(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncServerStreamingCall<global::Bond.Grpc.IMessage<T>> ServerStreamingAsync(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncServerStreamingCall(Method_ServerStreaming, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncDuplexStreamingCall<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<T>> DuplexStreamingAsync(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return DuplexStreamingAsync(new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncDuplexStreamingCall<global::Bond.Grpc.IMessage<T>, global::Bond.Grpc.IMessage<T>> DuplexStreamingAsync(global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncDuplexStreamingCall(Method_DuplexStreaming, null, options);
            }

            protected override BarClient NewInstance(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration)
            {
                return new BarClient(configuration);
            }
        }

        public static global::Grpc.Core.ServerServiceDefinition BindService(BarBase serviceImpl)
        {
            return global::Grpc.Core.ServerServiceDefinition.CreateBuilder()
                    .AddMethod(Method_ClientStreaming, serviceImpl.ClientStreaming)
                    .AddMethod(Method_ServerStreaming, serviceImpl.ServerStreaming)
                    .AddMethod(Method_DuplexStreaming, serviceImpl.DuplexStreaming)
                    .Build();
        }
    }

} // tests
