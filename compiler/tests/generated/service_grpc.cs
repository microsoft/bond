

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

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo11 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo11",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo12 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo12",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo12_impl = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo12_impl",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo13 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo13",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo14 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo14",
            global::Bond.Grpc.Marshaller<dummy>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::tests2.OtherBasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo15 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::tests2.OtherBasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo15",
            global::Bond.Grpc.Marshaller<global::tests2.OtherBasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo21 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo21",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo22 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo22",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo23 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo23",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<global::Bond.Void>> Method_foo24 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<global::Bond.Void>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo24",
            global::Bond.Grpc.Marshaller<dummy>.Instance,
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo31 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo31",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo32 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo32",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo33 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo33",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>> Method__rd_foo33 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "_rd_foo33",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<BasicTypes>> Method_foo34 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo34",
            global::Bond.Grpc.Marshaller<dummy>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<dummy>> Method_foo41 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<dummy>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo41",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<dummy>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<dummy>> Method_foo42 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<dummy>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo42",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<dummy>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<dummy>> Method_foo43 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<BasicTypes>, global::Bond.Grpc.IMessage<dummy>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo43",
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance,
            global::Bond.Grpc.Marshaller<dummy>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<dummy>> Method_foo44 = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<dummy>, global::Bond.Grpc.IMessage<dummy>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "foo44",
            global::Bond.Grpc.Marshaller<dummy>.Instance,
            global::Bond.Grpc.Marshaller<dummy>.Instance);

        static readonly global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>> Method_cq = new global::Grpc.Core.Method<global::Bond.Grpc.IMessage<global::Bond.Void>, global::Bond.Grpc.IMessage<BasicTypes>>(
            global::Grpc.Core.MethodType.Unary,
            ServiceName,
            "cq",
            global::Bond.Grpc.Marshaller<global::Bond.Void>.Instance,
            global::Bond.Grpc.Marshaller<BasicTypes>.Instance);

        public abstract class FooBase
        {
            public abstract global::System.Threading.Tasks.Task foo11(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            internal global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo11_impl(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(foo11, request, context);
            }

            public abstract global::System.Threading.Tasks.Task foo12(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            internal global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo12_impl0(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(foo12, request, context);
            }

            public abstract global::System.Threading.Tasks.Task foo12_impl(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            internal global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo12_impl_impl(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(foo12_impl, request, context);
            }

            public abstract global::System.Threading.Tasks.Task foo13(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.ServerCallContext context);

            internal global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo13_impl(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.ServerCallContext context) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(foo13, request, context);
            }

            public abstract global::System.Threading.Tasks.Task foo14(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.ServerCallContext context);

            internal global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo14_impl(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.ServerCallContext context) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(foo14, request, context);
            }

            public abstract global::System.Threading.Tasks.Task foo15(global::Bond.Grpc.IMessage<global::tests2.OtherBasicTypes> request, global::Grpc.Core.ServerCallContext context);

            internal global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo15_impl(global::Bond.Grpc.IMessage<global::tests2.OtherBasicTypes> request, global::Grpc.Core.ServerCallContext context) {
                return global::Bond.Grpc.Internal.NothingCallHandler.Handle(foo15, request, context);
            }

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo21(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo22(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo23(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<global::Bond.Void>> foo24(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> foo31(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> foo32(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> foo33(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> _rd_foo33(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> foo34(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<dummy>> foo41(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<dummy>> foo42(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<dummy>> foo43(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<dummy>> foo44(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.ServerCallContext context);

            public abstract global::System.Threading.Tasks.Task<global::Bond.Grpc.IMessage<BasicTypes>> cq(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.ServerCallContext context);
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

            public virtual void foo11Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                foo11Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual void foo11Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_foo11, null, options, request);
            }

            public virtual void foo12Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                foo12Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual void foo12Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_foo12, null, options, request);
            }

            public virtual void foo12_implAsync(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                foo12_implAsync(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual void foo12_implAsync(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_foo12_impl, null, options, request);
            }

            public virtual void foo13Async(BasicTypes request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                foo13Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual void foo13Async(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_foo13, null, options, request);
            }

            public virtual void foo14Async(dummy request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                foo14Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual void foo14Async(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_foo14, null, options, request);
            }

            public virtual void foo15Async(global::tests2.OtherBasicTypes request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                foo15Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual void foo15Async(global::Bond.Grpc.IMessage<global::tests2.OtherBasicTypes> request, global::Grpc.Core.CallOptions options)
            {
                global::Bond.Grpc.Internal.NothingCallInvoker.NothingCall(CallInvoker, Method_foo15, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo21Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo21Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo21Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo21, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo22Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo22Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo22Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo22, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo23Async(BasicTypes request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo23Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo23Async(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo23, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo24Async(dummy request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo24Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<global::Bond.Void>> foo24Async(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo24, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo31Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo31Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo31Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo31, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo32Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo32Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo32Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo32, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo33Async(BasicTypes request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo33Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo33Async(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo33, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> _rd_foo33Async(BasicTypes request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return _rd_foo33Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> _rd_foo33Async(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method__rd_foo33, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo34Async(dummy request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo34Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> foo34Async(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo34, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo41Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo41Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo41Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo41, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo42Async(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo42Async(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo42Async(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo42, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo43Async(BasicTypes request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo43Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo43Async(global::Bond.Grpc.IMessage<BasicTypes> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo43, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo44Async(dummy request, global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return foo44Async(global::Bond.Grpc.Message.From(request), new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<dummy>> foo44Async(global::Bond.Grpc.IMessage<dummy> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_foo44, null, options, request);
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> cqAsync(global::Grpc.Core.Metadata headers = null, global::System.DateTime? deadline = null, global::System.Threading.CancellationToken cancellationToken = default(global::System.Threading.CancellationToken))
            {
                return cqAsync(global::Bond.Grpc.Message.Void, new global::Grpc.Core.CallOptions(headers, deadline, cancellationToken));
            }

            public virtual global::Grpc.Core.AsyncUnaryCall<global::Bond.Grpc.IMessage<BasicTypes>> cqAsync(global::Bond.Grpc.IMessage<global::Bond.Void> request, global::Grpc.Core.CallOptions options)
            {
                return CallInvoker.AsyncUnaryCall(Method_cq, null, options, request);
            }

            protected override FooClient NewInstance(global::Grpc.Core.ClientBase.ClientBaseConfiguration configuration)
            {
                return new FooClient(configuration);
            }
        }

        public static global::Grpc.Core.ServerServiceDefinition BindService(FooBase serviceImpl)
        {
            return global::Grpc.Core.ServerServiceDefinition.CreateBuilder()
                    .AddMethod(Method_foo11, serviceImpl.foo11_impl)
                    .AddMethod(Method_foo12, serviceImpl.foo12_impl0)
                    .AddMethod(Method_foo12_impl, serviceImpl.foo12_impl_impl)
                    .AddMethod(Method_foo13, serviceImpl.foo13_impl)
                    .AddMethod(Method_foo14, serviceImpl.foo14_impl)
                    .AddMethod(Method_foo15, serviceImpl.foo15_impl)
                    .AddMethod(Method_foo21, serviceImpl.foo21)
                    .AddMethod(Method_foo22, serviceImpl.foo22)
                    .AddMethod(Method_foo23, serviceImpl.foo23)
                    .AddMethod(Method_foo24, serviceImpl.foo24)
                    .AddMethod(Method_foo31, serviceImpl.foo31)
                    .AddMethod(Method_foo32, serviceImpl.foo32)
                    .AddMethod(Method_foo33, serviceImpl.foo33)
                    .AddMethod(Method__rd_foo33, serviceImpl._rd_foo33)
                    .AddMethod(Method_foo34, serviceImpl.foo34)
                    .AddMethod(Method_foo41, serviceImpl.foo41)
                    .AddMethod(Method_foo42, serviceImpl.foo42)
                    .AddMethod(Method_foo43, serviceImpl.foo43)
                    .AddMethod(Method_foo44, serviceImpl.foo44)
                    .AddMethod(Method_cq, serviceImpl.cq)
                    .Build();
        }
    }

} // tests
