

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
    public interface IFoo
    {
        void foo11Async(global::Bond.Comm.IMessage<global::Bond.Void> param);

        void foo12Async(global::Bond.Comm.IMessage<global::Bond.Void> param);

        void foo12_implAsync(global::Bond.Comm.IMessage<global::Bond.Void> param);

        void foo13Async(global::Bond.Comm.IMessage<BasicTypes> param);

        void foo14Async(global::Bond.Comm.IMessage<dummy> param);

        void foo15Async(global::Bond.Comm.IMessage<global::tests2.OtherBasicTypes> param);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo21Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo22Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo23Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo24Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo31Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo32Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo33Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> _rd_foo33Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> foo34Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo41Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo42Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo43Async(global::Bond.Comm.IMessage<BasicTypes> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<dummy>> foo44Async(global::Bond.Comm.IMessage<dummy> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<BasicTypes>> cqAsync(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);
    }
    
} // tests
