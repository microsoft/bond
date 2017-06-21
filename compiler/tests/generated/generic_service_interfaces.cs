

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.9.0.0")]
    public interface IFoo<Payload>
    {
        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<global::Bond.Void>> foo31Async(global::Bond.Comm.IMessage<Payload> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo32Async(global::Bond.Comm.IMessage<global::Bond.Void> param, global::System.Threading.CancellationToken ct);

        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Payload>> foo33Async(global::Bond.Comm.IMessage<Payload> param, global::System.Threading.CancellationToken ct);
    }
    
} // tests
