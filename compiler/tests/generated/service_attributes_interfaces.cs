

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.10.2.0")]
    public interface IFoo
    {
        [global::Bond.Attribute("foo", "method")]
        [global::Bond.Attribute("method", "")]
        global::System.Threading.Tasks.Task<global::Bond.Comm.IMessage<Result>> fooAsync(global::Bond.Comm.IMessage<Param> param, global::System.Threading.CancellationToken ct);
    }
    
} // tests
