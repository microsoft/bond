

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

namespace test
{
    using System.Collections.Generic;

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class foo
    {
        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.nullable<long>))]
        public long? l { get; set; }

        [global::Bond.Id(2), global::Bond.Type(typeof(long))]
        public System.DateTime? t { get; set; }

        public foo()
            : this("test.foo", "foo")
        {}

        protected foo(string fullName, string name)
        {
            
        }
    }
} // test
