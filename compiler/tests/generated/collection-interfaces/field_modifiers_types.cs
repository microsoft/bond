

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

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Foo
    {
        [global::Bond.Id(0)]
        public bool o { get; set; }

        [global::Bond.Id(1), global::Bond.Required]
        public short r { get; set; }

        [global::Bond.Id(2), global::Bond.RequiredOptional]
        public double ro { get; set; }

        public Foo()
            : this("tests.Foo", "Foo")
        {}

        protected Foo(string fullName, string name)
        {
            
        }
    }
} // tests
