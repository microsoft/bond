

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
    public partial class Base
    {
        [global::Bond.Id(0)]
        public int x { get; set; }

        public Base()
            : this("tests.Base", "Base")
        {}

        protected Base(string fullName, string name)
        {
            
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Foo
        : Base
    {
        [global::Bond.Id(0)]
        new public int x { get; set; }

        public Foo()
            : this("tests.Foo", "Foo")
        {}

        protected Foo(string fullName, string name)
        {
            
        }
    }
} // tests
