

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

    [global::Bond.Attribute("EnumAttribute1", "one")]
    [global::Bond.Attribute("EnumAttribute2", "two")]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public enum Enum
    {
        Value1,
    }

    [global::Bond.Attribute("StructAttribute1", "one")]
    [global::Bond.Attribute("StructAttribute2", "two")]
    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Foo
    {
        [global::Bond.Attribute("FieldAttribute1", "one")]
        [global::Bond.Attribute("FieldAttribute2", "two")]
        [global::Bond.Id(0)]
        public string f { get; set; }

        public Foo()
            : this("tests.Foo", "Foo")
        {}

        protected Foo(string fullName, string name)
        {
            f = "";
        }
    }
} // tests
