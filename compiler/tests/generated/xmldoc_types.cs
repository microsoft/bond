

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

namespace example.some
{
    using System.Collections.Generic;

    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.0.1")]
    public enum FooBar
    {
        Foo = unchecked((int)1),
        Bar = unchecked((int)2),
        Baz = unchecked((int)3),
    }

    /// This is my struct
    /// 2nd line
    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.0.1")]
    public partial class SomeStruct
    {
        /// some field
        /// <code>someField = 42</code>
        /// last line
        [global::Bond.Id(0)]
        public int someField { get; set; }

        /// not ignored
        [global::Bond.Id(1)]
        public int otherField { get; set; }

        [global::Bond.Id(2)]
        public int notDocumented { get; set; }

        public SomeStruct()
            : this("example.some.SomeStruct", "SomeStruct")
        {}

        protected SomeStruct(string fullName, string name)
        {
            someField = 123;
            otherField = 234;
            notDocumented = 345;
        }
    }
} // example.some
