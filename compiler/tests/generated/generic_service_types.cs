

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.10.2.0")]
    public partial class SomeBox<T>
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.classT))]
        public T value { get; set; }

        public SomeBox()
            : this("tests.SomeBox", "SomeBox")
        {}

        protected SomeBox(string fullName, string name)
        {
            value = global::Bond.GenericFactory.Create<T>();
        }
    }
} // tests
