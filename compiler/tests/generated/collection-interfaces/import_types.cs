

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

namespace import_test
{
    using System.Collections.Generic;

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class HasEmpty
    {
        [global::Bond.Id(0)]
        public global::empty.Empty e { get; set; }

        public HasEmpty()
            : this("import_test.HasEmpty", "HasEmpty")
        {}

        protected HasEmpty(string fullName, string name)
        {
            e = new global::empty.Empty();
        }
    }
} // import_test
