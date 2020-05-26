

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

namespace deprecated.bondmeta
{
    using System.Collections.Generic;

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class HasMetaFields
    {
        [global::Bond.Id(0), global::Bond.RequiredOptional]
        public string full_name { get; set; }

        [global::Bond.Id(1), global::Bond.RequiredOptional]
        public string name { get; set; }

        public HasMetaFields()
            : this("deprecated.bondmeta.HasMetaFields", "HasMetaFields")
        {}

        protected HasMetaFields(string fullName, string name)
        {
            full_name = fullName;
            this.name = name;
        }
    }
} // deprecated.bondmeta
