

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
    public partial class ImmutableCollectionsHolder
    {
        [global::Bond.Id(0)]
        public System.Collections.Immutable.ImmutableArray<string> ImmutableArrayString { get; set; }

        [global::Bond.Id(1)]
        public System.Collections.Immutable.ImmutableList<string> ImmutableListString { get; set; }

        [global::Bond.Id(2)]
        public System.Collections.Immutable.ImmutableHashSet<string> ImmutableHashSetString { get; set; }

        [global::Bond.Id(3)]
        public System.Collections.Immutable.ImmutableSortedSet<int> ImmutableSortedSetInt { get; set; }

        [global::Bond.Id(4)]
        public System.Collections.Immutable.ImmutableDictionary<string,string> ImmutableDictionaryStringMap { get; set; }

        [global::Bond.Id(5)]
        public System.Collections.Immutable.ImmutableSortedDictionary<int,string> ImmutableSortedDictionaryStringMap { get; set; }

        public ImmutableCollectionsHolder()
            : this("tests.ImmutableCollectionsHolder", "ImmutableCollectionsHolder")
        {}

        protected ImmutableCollectionsHolder(string fullName, string name)
        {
            ImmutableArrayString = System.Collections.Immutable.ImmutableArray<string>.Empty;
            ImmutableListString = System.Collections.Immutable.ImmutableList<string>.Empty;
            ImmutableHashSetString = System.Collections.Immutable.ImmutableHashSet<string>.Empty;
            ImmutableSortedSetInt = System.Collections.Immutable.ImmutableSortedSet<int>.Empty;
            ImmutableDictionaryStringMap = System.Collections.Immutable.ImmutableDictionary<string,string>.Empty;
            ImmutableSortedDictionaryStringMap = System.Collections.Immutable.ImmutableSortedDictionary<int,string>.Empty;
        }
    }
} // tests
