

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.13.0.0")]
    public partial class Foo
    {
        
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.13.0.0")]
    public partial class ComplexTypes
    {
        [global::Bond.Id(0)]
        public LinkedList<sbyte> li8 { get; set; }

        [global::Bond.Id(1)]
        public HashSet<bool> sb { get; set; }

        [global::Bond.Id(2)]
        public List<System.ArraySegment<byte>> vb { get; set; }

        [global::Bond.Id(3), global::Bond.Type(typeof(global::Bond.Tag.nullable<float>))]
        public float? nf { get; set; }

        [global::Bond.Id(4), global::Bond.Type(typeof(Dictionary<string, global::Bond.Tag.wstring>))]
        public Dictionary<string, string> msws { get; set; }

        [global::Bond.Id(5)]
        public global::Bond.IBonded<Foo> bfoo { get; set; }

        [global::Bond.Id(6), global::Bond.Type(typeof(Dictionary<double, LinkedList<List<global::Bond.Tag.nullable<global::Bond.IBonded<Bar>>>>>))]
        public Dictionary<double, LinkedList<List<global::Bond.IBonded<Bar>>>> m { get; set; }

        public ComplexTypes()
            : this("tests.ComplexTypes", "ComplexTypes")
        {}

        protected ComplexTypes(string fullName, string name)
        {
            li8 = new LinkedList<sbyte>();
            sb = new HashSet<bool>();
            vb = new List<System.ArraySegment<byte>>();
            msws = new Dictionary<string, string>();
            bfoo = global::Bond.Bonded<Foo>.Empty;
            m = new Dictionary<double, LinkedList<List<global::Bond.IBonded<Bar>>>>();
        }
    }
} // tests
