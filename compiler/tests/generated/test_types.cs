

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "3.03")]
    public partial class foo
    {
        [global::Bond.Id(0)]
        public string a { get; set; }

        [global::Bond.Id(1)]
        public short b { get; set; }

        [global::Bond.Id(2)]
        public short c { get; set; }

        [global::Bond.Id(3)]
        public Dictionary<string, bool> m { get; set; }

        [global::Bond.Id(4), global::Bond.Type(typeof(LinkedList<global::Bond.Tag.nullable<bar>>))]
        public LinkedList<bar> n { get; set; }

        [global::Bond.Id(5)]
        public LinkedList<HashSet<float>> l { get; set; }
        
        public foo()
            : this("test.foo", "foo")
        {}

        protected foo(string fullName, string name)
        {
            a = "re";
            b = 10;
            m = new Dictionary<string, bool>();
            n = new LinkedList<bar>();
            l = new LinkedList<HashSet<float>>();
        }
    }
} // test
