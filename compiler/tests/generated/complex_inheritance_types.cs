

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

namespace Test
{
    using System.Collections.Generic;

    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public enum TestEnum
    {
        EnumVal1,
        EnumVal2,
        EnumVal3,
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Simple
    {
        [global::Bond.Id(0)]
        public int someInt { get; set; }

        [global::Bond.Id(1)]
        public int anotherInt { get; set; }

        [global::Bond.Id(2), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string someString { get; set; }

        public Simple()
            : this("Test.Simple", "Simple")
        {}

        protected Simple(string fullName, string name)
        {
            someString = "";
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Foo
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string someText { get; set; }

        public Foo()
            : this("Test.Foo", "Foo")
        {}

        protected Foo(string fullName, string name)
        {
            someText = "BaseText1";
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Bar
        : Foo
    {
        [global::Bond.Id(0)]
        public TestEnum testEnum { get; set; }

        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        new public string someText { get; set; }

        [global::Bond.Id(2)]
        public int someInt { get; set; }

        [global::Bond.Id(3), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string moreText { get; set; }

        [global::Bond.Id(4)]
        public List<Simple> someList { get; set; }

        [global::Bond.Id(5), global::Bond.Type(typeof(Dictionary<global::Bond.Tag.wstring, double>))]
        public Dictionary<string, double> someMap { get; set; }

        [global::Bond.Id(6), global::Bond.Type(typeof(HashSet<global::Bond.Tag.wstring>))]
        public HashSet<string> someSet { get; set; }

        public Bar()
            : this("Test.Bar", "Bar")
        {}

        protected Bar(string fullName, string name)
        {
            testEnum = TestEnum.Val2;
            someText = "DerivedText1";
            moreText = "";
            someList = new List<Simple>();
            someMap = new Dictionary<string, double>();
            someSet = new HashSet<string>();
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Baz
        : Bar
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        new public string someText { get; set; }

        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string evenMoreText { get; set; }

        [global::Bond.Id(2), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string someText1 { get; set; }

        public Baz()
            : this("Test.Baz", "Baz")
        {}

        protected Baz(string fullName, string name)
        {
            someText = "";
            evenMoreText = "";
            someText1 = "";
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class DerivedEmpty
        : Foo
    {
        
    }
} // Test
