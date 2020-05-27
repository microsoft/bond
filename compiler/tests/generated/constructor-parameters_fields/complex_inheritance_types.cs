

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
        public int someInt;

        [global::Bond.Id(1)]
        public int anotherInt;

        [global::Bond.Id(2), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string someString = "";

        public Simple(
            int someInt,
            int anotherInt,
            string someString)
        {
            this.someInt = someInt;
            this.anotherInt = anotherInt;
            this.someString = someString;
        }

        public Simple()
        {
            
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Foo
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string someText = "BaseText1";

        public Foo(
            string someText)
        {
            this.someText = someText;
        }

        public Foo()
        {
            
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Bar
        : Foo
    {
        [global::Bond.Id(0)]
        public TestEnum testEnum = TestEnum.Val2;

        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        new public string someText = "DerivedText1";

        [global::Bond.Id(2)]
        public int someInt;

        [global::Bond.Id(3), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string moreText = "";

        [global::Bond.Id(4)]
        public List<Simple> someList = new List<Simple>();

        [global::Bond.Id(5), global::Bond.Type(typeof(Dictionary<global::Bond.Tag.wstring, double>))]
        public Dictionary<string, double> someMap = new Dictionary<string, double>();

        [global::Bond.Id(6), global::Bond.Type(typeof(HashSet<global::Bond.Tag.wstring>))]
        public HashSet<string> someSet = new HashSet<string>();

        public Bar(
            // Base class parameters
            string someText,

            // This class parameters
            TestEnum testEnum,
            string someText0,
            int someInt,
            string moreText,
            List<Simple> someList,
            Dictionary<string, double> someMap,
            HashSet<string> someSet
        ) : base(
                someText)
        {
            this.testEnum = testEnum;
            this.someText = someText0;
            this.someInt = someInt;
            this.moreText = moreText;
            this.someList = someList;
            this.someMap = someMap;
            this.someSet = someSet;
        }

        public Bar()
        {
            
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class Baz
        : Bar
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        new public string someText = "";

        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string evenMoreText = "";

        [global::Bond.Id(2), global::Bond.Type(typeof(global::Bond.Tag.wstring))]
        public string someText1 = "";

        public Baz(
            // Base class parameters
            string someText,
            TestEnum testEnum,
            string someText0,
            int someInt,
            string moreText,
            List<Simple> someList,
            Dictionary<string, double> someMap,
            HashSet<string> someSet,

            // This class parameters
            string someText1,
            string evenMoreText,
            string someText10
        ) : base(
                someText,
                testEnum,
                someText0,
                someInt,
                moreText,
                someList,
                someMap,
                someSet)
        {
            this.someText = someText1;
            this.evenMoreText = evenMoreText;
            this.someText1 = someText10;
        }

        public Baz()
        {
            
        }
    }

    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.12.1.0")]
    public partial class DerivedEmpty
        : Foo
    {
        

        public DerivedEmpty(
            // Base class parameters
            string someText
        ) : base(
                someText)
        {
            
        }

        public DerivedEmpty()
        {
            
        }
    }
} // Test
