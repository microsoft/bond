

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
    public partial class Foo<T1, T2>
        where T2 : struct
    {
        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.structT))]
        public T2 t2 { get; set; }

        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.nullable<Foo<global::Bond.Tag.classT, bool>>))]
        public Foo<T1, bool> n { get; set; }

        public Foo()
            : this("tests.Foo", "Foo")
        {}

        protected Foo(string fullName, string name)
        {
            t2 = global::Bond.GenericFactory.Create<T2>();
        }
    }
} // tests
