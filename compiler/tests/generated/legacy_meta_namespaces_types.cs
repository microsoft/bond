

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

namespace csharp.tests
{
    using System.Collections.Generic;

    [global::Bond.Namespace("tests")]
    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.4.0.1")]
    public partial class BasicType
    {
        private static readonly string _schemaName = global::Bond.Reflection.GetSchemaLegacyName(typeof(BasicType));
        private static readonly string _schemaFullName = global::Bond.Reflection.GetSchemaLegacyFullName(typeof(BasicType));

        [global::Bond.Id(0)]
        public bool _bool { get; set; }

        public BasicType()
            : this(_schemaFullName, _schemaName)
        {}

        protected BasicType(string fullName, string name)
        {
            
        }
    }

    [global::Bond.Namespace("tests")]
    [global::Bond.Schema]
    [System.CodeDom.Compiler.GeneratedCode("gbc", "0.4.0.1")]
    public partial class Foo<T1, T2>
        where T2 : struct
    {
        private static readonly string _schemaName = global::Bond.Reflection.GetSchemaLegacyName(typeof(Foo<T1, T2>));
        private static readonly string _schemaFullName = global::Bond.Reflection.GetSchemaLegacyFullName(typeof(Foo<T1, T2>));

        [global::Bond.Id(0), global::Bond.Type(typeof(global::Bond.Tag.structT))]
        public T2 t2 { get; set; }

        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.nullable<Foo<global::Bond.Tag.classT, bool>>))]
        public Foo<T1, bool> n { get; set; }

        public Foo()
            : this(_schemaFullName, _schemaName)
        {}

        protected Foo(string fullName, string name)
        {
            t2 = global::Bond.GenericFactory.Create<T2>();
        }
    }
} // csharp.tests
