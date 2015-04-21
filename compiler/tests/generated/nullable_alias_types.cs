

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
    [System.CodeDom.Compiler.GeneratedCode("gbc", "3.05")]
    public partial class foo
    {
        [global::Bond.Id(1), global::Bond.Type(typeof(global::Bond.Tag.nullable<long>))]
        public long? l { get; set; }
        
        public foo()
            : this("test.foo", "foo")
        {}

        protected foo(string fullName, string name)
        {
            
        }
    }
} // test
