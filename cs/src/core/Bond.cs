// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System.Collections.Generic;

    public partial class Metadata
    {
        public override string ToString()
        {
            return name;
        }
    }

    public partial class FieldDef
    {
        public override string ToString()
        {
            return metadata.name;
        }
    }

    public partial class StructDef
    {
        public override string ToString()
        {
            return metadata.qualified_name;
        }
    }

    public partial class SchemaDef
    {
        public override string ToString()
        {
            return structs[root.struct_def].ToString();
        }
    }

    internal static class TypeDefExtensions
    {
        // We intentionally don't implement GetHashCode override for Bond generated classes like 
        // TypeDef because they are mutable and we can't guarantee value semantics in general case. 
        // CalculateHashCode is used internally by parsers to implement their own GetHashCode 
        // override which some transforms expect.
        internal static int CalculateHashCode(this TypeDef typeDef)
        {
            return (typeDef == null) ? 0 : (typeDef.struct_def | ((int)typeDef.id << 16))
                ^ typeDef.element.CalculateHashCode() ^ typeDef.key.CalculateHashCode();
        }
    }

    internal class TypeDefComparer : IEqualityComparer<RuntimeSchema>
    {
        public bool Equals(RuntimeSchema x, RuntimeSchema y)
        {
            return Comparer.Equal(x.TypeDef, y.TypeDef);
        }

        public int GetHashCode(RuntimeSchema x)
        {
            return x.TypeDef.CalculateHashCode();
        }
    }
}
