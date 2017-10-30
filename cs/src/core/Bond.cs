// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Collections.Generic;

    public partial class GUID
    {
        public static implicit operator Guid(GUID bondGuid)
        {
            return new Guid(
                (int)bondGuid.Data1,
                (short)bondGuid.Data2,
                (short)bondGuid.Data3,
                (byte)(bondGuid.Data4 >> 0),
                (byte)(bondGuid.Data4 >> 8),
                (byte)(bondGuid.Data4 >> 16),
                (byte)(bondGuid.Data4 >> 24),
                (byte)(bondGuid.Data4 >> 32),
                (byte)(bondGuid.Data4 >> 40),
                (byte)(bondGuid.Data4 >> 48),
                (byte)(bondGuid.Data4 >> 56));
        }

        public static implicit operator GUID(Guid systemGuid)
        {
            var bytes = systemGuid.ToByteArray();
            return new GUID
            {
                Data1 = BitConverter.ToUInt32(bytes, 0),
                Data2 = BitConverter.ToUInt16(bytes, 4),
                Data3 = BitConverter.ToUInt16(bytes, 6),
                Data4 = BitConverter.ToUInt64(bytes, 8),
            };
        }
    }

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

    /// <summary>
    /// Contains helper methods for working with <see cref="Bond.Box{T}"/>.
    /// </summary>
    public static class Box
    {
        /// <summary>
        /// Creates a new Box with the given value.
        /// </summary>
        /// <param name="value">The value to put in a Box</param>
        /// <returns>A Box with <paramref name="value"/> in it.</returns>
        public static Box<T> Create<T>(T value)
        {
            return new Box<T> {value = value};
        }
    }
}
