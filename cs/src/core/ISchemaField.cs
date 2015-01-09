// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Reflection;

    /// <summary>
    /// ISchemaField abstracts a field or property representing a schema field.
    /// </summary>
    /// <remarks>
    /// Schema fields on a compiled Type may be represented as class fields or as properties. An ISchemaField may represent either one.
    /// </remarks>
    public interface ISchemaField
    {
        ushort Id { get; }

        string Name { get; }

        Type MemberType { get; }

        Type DeclaringType { get; }

        MemberInfo MemberInfo { get; }

        object GetValue(object o);
    }
}
