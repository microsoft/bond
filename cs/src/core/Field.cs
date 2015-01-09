// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Reflection;

    internal class Field : ISchemaField
    {
        readonly FieldInfo fieldInfo;
        readonly ushort id;

        public Field(FieldInfo fieldInfo, ushort id)
        {
            this.fieldInfo = fieldInfo;
            this.id = id;
        }

        public ushort Id { get { return id; } }

        public string Name { get { return fieldInfo.Name; } }

        public Type MemberType { get { return fieldInfo.FieldType; } }

        public Type DeclaringType { get { return fieldInfo.DeclaringType; } }

        public MemberInfo MemberInfo { get { return fieldInfo; } }

        public object GetValue(object o)
        {
            return fieldInfo.GetValue(o);
        }
    }
}
