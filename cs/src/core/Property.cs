// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Reflection;
    using Bond.Internal.Reflection;

    internal class Property : ISchemaField
    {
        readonly PropertyInfo propertyInfo;
        readonly ushort id;

        public Property(PropertyInfo propertyInfo, ushort id)
        {
            this.propertyInfo = propertyInfo;
            this.id = id;
        }

        public ushort Id { get { return id; } }

        public string Name { get { return propertyInfo.Name; } }

        public Type MemberType { get { return propertyInfo.PropertyType; } }

        public Type DeclaringType { get { return propertyInfo.DeclaringType; } }

        public MemberInfo MemberInfo { get { return propertyInfo; } }

        public object GetValue(object o)
        {
            return propertyInfo.GetValue(o);
        }
    }
}
