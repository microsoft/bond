// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// Even though this file is compiled conditionally, the #if is necessary to unconfuse resharper
#if NET40

namespace Bond
{
    using System;
    using System.Collections.Generic;
    using System.Reflection;
    using System.Linq;

    // Common reflection interface implementation for .NET 4.0
    internal static class Reflection40
    {
        const BindingFlags AllDeclared = BindingFlags.NonPublic | BindingFlags.Public |
            BindingFlags.Instance | BindingFlags.Static | BindingFlags.DeclaredOnly;

        public static Type GetTypeInfo(this Type type)
        {
            return type;
        }

        public static Type[] GetGenericParameters(this Type type)
        {
            return type.GetGenericArguments();
        }

        public static IEnumerable<ConstructorInfo> GetDeclaredConstructors(this Type type)
        {
            return type.GetConstructors(AllDeclared);
        }

        public static IEnumerable<FieldInfo> GetDeclaredFields(this Type type)
        {
            return type.GetFields(AllDeclared);
        }

        public static IEnumerable<PropertyInfo> GetDeclaredProperties(this Type type)
        {
            return type.GetProperties(AllDeclared);
        }

        public static PropertyInfo GetDeclaredProperty(this Type type, string name)
        {
            return type.GetProperty(name, AllDeclared);
        }

        public static IEnumerable<MethodInfo> GetDeclaredMethods(this Type type, string name)
        {
            return type.GetMethods(AllDeclared).Where(m => m.Name == name);
        }

        public static object GetValue(this PropertyInfo property, object o)
        {
            return property.GetValue(o, null);
        }
    }
}

#endif
