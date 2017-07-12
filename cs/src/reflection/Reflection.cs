// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Internal.Reflection
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Reflection;

    internal static class ReflectionExtensions
    {
        #region PCL compatibility

        public static bool IsClass(this Type type)
        {
            return type.GetTypeInfo().IsClass;
        }

        public static bool IsValueType(this Type type)
        {
            return type.GetTypeInfo().IsValueType;
        }

        public static bool IsInterface(this Type type)
        {
            return type.GetTypeInfo().IsInterface;
        }

        public static bool IsEnum(this Type type)
        {
            return type.GetTypeInfo().IsEnum;
        }

        public static bool IsGenericType(this Type type)
        {
            return type.GetTypeInfo().IsGenericType;
        }

        public static Type GetBaseType(this Type type)
        {
            if (type.IsInterface())
            {
                throw new ArgumentException("GetBaseType cannot be called on an interface, as there may be multiple base interfaces", "type");
            }

            return type.GetTypeInfo().BaseType;
        }

        public static bool IsAssignableFrom(this Type type, Type that)
        {
            return type.GetTypeInfo().IsAssignableFrom(that.GetTypeInfo());
        }

        public static MethodInfo FindMethod(this Type type, string name, params Type[] paramTypes)
        {
            var methods = type.GetTypeInfo().GetDeclaredMethods(name);

            var result = (
                from method in methods
                let parameters = method.GetParameters()
                where parameters != null
                where parameters.Select(p => p.ParameterType).Where(t => !t.IsGenericParameter).SequenceEqual(paramTypes)
                select method).FirstOrDefault();

            if (result == null)
            {
                if (type.IsInterface())
                {
                    var interfaces = type.GetTypeInfo().ImplementedInterfaces;
                    var matchedMethods = interfaces.Select(x => x.FindMethod(name, paramTypes)).Where(x => x != null).ToList();

                    if (matchedMethods.Count > 1)
                    {
                        throw new AmbiguousMatchException("FindMethod found more than one matching method");
                    }
                    else
                    {
                        result = matchedMethods.FirstOrDefault();
                    }
                }
                else
                {
                    var baseType = type.GetBaseType();
                    if (baseType != null)
                        result = baseType.FindMethod(name, paramTypes);
                }
            }

            return result;
        }

        public static MethodInfo ResolveMethod(this Type type, string name, params Type[] argumentTypes)
        {
            var methods = type.GetTypeInfo().GetDeclaredMethods(name);
            var typeArgs = new Type[0];

            foreach (var method in methods)
            {
                var parameters = method.GetParameters();
                if (parameters.Length != argumentTypes.Length)
                    continue;

                int paramIndex;
                for (paramIndex = 0; paramIndex < parameters.Length; ++paramIndex)
                {
                    var param = parameters[paramIndex].ParameterType;
                    var arg = argumentTypes[paramIndex];

                    if (param == arg)
                        continue;

                    if (param.IsGenericType() && arg.IsGenericType() &&
                        param.GetGenericTypeDefinition() == arg.GetGenericTypeDefinition() &&
                        param.GetTypeInfo().GenericTypeArguments.All(p => p.IsGenericParameter))
                    {
                        typeArgs = arg.GetTypeInfo().GenericTypeArguments;
                        continue;
                    }
                    break;
                }

                if (paramIndex == parameters.Length)
                    return typeArgs.Length == 0 ? method : method.MakeGenericMethod(typeArgs);
            }
            return null;
        }

        public static MethodInfo GetMethod(this Type type, Type declaringType, string name, params Type[] paramTypes)
        {
            return declaringType.MakeGenericTypeFrom(type).FindMethod(name, paramTypes);
        }

        public static ConstructorInfo GetConstructor(this Type type, params Type[] paramTypes)
        {
            var methods = type.GetTypeInfo().DeclaredConstructors;

            return (
                from method in methods
                let parameters = method.GetParameters()
                where parameters != null
                where method.IsStatic == false
                where parameters.Select(p => p.ParameterType).SequenceEqual(paramTypes)
                select method).FirstOrDefault();
        }

        public static PropertyInfo GetDeclaredProperty(this Type type, string name, Type returnType)
        {
            var property = type.GetTypeInfo().GetDeclaredProperty(name);
            return (property != null && property.PropertyType == returnType) ? property : null;
        }

        public static PropertyInfo GetDeclaredProperty(this Type type, Type declaringType, string name, Type returnType)
        {
            return declaringType.MakeGenericTypeFrom(type).GetDeclaredProperty(name, returnType);
        }

        static Type MakeGenericTypeFrom(this Type genericType, Type concreteType)
        {
            var typeArguments = concreteType.GetTypeInfo().GenericTypeArguments;
            if (concreteType.IsArray)
            {
                typeArguments = new[] { concreteType.GetElementType() };
            }

            var typeParameters = genericType.GetTypeInfo().GenericTypeParameters;

            if (typeArguments.Length == 2 && typeParameters.Length == 1)
                typeArguments = new[] { typeof(KeyValuePair<,>).MakeGenericType(typeArguments) };

            return genericType.MakeGenericType(typeArguments);
        }

        #endregion
    }
}
