// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Linq.Expressions;

    internal class TypeAlias
    {
        readonly HashSet<Type> converters;

        public TypeAlias(Type type)
        {
            var schemaTypes = new HashSet<Type>();
            GetSchemaTypes(type, schemaTypes);
            converters = new HashSet<Type>(schemaTypes.Select(GetConverter).Where(c => c != null));
        }

        public Expression Assign(Expression left, Expression right)
        {
            var leftType = left.Type;

            if (leftType != right.Type &&
                leftType.IsGenericType() &&
                leftType.GetGenericTypeDefinition() == typeof (Nullable<>))
            {
                leftType = leftType.GetGenericArguments()[0];
            }

            var value = Convert(right, leftType);

            return Expression.Assign(left, PrunedExpression.Convert(value, left.Type));
        }

        public Expression Convert(Expression value, Type type)
        {
            if (type == typeof(Tag.blob))
                type = typeof(ArraySegment<byte>);

            if (type == typeof(Tag.wstring))
                type = typeof(string);

            if (type != value.Type &&
                value.Type.IsGenericType() &&
                value.Type.GetGenericTypeDefinition() == typeof (Nullable<>))
            {
                value = Expression.Convert(value, value.Type.GetGenericArguments()[0]);
            }

            if (type != value.Type)
            {
                // Converter can be defined either in the namespace/assembly of one of the types
                // being converted or the namespace/assembly of the schema where the alias is used.
                var all = new[] { type, value.Type }.Select(GetConverter).Where(c => c != null).Concat(converters);

                foreach (var converter in all)
                {
                    var convert = converter.FindMethod("Convert", value.Type, type);
                    if (convert != null)
                        return Expression.Call(null, convert, value, Expression.Default(type));
                }
            }

            return value;
        }

        static void GetSchemaTypes(Type type, HashSet<Type> schemaTypes)
        {
            while (true)
            {
                if (type.IsBondStruct())
                {
                    if (schemaTypes.Contains(type))
                        return;

                    schemaTypes.Add(type);

                    foreach (var f in type.GetSchemaFields())
                    {
                        GetSchemaTypes(f.GetSchemaType(), schemaTypes);
                    }

                    type = type.GetBaseSchemaType();
                    if (type != null)
                        continue;
                }
                else if (type.IsBondMap())
                {
                    type = type.GetKeyValueType().Value;
                    continue;
                }
                else if (type.IsBondList() || type.IsBondNullable() || type.IsBonded())
                {
                    type = type.GetValueType();
                    continue;
                }
                break;
            }
        }

        static Type GetConverter(Type type)
        {
            var name = type.AssemblyQualifiedName;
            var converterName = type.Namespace + ".BondTypeAliasConverter" + name.Substring(type.FullName.Length);
            return Type.GetType(converterName);
        }
    }
}
