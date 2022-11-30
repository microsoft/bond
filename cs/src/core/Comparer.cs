// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Internal.Reflection;

    /// <summary>
    /// Utility for comparing instances of Bond schemas for equality
    /// </summary>
    public static class Comparer
    {
        /// <summary>
        /// Compare objects for equality
        /// </summary>
        /// <typeparam name="T">Type representing a Bond schema</typeparam>
        /// <param name="left">First object to compare</param>
        /// <param name="right">Second object to compare</param>
        /// <returns></returns>
        public static bool Equal<T>(T left, T right)
        {
            return Cache<T>.Equal(left, right);
        }

        static class Cache<T>
        {
            public static readonly Func<T, T, bool> Equal;

            static readonly MethodInfo blobCompareData = Reflection.MethodInfoOf(() => Blob.CompareData(default(ArraySegment<byte>), default(ArraySegment<byte>)));
            static readonly MethodInfo moveNext =        Reflection.MethodInfoOf((IEnumerator ie) => ie.MoveNext());
            static readonly MethodInfo comparerEqual =   Reflection.GenericMethodInfoOf(() => Comparer.Equal(default(T), default(T)));

            static Cache()
            {
                var left = Expression.Parameter(typeof(T));
                var right = Expression.Parameter(typeof(T));

                var equal = typeof(T).IsBondStruct() ?
                    typeof(T).IsValueType() ?
                        StructsEqual(left, right) :
                        NullCheckEqual(left, right, StructsEqual(left, right)) :
                    ObjectsEqual(left, right);
                Equal = Expression.Lambda<Func<T, T, bool>>(equal, left, right).Compile();
            }

            static Expression ObjectsEqual(Expression left, Expression right)
            {
                var type = left.Type;

                if (type.IsBondBlob())
                    return Expression.Call(null, blobCompareData, left, right);

                if (type.IsBondStruct())
                    return Expression.Call(null, comparerEqual.MakeGenericMethod(type), left, right);

                if (type.IsBondContainer())
                {
                    if (type.IsValueType())
                    {
                        return StructsEqual(left, right);
                    }
                    else
                    {
                        return EnumerablesEqual(left, right);
                    }
                }

                if (type.IsGenericType() && type.GetGenericTypeDefinition() == typeof(KeyValuePair<,>))
                    return KeyValuePairEqual(left, right);
                
                return Expression.Equal(left, right);
            }

            static Expression StructsEqual(Expression left, Expression right)
            {
                var type = left.Type;
                var baseType = type.GetBaseSchemaType();
                var fields = type.GetSchemaFields().GetEnumerator();

                var baseEquals = baseType == null ? 
                    Expression.Constant(true) :
                    StructsEqual(Expression.Convert(left, baseType), Expression.Convert(right, baseType));

                return Expression.AndAlso(baseEquals, FieldsEqual(fields, left, right));
            }

            static Expression KeyValuePairEqual(Expression left, Expression right)
            {
                return Expression.AndAlso(
                    ObjectsEqual(Expression.Property(left, "Key"), Expression.Property(right, "Key")),
                    ObjectsEqual(Expression.Property(left, "Value"), Expression.Property(right, "Value")));
            }

            static Expression FieldsEqual(IEnumerator<ISchemaField> fields, Expression left, Expression right)
            {
                if (!fields.MoveNext())
                    return Expression.Constant(true);
                
                return Expression.AndAlso(
                    ObjectsEqual(
                        Expression.MakeMemberAccess(left, fields.Current.MemberInfo),
                        Expression.MakeMemberAccess(right, fields.Current.MemberInfo)),
                    FieldsEqual(fields, left, right));
            }

            static Expression EnumerablesEqual(Expression left, Expression right)
            {
                var type = left.Type;
                var getEnumerator = type.GetMethod(typeof(IEnumerable<>), "GetEnumerator");
                var result = Expression.Variable(typeof(bool));
                var leftEnumerator = Expression.Variable(getEnumerator.ReturnType);
                var rightEnumerator = Expression.Variable(getEnumerator.ReturnType);
                var nextLeft = Expression.Call(leftEnumerator, moveNext);
                var nextRight = Expression.Call(rightEnumerator, moveNext);
                var leftItem = Expression.Property(leftEnumerator, "Current");
                var rightItem = Expression.Property(rightEnumerator, "Current");
                var breakLabel = Expression.Label();

                return Expression.Block(
                    new[] { leftEnumerator, rightEnumerator },
                    NullCheckEqual(left, right,
                        Expression.Block(
                            new [] { result },
                            Expression.Assign(leftEnumerator, Expression.Call(left, getEnumerator)),
                            Expression.Assign(rightEnumerator, Expression.Call(right, getEnumerator)),
                            Expression.Assign(result, Expression.Constant(true)),
                            Expression.Loop(
                                Expression.IfThenElse(
                                    Expression.AndAlso(result, nextLeft),
                                    Expression.IfThenElse(
                                        nextRight,
                                        Expression.Assign(result, ObjectsEqual(leftItem, rightItem)),
                                        Expression.Assign(result, Expression.Constant(false))),
                                    Expression.Break(breakLabel)),
                                breakLabel),
                            Expression.AndAlso(result, Expression.Equal(nextLeft, nextRight)))));
            }

            static Expression NullCheckEqual(Expression left, Expression right, Expression equal)
            {
                return Expression.Condition(
                    Expression.OrElse(
                        Expression.Equal(left, Expression.Constant(null)),
                        Expression.Equal(right, Expression.Constant(null))),
                    Expression.Equal(left, right),
                    equal);
            }
        }
    }
}
