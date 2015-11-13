// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.Globalization;
    using System.Linq.Expressions;
    using System.Reflection;

    /// <summary>
    /// StringExpression is a utility that makes it easy to create expressions that call methods on the System.String class.
    /// </summary>
    internal static class StringExpression
    {
        static readonly Expression invariantCulture = Expression.Property(null, typeof(CultureInfo), "InvariantCulture");
        static readonly MethodInfo equals =      Reflection.MethodInfoOf(() => string.Equals(default(string), default(string), default(StringComparison)));
        static readonly MethodInfo format =      Reflection.MethodInfoOf(() => string.Format(default(IFormatProvider), default(string), default(object[])));
        static readonly MethodInfo getHashCode = Reflection.MethodInfoOf((string s) => s.GetHashCode());
        static readonly FieldInfo stringEmpty =  Reflection.FieldInfoOf((string s) => string.Empty);
        
        static readonly IDictionary<BondDataType, MethodInfo> methods =
            new Dictionary<BondDataType, MethodInfo>
                {
                    { BondDataType.BT_BOOL,   Reflection.MethodInfoOf(() => System.Convert.ToBoolean(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_UINT8,  Reflection.MethodInfoOf(() => System.Convert.ToByte(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_UINT16, Reflection.MethodInfoOf(() => System.Convert.ToUInt16(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_UINT32, Reflection.MethodInfoOf(() => System.Convert.ToUInt32(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_UINT64, Reflection.MethodInfoOf(() => System.Convert.ToUInt64(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_INT8,   Reflection.MethodInfoOf(() => System.Convert.ToSByte(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_INT16,  Reflection.MethodInfoOf(() => System.Convert.ToInt16(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_INT32,  Reflection.MethodInfoOf(() => System.Convert.ToInt32(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_INT64,  Reflection.MethodInfoOf(() => System.Convert.ToInt64(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_FLOAT,  Reflection.MethodInfoOf(() => System.Convert.ToSingle(default(string), default(IFormatProvider))) },
                    { BondDataType.BT_DOUBLE, Reflection.MethodInfoOf(() => System.Convert.ToDouble(default(string), default(IFormatProvider))) }
                };

        public static Expression Convert(Expression valueAsString, BondDataType type)
        {
            if (type == BondDataType.BT_STRING || type == BondDataType.BT_WSTRING)
            {
                return valueAsString;
            }

            return Expression.Call(methods[type], valueAsString, invariantCulture);
        }
        
        public static Expression Equals(Expression a, Expression b, StringComparison comparison)
        {
            return Expression.Call(equals, a, b, Expression.Constant(comparison));
        }

        public static Expression Equals(Expression a, string b, StringComparison comparison)
        {
            return Expression.Call(equals, a, Expression.Constant(b), Expression.Constant(comparison));
        }

        public static Expression GetHashCode(Expression s)
        {
            return Expression.Call(s, getHashCode);
        }

        public static Expression Format(string format, params Expression[] argumentExpressions)
        {
            return Expression.Call(
                StringExpression.format,
                invariantCulture,
                Expression.Constant(format),
                Expression.NewArrayInit(typeof(object), argumentExpressions));
        }

        public static Expression Empty()
        {
            return Expression.Field(null, stringEmpty);
        }
    }
}
