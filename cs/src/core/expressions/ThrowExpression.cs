// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Collections.Generic;
    using System.IO;
    using System.Linq;
    using System.Linq.Expressions;
    using System.Runtime.CompilerServices;

    /// <summary>
    /// ThrowExpression is a utility that makes it easy to create expressions that throw exceptions.
    /// </summary>
    internal static class ThrowExpression
    {
        static readonly Expression<Action<BondDataType>> throwInvalidTypeException = 
            t => ThrowInvalidTypeException(t);
        static readonly Expression<Action<BondDataType, BondDataType>> throwInvalidTypeException2 = 
            (e, a) => ThrowInvalidTypeException(e, a);
        static readonly Expression<Action<string>> throwInvalidDataException = 
            m => ThrowInvalidDataException(m);
        static readonly Expression<Action<string, string>> throwRequiredFieldMissingException =
            (s, f) => ThrowRequiredFieldMissingException(s, f);
        static readonly Expression<Action<List<string>, int>> throwRequiredFieldsMissingException =
            (f, i) => ThrowRequiredFieldsMissingException(f, i);

        public static Expression InvalidTypeException(Expression actualType)
        {
            return Expression.Invoke(throwInvalidTypeException, actualType);
        }

        public static Expression InvalidTypeException(Expression expectedType, Expression actualType)
        {
            return Expression.Invoke(throwInvalidTypeException2, expectedType, actualType);
        }

        public static Expression InvalidDataException(string message)
        {
            return Expression.Invoke(throwInvalidDataException, Expression.Constant(message));
        }

        public static Expression InvalidDataException(Expression message)
        {
            return Expression.Invoke(throwInvalidDataException, message);
        }

        public static Expression RequiredFieldMissingException(string schema, Expression field)
        {
            return Expression.Invoke(throwRequiredFieldMissingException, Expression.Constant(schema), field);
        }

        public static Expression RequiredFieldsMissingException(Expression fields, IEnumerable<string> names)
        {
            return Expression.Invoke(throwRequiredFieldsMissingException,
                Expression.ListInit(Expression.New(typeof(List<string>)), names.Select(Expression.Constant)),
                RequiredFields.FirstMissing(fields));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static void ThrowInvalidTypeException(BondDataType actualType)
        {
            throw new InvalidDataException(string.Format("Invalid type {0}", actualType));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static void ThrowInvalidTypeException(BondDataType expectedType, BondDataType actualType)
        {
            throw new InvalidDataException(string.Format("Invalid type {0}, expected {1}", actualType, expectedType));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static void ThrowInvalidDataException(string message)
        {
            throw new InvalidDataException(message);
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static void ThrowRequiredFieldMissingException(string schema, string field)
        {
            throw new InvalidDataException(string.Format("Required field {0}.{1} missing", schema, field));
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        static void ThrowRequiredFieldsMissingException(List<string> names, int index)
        {
            throw new InvalidDataException(string.Format("Required field {0} missing", names[index]));
        }
    }
}
