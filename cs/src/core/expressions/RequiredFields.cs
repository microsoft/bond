// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System.Diagnostics;
    using System.Linq;
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Internal.Reflection;

    internal static class RequiredFields
    {
        static readonly ConstructorInfo ctor =  typeof(Bitmap).GetConstructor(typeof(int));
        static readonly PropertyInfo isAnySet = Reflection.PropertyInfoOf((Bitmap b) => b.IsAnySet);
        static readonly PropertyInfo firstSet = Reflection.PropertyInfoOf((Bitmap b) => b.FirstSet);
        static readonly MethodInfo reset =      Reflection.MethodInfoOf((Bitmap bitmap) => bitmap.Reset(default(int), default(long)));

        public static ParameterExpression Variable(string name)
        {
            return Expression.Variable(typeof(Bitmap), name);
        }

        public static Expression Init(ParameterExpression requiredFields, int count)
        {
            return count == 0
                ? (Expression) Expression.Empty()
                : Expression.Assign(requiredFields, Expression.New(ctor, Expression.Constant(count)));
        }

        public static Expression Mark(ParameterExpression requiredFields, int index)
        {
            return Expression.Call(requiredFields, reset,
                Expression.Constant(index >> 6),
                Expression.Constant(1L << (index % 64)));
        }

        public static Expression IfMissingAny(ParameterExpression requiredFields, Expression then)
        {
            return Expression.IfThen(Expression.Property(requiredFields, isAnySet), then);
        }

        public static Expression FirstMissing(Expression fields)
        {
            return Expression.Property(fields, firstSet);
        }


        internal class Bitmap
        {
            readonly long[] bitmap;

            public Bitmap(int count)
            {
                Debug.Assert(count > 0);

                bitmap = new long[1 + ((count - 1) >> 6)];
                var index = 0;
                for (; count >= 64; count -= 64)
                    bitmap[index++] = -1;
                if (count != 0)
                    bitmap[index] = (1L << count) - 1;
            }

            public bool IsAnySet { get { return bitmap.Any(b => b != 0); } }

            public int FirstSet
            {
                get
                {
                    Debug.Assert(IsAnySet);

                    int index = 0, i = 0;
                    while (0 == bitmap[index])
                        index++;
                    while (0 == (bitmap[index] & (1L << i)))
                        i++;
                    return (index << 6) + i;
                }
            }

            public void Reset(int index, long mask)
            {
                bitmap[index] &= ~mask;
            }
        }
    }
}
