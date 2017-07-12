// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Expressions;
    using Bond.Internal.Reflection;

    /// <summary>
    /// Generic object factory
    /// </summary>
    public static class GenericFactory
    {
        /// <summary>
        /// Create an instance of T
        /// </summary>
        /// <typeparam name="T">Type of object to create</typeparam>
        /// <returns>Object of type T initialized to the default value</returns>
        public static T Create<T>()
        {
            return Cache<T>.Create();
        }

        static class Cache<T>
        {
            public static readonly Func<T> Create;

            static Cache()
            {
                Expression create;

                if (typeof(T) == typeof(string))
                {
                    create = StringExpression.Empty();
                }
                else if (typeof(T).IsBonded())
                {
                    create = Expression.Field(null, 
                        typeof(Bonded<>).MakeGenericType(typeof(T).GetValueType()).GetTypeInfo().GetDeclaredField("Empty"));
                }
                else if (typeof(T).IsClass())
                {
                    create = Expression.New(typeof(T));
                }
                else
                {
                    create = Expression.Default(typeof(T));
                }

                Create = Expression.Lambda<Func<T>>(create).Compile();
            }
        }
    }
}
