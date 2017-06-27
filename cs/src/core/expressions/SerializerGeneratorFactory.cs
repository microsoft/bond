// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Globalization;
    using System.Linq.Expressions;
    using System.Reflection;
    using Bond.Internal.Reflection;

    internal static class SerializerGeneratorFactory<R, W>
    {
        public static ISerializerGenerator<R, W> Create<S>(
            Expression<Action<R, W, int>> deferredSerialize, S schema, bool inlineNested = true)
        {
            return Cache<S>.Create(deferredSerialize, schema, inlineNested);
        }

        static class Cache<S>
        {
            public static readonly Func<Expression<Action<R, W, int>>, S, bool, ISerializerGenerator<R, W>> Create;

            [System.Diagnostics.CodeAnalysis.SuppressMessage(
                "Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
            static Cache()
            {
                Type generator;

                var attribute = typeof(W).GetAttribute<SerializerAttribute>();
                if (attribute == null)
                {
                    generator = typeof(SerializerTransform<R, W>);
                }
                else
                {
                    if (!attribute.Type.IsGenericType() || attribute.Type.GetTypeInfo().GenericTypeParameters.Length != 2)
                    {
                        throw new InvalidOperationException(
                            "Serializer generator is expected to be a generic type with two type parameters.");
                    }

                    generator = attribute.Type.MakeGenericType(typeof(R), typeof(W));

                    if (!typeof(ISerializerGenerator<R, W>).IsAssignableFrom(generator))
                    {
                        throw new InvalidOperationException(
                            string.Format(
                                CultureInfo.InvariantCulture,
                                "Serializer generator {0} specified for writer {1} is not an ISerializerGenerator.",
                                generator, typeof(W)));
                    }
                }

                var ctor =
                    generator.GetConstructor(typeof(Expression<Action<R, W, int>>), typeof(S), typeof(bool)) ??
                    generator.GetConstructor(typeof(Expression<Action<R, W, int>>), typeof(S));

                if (ctor == null)
                {
                    throw new InvalidOperationException(
                        string.Format(
                            CultureInfo.InvariantCulture,
                            "Constructor {0}(Expression<Action<R, W, int>>, {1}) not defined.",
                            generator, typeof(S)));
                }

                var deferredSerialize = Expression.Parameter(typeof(Expression<Action<R, W, int>>));
                var schema = Expression.Parameter(typeof(S));
                var inlineNested = Expression.Parameter(typeof(bool));

                var newExpression = 
                    ctor.GetParameters().Length == 3
                        ? Expression.New(ctor, deferredSerialize, schema, inlineNested)
                        : Expression.New(ctor, deferredSerialize, schema);

                Create =
                    Expression.Lambda<Func<Expression<Action<R, W, int>>, S, bool, ISerializerGenerator<R, W>>>(
                        newExpression,
                        deferredSerialize,
                        schema,
                        inlineNested).Compile();
            }
        }
    }
}
