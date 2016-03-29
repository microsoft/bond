// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Expressions
{
    using System;
    using System.Globalization;
    using System.Linq.Expressions;
    using Bond.Protocols;

    internal static class ParserFactory<R>
    {
        public static IParser Create<S>(S schema, Factory factory = null)
        {
            return Cache<S>.Create(schema, factory);
        }

        static class Cache<S>
        {
            public static readonly Func<S, Factory, IParser> Create;

            [System.Diagnostics.CodeAnalysis.SuppressMessage(
                "Microsoft.Design", "CA1065:DoNotRaiseExceptionsInUnexpectedLocations")]
            static Cache()
            {
                Type parserType;

                var attribute = typeof(R).GetAttribute<ParserAttribute>();
                if (attribute == null)
                {
                    if (typeof(ITaggedProtocolReader).IsAssignableFrom(typeof(R)))
                    {
                        parserType = typeof(TaggedParser<R>);
                    }
                    else if (typeof(IUntaggedProtocolReader).IsAssignableFrom(typeof(R)))
                    {
                        parserType = typeof(UntaggedParser<R>);
                    }
                    else
                    {
                        throw new InvalidOperationException(
                            string.Format(
                                CultureInfo.InvariantCulture,
                                "Can't determine parser type for reader type {0}, specify using ParserAttribute.",
                                typeof(R)));
                    }
                }
                else
                {
                    var genericParserType = attribute.ParserType;
                    if (!genericParserType.IsGenericType() || genericParserType.GetGenericParameters().Length != 1)
                    {
                        throw new InvalidOperationException(
                            "Parser type is expected to be a generic type with one type param for Reader.");
                    }

                    parserType = genericParserType.MakeGenericType(typeof(R));
                    if (!typeof(IParser).IsAssignableFrom(parserType))
                    {
                        throw new InvalidOperationException(
                            string.Format(
                                CultureInfo.InvariantCulture,
                                "Parser type {0} specified in attribute for Reader type {1} is not an IParser.",
                                parserType,
                                typeof(R)));
                    }
                }

                var schema = Expression.Parameter(typeof(S));
                var bondedFactory = Expression.Parameter(typeof(Factory));

                var ctor = parserType.GetConstructor(typeof (S), typeof (Factory)) ??
                           parserType.GetConstructor(typeof (S));

                if (ctor == null)
                {
                    throw new InvalidOperationException(
                        string.Format(
                            CultureInfo.InvariantCulture,
                            "Constructor {0}({1}) not defined.",
                            parserType, typeof(S)));
                }

                var newExpression = ctor.GetParameters().Length == 2
                    ? Expression.New(ctor, schema, bondedFactory)
                    : Expression.New(ctor, schema);

                Create = Expression.Lambda<Func<S, Factory, IParser>>(newExpression, schema, bondedFactory).Compile();
            }
        }
    }
}
