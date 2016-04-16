// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Linq;
    using Bond.Expressions;

    /// <summary>
    /// Clone API for creating objects of type T by deep cloning properties/fields of a compatible object.
    /// </summary>
    /// <typeparam name="T">type representing a Bond schema</typeparam>
    public static class Clone<T>
    {
        static class Cache<SourceT>
        {
            public static readonly Cloner<SourceT> Instance = new Cloner<SourceT>(typeof(T));
        }

        /// <summary>
        /// Create an instance of type <typeparamref name="T"/> by deep cloning properties/fields of a source object of type <typeparamref name="SourceT"/>.
        /// </summary>
        /// <typeparam name="SourceT">type representing a source schema compatible with schema <typeparamref name="T"/></typeparam>
        /// <param name="source">source object to create a clone from</param>
        /// <returns></returns>
        public static T From<SourceT>(SourceT source)
        {
            return Cache<SourceT>.Instance.Clone<T>(source);
        }
    }

    /// <summary>
    /// Utility for cloning objects of type <typeparamref name="SourceT"/>.
    /// </summary>
    /// <typeparam name="SourceT">type representing a Bond schema</typeparam>
    public class Cloner<SourceT>
    {
        readonly Func<object, object>[] clone;

        /// <summary>
        /// Create a cloner that makes clones of the same type <typeparamref name="SourceT"/> as source objects.
        /// </summary>
        public Cloner()
            : this(typeof(SourceT))
        {}

        /// <summary>
        /// Create a cloner that makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        public Cloner(Type type)
            : this(type, (IParser) null)
        {}

        /// <summary>
        /// Create a cloner that makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        /// <param name="parser">Custom <see cref="IParser"/> instance</param>
        public Cloner(Type type, IParser parser)
        {
            clone = Generate(type,
                             new DeserializerTransform<object>((o, i) => clone[i](o)),
                             parser);
        }

        /// <summary>
        /// Create a cloner that makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        /// /// <param name="factory">factory implementing <see cref="IFactory"/> interface</param>
        public Cloner(Type type, IFactory factory)
            : this(type, null, factory)
        {}

        /// <summary>
        /// Create a cloner that uses specified factory and makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        /// <param name="parser">Custom <see cref="IParser"/> instance</param>
        /// <param name="factory">factory implementing <see cref="IFactory"/> interface</param>
        public Cloner(Type type, IParser parser, IFactory factory)
        {
            clone = Generate(type,
                             new DeserializerTransform<object>(
                                 (o, i) => clone[i](o),
                                 true,
                                 (t1, t2) => factory.CreateObject(t1, t2),
                                 (t1, t2, count) => factory.CreateContainer(t1, t2, count)),
                             parser);
        }

        /// <summary>
        /// Create a cloner that uses specified factory and makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        /// <param name="factory">factory delegate returning expressions to create objects</param>
        public Cloner(Type type, Factory factory)
            : this(type, null, factory)
        {}

        /// <summary>
        /// Create a cloner that uses specified factory and makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        /// <param name="parser">Custom <see cref="IParser"/> instance</param>
        /// <param name="factory">factory delegate returning expressions to create objects</param>
        public Cloner(Type type, IParser parser, Factory factory)
        {
            clone = Generate(type,
                             new DeserializerTransform<object>(
                                 (o, i) => clone[i](o),
                                 factory),
                             parser);
        }

        /// <summary>
        /// Clone the source object into an object of type <typeparamref name="T"/>.
        /// </summary>
        /// <typeparam name="T">type of result, must be the same as types specified during <see cref="Cloner{SourceT}" /> construction</typeparam>
        /// <param name="source">source object to be cloned</param>
        /// <returns>clone of the source object projected on type <typeparamref name="T"/></returns>
        public T Clone<T>(SourceT source)
        {
            return (T)clone[0](source);
        }

        static Func<object, object>[] Generate(Type type, DeserializerTransform<object> transform, IParser parser)
        {
            parser = parser ?? new ObjectParser(typeof(SourceT));

            return transform.Generate(parser, type).Select(lambda => lambda.Compile()).ToArray();
        }
    }
}
