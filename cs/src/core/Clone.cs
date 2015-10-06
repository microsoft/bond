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
        /// Create an instance of type T by deep cloning properties/fields of a source object of type SourceT.
        /// </summary>
        /// <typeparam name="SourceT">type representing a source schema compatible with schema T</typeparam>
        /// <param name="source">source object to create a clone from</param>
        /// <returns></returns>
        public static T From<SourceT>(SourceT source)
        {
            return Cache<SourceT>.Instance.Clone<T>(source);
        }
    }

    /// <summary>
    /// Utility for cloning objects of type SourceT.
    /// </summary>
    /// <typeparam name="SourceT">type representing a Bond schema</typeparam>
    public class Cloner<SourceT>
    {
        readonly Func<object, object>[] clone;
        
        /// <summary>
        /// Create a cloner that makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        public Cloner(Type type)
        {
            clone = Generate(type, new DeserializerTransform<object>(
                (o, i) => clone[i](o)));
        }

        /// <summary>
        /// Create a cloner that uses specified factory and makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        /// <param name="factory">factory implementing IFactory interface</param>
        public Cloner(Type type, IFactory factory)
        {
            clone = Generate(type, 
                new DeserializerTransform<object>(
                    (o, i) => clone[i](o),
                    true,
                    (t1, t2) => factory.CreateObject(t1, t2),
                    (t1, t2, count) => factory.CreateContainer(t1, t2, count)));
        }

        /// <summary>
        /// Create a cloner that uses specified factory and makes clones of the specified type.
        /// </summary>
        /// <param name="type">type of clone object, may be different than source object</param>
        /// <param name="factory">factory delegate returning expressions to create objects</param>
        public Cloner(Type type, Factory factory)
        {
            clone = Generate(type,
                new DeserializerTransform<object>(
                    (o, i) => clone[i](o),
                    factory));
        }

        /// <summary>
        /// Create a cloner that makes clones of the same type SourceT as source objects.
        /// </summary>
        public Cloner()
            : this(typeof(SourceT))
        {}

        /// <summary>
        /// Clone the source object into an object of type T.
        /// </summary>
        /// <typeparam name="T">type of result, must be the same as types specified during Cloner construction</typeparam>
        /// <param name="source">source object to be cloned</param>
        /// <returns>clone of the source object projected on type T</returns>
        public T Clone<T>(SourceT source)
        {
            return (T)clone[0](source);
        }

        static Func<object, object>[] Generate(Type type, DeserializerTransform<object> transform)
        {
            var parser = new ObjectParser(typeof(SourceT));
            return transform.Generate(parser, type).Select(lambda => lambda.Compile()).ToArray();
        }
    }
}
