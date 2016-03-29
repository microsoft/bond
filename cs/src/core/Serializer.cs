// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Linq;

    using Bond.Expressions;

    /// <summary>
    /// Serialize objects
    /// </summary>
    public static class Serialize
    {
        static class Cache<W, T>
        {
            public static readonly Serializer<W> Instance = new Serializer<W>(typeof(T));
        }

        /// <summary>
        /// Serialize object of type T to protocol writer of type W
        /// </summary>
        /// <typeparam name="W">Protocol writer</typeparam>
        /// <typeparam name="T">Type representing a Bond schema</typeparam>
        /// <param name="writer">Writer instance</param>
        /// <param name="obj">Object to serialize</param>
        public static void To<W, T>(W writer, T obj)
        {
            Cache<W, T>.Instance.Serialize(obj, writer);
        }

        /// <summary>
        /// Serialize IBonded&lt;T> to protocol writer of type W
        /// </summary>
        /// <typeparam name="W">Protocol writer</typeparam>
        /// <typeparam name="T">Type representing a Bond schema</typeparam>
        /// <param name="writer">Writer instance</param>
        /// <param name="bonded">IBonded instance</param>
        public static void To<W, T>(W writer, IBonded<T> bonded)
        {
            bonded.Serialize(writer);
        }

        /// <summary>
        /// Serialize IBonded to protocol writer of type W
        /// </summary>
        /// <typeparam name="W">Protocol writer</typeparam>
        /// <param name="writer">Writer instance</param>
        /// <param name="bonded">IBonded instance</param>
        public static void To<W>(W writer, IBonded bonded)
        {
            bonded.Serialize(writer);
        }
    }

    /// <summary>
    /// Serializer for protocol writer W
    /// </summary>
    /// <typeparam name="W">Protocol writer</typeparam>
    public class Serializer<W>
    {
        readonly Action<object, W>[] serialize;

        /// <summary>
        /// Create a serializer for specified type
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="factory">Factory</param>
        public Serializer(Type type, Factory factory = null) : this(type, inlineNested: true, factory: factory) { }

        /// <summary>
        /// Create a serializer for specified type
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="inlineNested">Indicates whether nested struct serialization code may be inlined</param>
        /// <param name="factory"> </param>
        public Serializer(Type type, bool inlineNested, Factory factory = null)
        {
            var parser = new ObjectParser(type, factory);
            serialize = SerializerGeneratorFactory<object, W>.Create(
                    (o, w, i) => serialize[i](o, w), type, inlineNested)
                .Generate(parser)
                .Select(lambda => lambda.Compile()).ToArray();
        }

        /// <summary>
        /// Serialize object using protocol writer of type W
        /// </summary>
        /// <param name="obj">Object to serialize</param>
        /// <param name="writer">Writer instance</param>
        /// <remarks>
        /// The object must be of type used to create the Serializer, otherwise behavior is undefined
        /// </remarks>
        public void Serialize(object obj, W writer)
        {
            serialize[0](obj, writer);
        }
    }
}
