// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Diagnostics;
    using System.Linq;
    using System.Reflection;
    using Bond.Expressions;
    using Bond.IO;

    /// <summary>
    /// Deserialize objects of type T
    /// </summary>
    /// <typeparam name="T">Type representing a Bond schema</typeparam>
    public static class Deserialize<T>
    {
        static class Cache<R>
        {
            public static readonly Deserializer<R> Instance = new Deserializer<R>(typeof(T));
        }

        /// <summary>
        /// Deserialize an object of type T from a payload
        /// </summary>
        /// <typeparam name="R">Protocol reader</typeparam>
        /// <param name="reader">Protocol reader representing payload</param>
        /// <returns>Deserialized object</returns>
        public static T From<R>(R reader)
        {
            return Cache<R>.Instance.Deserialize<T>(reader);
        }
    }

    /// <summary>
    /// Deserializer for a protocol reader R
    /// </summary>
    /// <typeparam name="R">Protocol reader</typeparam>
    public class Deserializer<R>
    {
        internal readonly Func<R, object>[] deserialize;

        /// <summary>
        /// Create a deserializer instance for specified type and payload schema, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="schema">Schema of the payload</param>
        /// <param name="factory">Factory to create objects during deserialization</param>
        /// <param name="inlineNested">Inline nested types if possible (optimizes for reduction of execution time
        /// at the expense of initialization time and memory)</param>
        public Deserializer(Type type, RuntimeSchema schema, IFactory factory, bool inlineNested)
            : this(type, ParserFactory<R>.Create(schema), factory, null, inlineNested)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type and payload schema, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="schema">Schema of the payload</param>
        /// <param name="factory">Factory providing expressions to create objects during deserialization</param>
        /// <param name="inlineNested">Inline nested types if possible (optimizes for reduction of execution time
        /// at the expense of initialization time and memory)</param>
        public Deserializer(Type type, RuntimeSchema schema, Factory factory, bool inlineNested)
            : this(type, ParserFactory<R>.Create(schema), null, factory, inlineNested)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type and payload schema, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="schema">Schema of the payload</param>
        /// <param name="factory">Factory to create objects during deserialization</param>
        public Deserializer(Type type, RuntimeSchema schema, IFactory factory)
            : this(type, ParserFactory<R>.Create(schema), factory)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type and payload schema, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="schema">Schema of the payload</param>
        /// <param name="factory">Factory providing expressions to create objects during deserialization</param>
        public Deserializer(Type type, RuntimeSchema schema, Factory factory)
            : this(type, ParserFactory<R>.Create(schema), null, factory)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type and payload schema
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="schema">Schema of the payload</param>
        public Deserializer(Type type, RuntimeSchema schema)
            : this(type, ParserFactory<R>.Create(schema))
        { }

        /// <summary>
        /// Create a deserializer instance for specified type, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="factory">Factory to create objects during deserialization</param>
        /// <param name="inlineNested">Inline nested types if possible (optimizes for reduction of execution time
        /// at the expense of initialization time and memory)</param>
        public Deserializer(Type type, IFactory factory, bool inlineNested)
            : this(type, ParserFactory<R>.Create(type), factory, null, inlineNested)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="factory">Factory providing expressions to create objects during deserialization</param>
        /// <param name="inlineNested">Inline nested types if possible (optimizes for reduction of execution time
        /// at the expense of initialization time and memory)</param>
        public Deserializer(Type type, Factory factory, bool inlineNested)
            : this(type, ParserFactory<R>.Create(type), null, factory, inlineNested)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="factory">Factory to create objects during deserialization</param>
        public Deserializer(Type type, IFactory factory)
            : this(type, ParserFactory<R>.Create(type), factory)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type, using a custom object factory
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="factory">Factory providing expressions to create objects during deserialization</param>
        public Deserializer(Type type, Factory factory)
            : this(type, ParserFactory<R>.Create(type), null, factory)
        { }

        /// <summary>
        /// Create a deserializer instance for specified type
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        public Deserializer(Type type)
            : this(type, ParserFactory<R>.Create(type))
        { }

        public Deserializer(Assembly precompiledAssembly, Type type)
        {
            var precompiledType = precompiledAssembly.GetType(GetPrecompiledClassName(type));
            var property = precompiledType.GetDeclaredProperty("Deserializer", typeof(Func<R, object>));
            deserialize = new[] { (Func<R, object>)property.GetValue(null) };
        }

        Deserializer(Type type, IParser parser, IFactory factory = null, Factory factory2 = null, bool inlineNested = true)
        {
            DeserializerTransform<R> transform;
            if (factory != null)
            {
                Debug.Assert(factory2 == null);

                transform = new DeserializerTransform<R>(
                    (r, i) => deserialize[i](r),
                    inlineNested,
                    (t1, t2) => factory.CreateObject(t1, t2),
                    (t1, t2, count) => factory.CreateContainer(t1, t2, count));
            }
            else
            {
                transform = new DeserializerTransform<R>(
                    (r, i) => deserialize[i](r),
                    factory2,
                    inlineNested);
            }

            deserialize = transform.Generate(parser, type).Select(lambda => lambda.Compile()).ToArray();
        }

        /// <summary>
        /// Deserialize an object of type T from a payload
        /// </summary>
        /// <typeparam name="T">Type representing a Bond schema</typeparam>
        /// <param name="reader">Protocol reader representing the payload</param>
        /// <returns>Deserialized object</returns>
        public T Deserialize<T>(R reader)
        {
            return (T)deserialize[0](reader);
        }

        /// <summary>
        /// Deserialize an object from a payload
        /// </summary>
        /// <param name="reader">Protocol reader representing the payload</param>
        /// <returns>Deserialized object</returns>
        public object Deserialize(R reader)
        {
            return deserialize[0](reader);
        }

        internal static string GetPrecompiledClassName(Type type, string suffix = null)
        {
            return string.Concat("Deserializer", suffix ?? string.Empty, "__", typeof(R).Name, "__", type.GetSchemaFullName())
                .Replace('.', '_').Replace('<', '_').Replace('>', '_').Replace(' ', '_');
        }
    }

    /// <summary>
    /// Deserializer extension methods
    /// </summary>
    public static class Deserializer
    {
        /// <summary>
        /// Deserialize an object from an IBonded&lt;T> instance using a specific deserializer
        /// </summary>
        /// <param name="deserializer">Deserializer to be used to deserialize IBonded&lt;T> payload</param>
        /// <param name="bonded">IBonded&lt;T> instance representing payload</param>
        /// <remarks>Implemented as an extension method to avoid ICloneable&lt;R> constraint on Deserializer&lt;R></remarks>
        /// <returns>Deserialized object</returns>
        public static T Deserialize<T, R>(this Deserializer<R> deserializer, IBonded<T> bonded)
            where R : ICloneable<R>
        {
            var b = bonded as Bonded<T, R>;
            if (b == null)
                throw new InvalidOperationException(string.Format("Expected Bonded<{0}, {1}>", typeof(T), typeof(R)));

            return (T)deserializer.deserialize[0](b.reader.Clone());
        }
    }
}
