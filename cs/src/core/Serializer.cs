// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Linq;
    using System.Threading;

    using Bond.Expressions;
    using Bond.Protocols;
    using Bond.Internal.Reflection;

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
        /// Serialize object of type <typeparamref name="T"/> to protocol writer of type <typeparamref name="W"/>
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
        /// Serialize <see cref="IBonded{T}" /> to protocol writer of type <typeparamref name="W"/>
        /// </summary>
        /// <typeparam name="W">Protocol writer</typeparam>
        /// <typeparam name="T">Type representing a Bond schema</typeparam>
        /// <param name="writer">Writer instance</param>
        /// <param name="bonded"><see cref="IBonded"/> instance</param>
        public static void To<W, T>(W writer, IBonded<T> bonded)
        {
            bonded.Serialize(writer);
        }

        /// <summary>
        /// Serialize <see cref="IBonded"/> to protocol writer of type <typeparamref name="W"/>
        /// </summary>
        /// <typeparam name="W">Protocol writer</typeparam>
        /// <param name="writer">Writer instance</param>
        /// <param name="bonded"><see cref="IBonded"/> instance</param>
        public static void To<W>(W writer, IBonded bonded)
        {
            bonded.Serialize(writer);
        }
    }

    /// <summary>
    /// Serializer for protocol writer <typeparamref name="W"/>
    /// </summary>
    /// <typeparam name="W">Protocol writer</typeparam>
    public class Serializer<W>
    {
        static readonly Type helperType;
        readonly SerializerHelper helper;

        static Serializer()
        {
            var firstPassAttribute = typeof(W).GetAttribute<FirstPassWriterAttribute>();
            if (firstPassAttribute != null)
            {
                if (!typeof(ITwoPassProtocolWriter).IsAssignableFrom(typeof(W)))
                {
                    throw new ArgumentException("Writers with FirstPassWriterAttribute must implement ITwoPassProtocolWriter");
                }

                helperType = typeof(TwoPassSerializerHelper<>).MakeGenericType(typeof(W), firstPassAttribute.Type);
            }
            else
            {
                helperType = typeof(SerializerHelper);
            }
        }

        /// <summary>
        /// Create a serializer for specified type
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        public Serializer(Type type) : this(type, null, inlineNested: true) { }

        /// <summary>
        /// Create a serializer for specified type
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="parser">Custom <see cref="IParser"/> instance</param>
        public Serializer(Type type, IParser parser) : this(type, parser, inlineNested: true) { }

        /// <summary>
        /// Create a serializer for specified type
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="inlineNested">Indicates whether nested struct serialization code may be inlined</param>
        public Serializer(Type type, bool inlineNested) : this(type, null, inlineNested) { }

        /// <summary>
        /// Create a serializer for specified type
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="parser">Custom <see cref="IParser"/> instance</param>
        /// <param name="inlineNested">Indicates whether nested struct serialization code may be inlined</param>
        public Serializer(Type type, IParser parser, bool inlineNested)
        {
            parser = parser ?? new ObjectParser(type);

            helper = (SerializerHelper)Activator.CreateInstance(helperType, parser, type, inlineNested);

        }

        /// <summary>
        /// Serialize object using protocol writer of type <typeparamref name="W"/>
        /// </summary>
        /// <param name="obj">Object to serialize</param>
        /// <param name="writer">Writer instance</param>
        /// <remarks>
        /// The object must be of type used to create the <see cref="Serializer{W}"/>, otherwise behavior is undefined
        /// </remarks>
        public void Serialize(object obj, W writer)
        {
            helper.Serialize(obj, writer);
        }

        class SerializerHelper
        {
            readonly Action<object, W>[] serialize;

            public SerializerHelper(ObjectParser parser, Type type, bool inlineNested)
            {
                serialize = SerializerGeneratorFactory<object, W>.Create(
                    (o, w, i) => serialize[i](o, w), type, inlineNested)
                .Generate(parser)
                .Select(lambda => lambda.Compile()).ToArray();
            }

            public virtual void Serialize(object obj, W writer)
            {
                serialize[0](obj, writer);
            }
        }

        class TwoPassSerializerHelper<FPW> : SerializerHelper
        {
            readonly Lazy<Action<object, FPW>[]> firstPassSerialize;

            public TwoPassSerializerHelper(ObjectParser parser, Type type, bool inlineNested) :
                base(parser, type, inlineNested)
            {
                firstPassSerialize = new Lazy<Action<object, FPW>[]>(() => {
                    return SerializerGeneratorFactory<object, FPW>.Create(
                        (o, w, i) => firstPassSerialize.Value[i](o, w), type, inlineNested)
                    .Generate(parser)
                    .Select(lambda => lambda.Compile()).ToArray();
                }, LazyThreadSafetyMode.PublicationOnly);
            }

            public override void Serialize(object obj, W writer)
            {
                var firstPassWriter = ((ITwoPassProtocolWriter)writer).GetFirstPassWriter();
                if (firstPassWriter != null)
                {
                    firstPassSerialize.Value[0](obj, (FPW)firstPassWriter);
                }

                base.Serialize(obj, writer);
            }
        }
    }
}
