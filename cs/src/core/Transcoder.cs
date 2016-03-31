// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Linq;
    using Bond.Expressions;

    /// <summary>
    /// Transcode payload from one protocol into another
    /// </summary>
    public static class Transcode
    {
        static class Cache<R, W>
        {
            public static readonly Transcoder<R, W> Instance = new Transcoder<R, W>(); 
        }

        /// <summary>
        /// Transcode data from protocol reader R to protocol writer W
        /// </summary>
        /// <typeparam name="R">Protocol reader</typeparam>
        /// <typeparam name="W">Protocol writer</typeparam>
        /// <param name="reader">Reader instance representing source payload</param>
        /// <param name="writer">Writer instance</param>
        public static void FromTo<R, W>(R reader, W writer)
        {
            Cache<R, W>.Instance.Transcode(reader, writer);
        }
    }

    /// <summary>
    /// Transcode payload from one protocol into another using compile-time schema T
    /// </summary>
    /// <typeparam name="T">Type representing a Bond schema</typeparam>
    public static class Transcode<T>
    {
        static class Cache<R, W>
        {
            public static readonly Transcoder<R, W> Instance = new Transcoder<R, W>(typeof(T));
        }

        /// <summary>
        /// Transcode data from protocol reader R to protocol writer W
        /// </summary>
        /// <typeparam name="R">Protocol reader</typeparam>
        /// <typeparam name="W">Protocol writer</typeparam>
        /// <param name="reader">Reader instance representing source payload</param>
        /// <param name="writer">Writer instance</param>
        public static void FromTo<R, W>(R reader, W writer)
        {
            Cache<R, W>.Instance.Transcode(reader, writer);
        }
    }

    /// <summary>
    /// Transcoder from protocol reader R to protocol writer W
    /// </summary>
    /// <typeparam name="R">Protocol reader</typeparam>
    /// <typeparam name="W">Protocol writer</typeparam>
    public class Transcoder<R, W>
    {
        readonly Action<R, W>[] transcode;

        /// <summary>
        /// Create a transcoder for payloads with specified runtime schema
        /// </summary>
        /// <param name="schema">Payload schema, required for transcoding from untagged protocols</param>
        /// <param name="parser">Custom IParser instance</param>
        public Transcoder(RuntimeSchema schema, IParser parser = null)
        {
            transcode = Generate(schema, parser);
        }

        /// <summary>
        /// Create a transcoder for payloads with specified compile-time schema
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="parser">Custom IParser instance</param>
        public Transcoder(Type type, IParser parser = null)
        {
            transcode = Generate(type, parser);
        }

        // Create a transcoder
        public Transcoder()
            : this(RuntimeSchema.Empty)
        {}

        /// <summary>
        /// Transcode payload
        /// </summary>
        /// <param name="reader">Reader instance representing source payload</param>
        /// <param name="writer">Writer instance</param>
        public void Transcode(R reader, W writer)
        {
            transcode[0](reader, writer);
        }

        Action<R, W>[] Generate<S>(S schema, IParser parser = null)
        {
            parser = parser ?? ParserFactory<R>.Create(schema);

            return SerializerGeneratorFactory<R, W>.Create((r, w, i) => transcode[i](r, w), schema)
                                                   .Generate(parser)
                                                   .Select(lambda => lambda.Compile()).ToArray();
        }
    }
}
