// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Linq;
    using Bond.Expressions;
    using Bond.IO;
    using Bond.Protocols;

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
        /// Transcode data from protocol reader <typeparamref name="R"/> to protocol writer <typeparamref name="W"/>
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
    /// Transcode payload from one protocol into another using compile-time schema <typeparamref name="T"/>
    /// </summary>
    /// <typeparam name="T">Type representing a Bond schema</typeparam>
    public static class Transcode<T>
    {
        static class Cache<R, W>
        {
            public static readonly Transcoder<R, W> Instance = new Transcoder<R, W>(typeof(T));
        }

        /// <summary>
        /// Transcode data from protocol reader <typeparamref name="R"/> to protocol writer <typeparamref name="W"/>
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
    /// Transcoder from protocol reader <typeparamref name="R"/> to protocol writer <typeparamref name="W"/>
    /// </summary>
    /// <typeparam name="R">Protocol reader</typeparam>
    /// <typeparam name="W">Protocol writer</typeparam>
    public class Transcoder<R, W>
    {
        /// <summary>
        /// Create a transcoder for payloads with specified runtime schema
        /// </summary>
        /// <param name="schema">Payload schema, required for transcoding from untagged protocols</param>
        public Transcoder(RuntimeSchema schema)
            : this(schema, null)
        {}

        /// <summary>
        /// Create a transcoder for payloads with specified compile-time schema
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        public Transcoder(Type type)
            : this(type, null)
        {}

        /// <summary>
        /// Create a transcoder for payloads with specified runtime schema
        /// </summary>
        /// <param name="schema">Payload schema, required for transcoding from untagged protocols</param>
        /// <param name="parser">Custom <see cref="IParser"/> instance</param>
        public Transcoder(RuntimeSchema schema, IParser parser)
        {
            if (typeof(W).GetAttribute<FirstPassWriterAttribute>() != null)
            {
                helper = new TwoPassTranscoderHelper<CompactBinaryCounter>(schema, parser);
            }
            else
            {
                helper = new TranscoderHelper(schema, parser);
            }
        }

        /// <summary>
        /// Create a transcoder for payloads with specified compile-time schema
        /// </summary>
        /// <param name="type">Type representing a Bond schema</param>
        /// <param name="parser">Custom <see cref="IParser"/> instance</param>
        public Transcoder(Type type, IParser parser)
        {
            var firstPassAttribute = typeof(W).GetAttribute<FirstPassWriterAttribute>();
            if (firstPassAttribute != null)
            {
                Type firstPassHelperType = typeof(TwoPassTranscoderHelper<>).MakeGenericType(typeof(R), typeof(W), firstPassAttribute.Type);

                helper = (TranscoderHelper)Activator.CreateInstance(firstPassHelperType, type, parser);
            }
            else
            {
                helper = new TranscoderHelper(type, parser);
            }
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
            helper.Transcode(reader, writer);
        }

        TranscoderHelper helper;

        private class TranscoderHelper
        {
            readonly Action<R, W>[] transcode;

            public TranscoderHelper(RuntimeSchema schema, IParser parser)
            {
                transcode = Generate(schema, parser);
            }

            public TranscoderHelper(Type type, IParser parser)
            {
                transcode = Generate(type, parser);
            }

            virtual public void Transcode(R reader, W writer)
            {
                transcode[0](reader, writer);
            }

            Action<R, W>[] Generate<S>(S schema, IParser parser)
            {
                parser = parser ?? ParserFactory<R>.Create(schema);
                return SerializerGeneratorFactory<R, W>.Create(
                        (r, w, i) => transcode[i](r, w), schema)
                    .Generate(parser)
                    .Select(lambda => lambda.Compile()).ToArray();
            }
        }

        private class TwoPassTranscoderHelper<FPW> : TranscoderHelper
        {
            readonly Action<R, FPW>[] firstPassTranscode;

            public TwoPassTranscoderHelper(RuntimeSchema schema, IParser parser):
                base(schema)
            {
                firstPassTranscode = GenerateFirstPass(schema, parser);
            }

            public TwoPassTranscoderHelper(Type type):
                base(type)
            {
                firstPassTranscode = GenerateFirstPass(type, parser);
            }

            public override void Transcode(R reader, W writer)
            {
                var firstPassWriter = ((ITwoPassProtocolWriter)writer).GetFirstPassWriter();
                if (firstPassWriter != null)
                {
                    R clonedReader = ((ICloneable<R>)reader).Clone();
                    firstPassTranscode[0](clonedReader, (FPW)firstPassWriter);
                }

                base.Transcode(reader, writer);
            }

            Action<R, FPW>[] GenerateFirstPass<S>(S schema, IParser parser)
            {
                var parser = parser ?? ParserFactory<R>.Create(schema);
                return SerializerGeneratorFactory<R, FPW>.Create(
                        (r, w, i) => firstPassTranscode[i](r, w), schema)
                    .Generate(parser)
                    .Select(lambda => lambda.Compile()).ToArray();
            }
        }
    }
}
