// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using Bond.IO;

    /// <summary>
    /// Interface representing bonded payload of unknown type
    /// </summary>
    public interface IBonded
    {
        /// <summary>
        /// Serialize content of IBonded instance to protocol writer
        /// </summary>
        /// <typeparam name="W">Type of the protocol writer</typeparam>
        /// <param name="writer">Protocol writer instance</param>
        void Serialize<W>(W writer);

        /// <summary>
        /// Deserialize an object of type U from the IBonded instance
        /// </summary>
        /// <typeparam name="U">Type of object to deserialize</typeparam>
        /// <returns>Deserialized object</returns>
        U Deserialize<U>();

        /// <summary>
        /// Convert to an instance of IBonded&lt;U>
        /// </summary>
        /// <typeparam name="U">Type representing a Bond schema</typeparam>
        /// <returns>An instance of IBonded&lt;U></returns>
        IBonded<U> Convert<U>();
    }

    /// <summary>
    /// Interface representing the schema type bonded&lt;T>
    /// </summary>
    /// <typeparam name="T">Type representing a Bond schema</typeparam>
    public interface IBonded<out T> : IBonded
    {
        /// <summary>
        /// Deserialize an object of type T from the IBonded&lt;T> instance
        /// </summary>
        /// <returns>Deserialized object</returns>
        T Deserialize();
    }

    /// <summary>
    /// Implementation of IBonded&lt;T> holding an instance of T
    /// </summary>
    /// <typeparam name="T">Type representing a Bond schema</typeparam>
    public sealed class Bonded<T> : IBonded<T>
    {
        /// <summary>
        /// A static, readonly field representing an empty instance of Bonded&lt;T>
        /// </summary>
        [System.Diagnostics.CodeAnalysis.SuppressMessage(
            "Microsoft.Security", "CA2104:DoNotDeclareReadOnlyMutableReferenceTypes")]
        public static readonly Bonded<T> Empty = new Bonded<T>(GenericFactory.Create<T>());
        readonly T instance;

        /// <summary>
        /// Creater Bonded&lt;T> from an instance of T
        /// </summary>
        /// <param name="instance">Object of type T</param>
        public Bonded(T instance)
        {
            this.instance = instance;
        }
        
        T IBonded<T>.Deserialize()
        {
            return Clone<T>.From(instance);
        }

        U IBonded.Deserialize<U>()
        {
            return Clone<U>.From(instance);
        }

        void IBonded.Serialize<W>(W writer)
        {
            Serialize.To(writer, instance);
        }

        IBonded<U> IBonded.Convert<U>()
        {
            return this as IBonded<U>;
        }
    }
    
    /// <summary>
    /// Implementation of IBonded&lt;T> holding data serialized using protocol R
    /// </summary>
    /// <typeparam name="T">Type representing a Bond schema</typeparam>
    /// <typeparam name="R">Protocol reader</typeparam>
    public sealed class Bonded<T, R> : IBonded<T>
        where R : ICloneable<R>
    {
        internal readonly R reader;
        readonly RuntimeSchema schema;

        /// <summary>
        /// Create an instance of Bonded&lt;T, R> from a protocol reader
        /// </summary>
        /// <param name="reader">Protocol reader instance</param>
        public Bonded(R reader)
        {
            this.reader = reader.Clone();
        }

        /// <summary>
        /// Create an instance of Bonded&lt;T, R> from a protocol reader and runtime schema
        /// </summary>
        /// <param name="reader">Protocol reader instance</param>
        /// <param name="schema">Runtime schema of the payload</param>
        public Bonded(R reader, RuntimeSchema schema)
        {
            this.reader = reader.Clone();
            this.schema = schema;
        }

        T IBonded<T>.Deserialize()
        {
            // TODO: This is very bad from performance perspective.
            // It affects only rare cases (untagged payload with a struct deserialized as a bonded<T>)
            // but we need a better solution. The same applies to Deserialize<U> and Serialize<W>.
            if (schema.HasValue)
                return new Deserializer<R>(typeof(T), schema).Deserialize<T>(reader.Clone());
            
            return Deserialize<T>.From(reader.Clone());
        }

        U IBonded.Deserialize<U>()
        {
            if (schema.HasValue)
                return new Deserializer<R>(typeof(U), schema).Deserialize<U>(reader.Clone());
            
            return Deserialize<U>.From(reader.Clone());
        }

        void IBonded.Serialize<W>(W writer)
        {
            if (schema.HasValue)
                new Transcoder<R, W>(schema).Transcode(reader.Clone(), writer);
            else
                Transcode<T>.FromTo(reader.Clone(), writer);
        }

        IBonded<U> IBonded.Convert<U>()
        {
            return new Bonded<U, R>(reader, schema);
        }
    }

    internal class BondedVoid<R> : IBonded 
        where R : ICloneable<R>
    {
        readonly R reader;
        readonly RuntimeSchema schema;

        public BondedVoid(R reader)
        {
            this.reader = reader;
        }

        public BondedVoid(R reader, RuntimeSchema schema)
        {
            this.reader = reader;
            this.schema = schema;
        }

        U IBonded.Deserialize<U>()
        {
            if (schema.HasValue)
                return new Deserializer<R>(typeof(U), schema).Deserialize<U>(reader.Clone());

            return Deserialize<U>.From(reader.Clone());
        }

        void IBonded.Serialize<W>(W writer)
        {
            if (schema.HasValue)
                new Transcoder<R, W>(schema).Transcode(reader.Clone(), writer);
            else
                Transcode.FromTo(reader.Clone(), writer);
        }

        IBonded<U> IBonded.Convert<U>()
        {
            return new Bonded<U, R>(reader, schema);
        }
    }
}
