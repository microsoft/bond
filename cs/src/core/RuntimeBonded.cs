// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;

    /// <summary>
    /// Interface representing bonded payload of unknown type
    /// </summary>
    public interface IRuntimeBonded
    {
        /// <summary>
        /// Deserialize an object from the IRuntimeBonded instance
        /// </summary>
        /// <returns>Deserialized object</returns>
        object Deserialize();
    }

    /// <summary>
    /// Interface representing the schema type bonded&lt;T>
    /// </summary>
    /// <typeparam name="T">Type representing a Bond schema</typeparam>
    public interface IRuntimeBonded<out T> : IRuntimeBonded
    {
        /// <summary>
        /// Deserialize an object of type T from the IRuntimeBonded&lt;T> instance
        /// </summary>
        /// <returns>Deserialized object</returns>
        new T Deserialize();
    }

    public class RuntimeBonded<T> : IRuntimeBonded<T>
    {
        private readonly Func<T> valueDeserializer;

        public RuntimeBonded(T value)
            : this(() => value)
        {
        }

        public RuntimeBonded(Func<T> valueDeserializer)
        {
            this.valueDeserializer = valueDeserializer;
        }

        object IRuntimeBonded.Deserialize()
        {
            return Deserialize();
        }

        public T Deserialize()
        {
            return valueDeserializer();
        }
    }
}
