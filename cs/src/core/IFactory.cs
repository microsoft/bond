// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;

    /// <summary>
    /// Interface for custom factory to create objects during deserialization
    /// </summary>
    public interface IFactory
    {
        /// <summary>
        /// Create an object
        /// </summary>
        /// <param name="type">Type of field/value the created object will be assigned to</param>
        /// <param name="schemaType">Type in the schema</param>
        /// <returns></returns>
        object CreateObject(Type type, Type schemaType);

        /// <summary>
        /// Create a container
        /// </summary>
        /// <param name="type">Type of field/value the created object will be assigned to</param>
        /// <param name="schemaType">Type in the schema</param>
        /// <param name="count">Initial capacity</param>
        /// <returns></returns>
        object CreateContainer(Type type, Type schemaType, int count);
    }
}
