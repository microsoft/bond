// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond
{
    using System;
    using System.Linq.Expressions;

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

    /// <summary>
    /// Returns an expression to create an object
    /// </summary>
    /// <param name="type">Type of field/value the created object will be assigned to</param>
    /// <param name="schemaType">Type in the schema</param>
    /// <param name="arguments">Optional, type-specific argument(s). For example for containers 
    /// number of items, for IBonded&lt;T> the IBonded instance from the parser.</param>
    /// <returns></returns>
    public delegate Expression Factory(Type type, Type schemaType, params Expression[] arguments);
}
