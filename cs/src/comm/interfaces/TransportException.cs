// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;

    /// <summary>
    /// Represents an transport-level exception.
    /// </summary>
    /// <remarks>
    /// Used as the base exception class for transport-specific exception.
    /// </remarks>
    public class TransportException : Exception
    {
        /// <summary>
        /// Creates a TransportException with the given message.
        /// </summary>
        /// <param name="message">The message.</param>
        public TransportException(string message) : base(message)
        {
        }

        /// <summary>
        /// Creates a TransportException with the given message and inner
        /// exception.
        /// </summary>
        /// <param name="message">The message.</param>
        /// <param name="innerException">The inner exception.</param>
        public TransportException(string message, Exception innerException) : base(message, innerException)
        {
        }
    }
}