// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    /// <summary>
    /// Represents the details about the current operation.
    /// </summary>
    public abstract class Context
    {
        /// <summary>
        /// The connection being used.
        /// </summary>
        /// <remarks>
        /// <para>
        /// May be <c>null</c> if a connection-less protocol is being used.
        /// </para>
        /// <para>
        /// For received messages, this is the connection the message was
        /// received on. For messages that are being sent, this is the
        /// connection that is being used to transmit the message.
        /// </para>
        /// <para>
        /// Typically, this is cast to a more derived type, depending on the
        /// transport that is being used, to get access to the
        /// transport-specific members of a connection.
        /// </para>
        /// </remarks>
        public abstract Connection Connection { get; }
    }

    /// <summary>
    /// Represents the details about the current send operation.
    /// </summary>
    public abstract class SendContext : Context
    {
    }

    /// <summary>
    /// Represents the details about the receive operation.
    /// </summary>
    public abstract class ReceiveContext : Context
    {
    }
}
