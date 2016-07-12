// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    /// <summary>
    /// Represents the details about the current operation.
    /// </summary>
    public abstract class Context
    {
        public readonly ConnectionMetrics ConnectionMetrics;
        public readonly RequestMetrics RequestMetrics;

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

        protected Context(ConnectionMetrics connectionMetrics, RequestMetrics requestMetrics)
        {
            ConnectionMetrics = connectionMetrics;
            RequestMetrics = requestMetrics;
        }
    }

    /// <summary>
    /// Represents the details about the current send operation.
    /// </summary>
    public abstract class SendContext : Context
    {

        protected SendContext(ConnectionMetrics connectionMetrics, RequestMetrics requestMetrics)
            : base(connectionMetrics, requestMetrics) {}
    }

    /// <summary>
    /// Represents the details about the receive operation.
    /// </summary>
    public abstract class ReceiveContext : Context
    {
        protected ReceiveContext(ConnectionMetrics connectionMetrics, RequestMetrics requestMetrics)
            : base(connectionMetrics, requestMetrics) {}
    }
}
