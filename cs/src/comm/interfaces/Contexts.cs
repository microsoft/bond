// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    public abstract class Context
    {
        // For received messages, the connection the message was
        // received on. For messages being sent, will be set to the connection
        // that is being used.
        // Allows getting things like the remote endpoint information.
        // Maybe be null if a connection-less protocol is being used.
        public abstract Connection Connection { get; }
    }

    public abstract class SendContext : Context
    {
        // TransportArgs specific to this request. If null, will use the transport/connection default.
        public abstract TransportArgs TransportArgs { get; }
    }

    public abstract class ReceiveContext : Context
    {
    }
}
