// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    public class SimpleInMemSendContext : SendContext
    {
        public SimpleInMemSendContext(SimpleInMemConnection connection, ConnectionMetrics connectionMetrics, RequestMetrics requestMetrics)
            : base(connectionMetrics, requestMetrics)
        {
            Connection = connection;
        }

        public override Connection Connection { get; }
    }

    public class SimpleInMemReceiveContext : ReceiveContext
    {
        public SimpleInMemReceiveContext(SimpleInMemConnection connection, ConnectionMetrics connectionMetrics, RequestMetrics requestMetrics)
            : base(connectionMetrics, requestMetrics)
        {
            Connection = connection;
        }

        public override Connection Connection { get; }
    }
}
