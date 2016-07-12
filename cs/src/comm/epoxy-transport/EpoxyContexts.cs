// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    public class EpoxySendContext : SendContext
    {
        public EpoxySendContext(EpoxyConnection connection, ConnectionMetrics connectionMetrics, RequestMetrics requestMetrics)
            : base(connectionMetrics, requestMetrics)
        {
            Connection = connection;
        }

        public override Connection Connection { get; }
    }

    public class EpoxyReceiveContext : ReceiveContext
    {
        public EpoxyReceiveContext(EpoxyConnection connection, ConnectionMetrics connectionMetrics, RequestMetrics requestMetrics)
            : base(connectionMetrics, requestMetrics)
        {
            Connection = connection;
        }

        public override Connection Connection { get; }
    }
}
