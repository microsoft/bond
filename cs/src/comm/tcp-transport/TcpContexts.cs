// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    public class TcpReceiveContext : ReceiveContext
    {
        TcpConnection m_connection;

        public TcpReceiveContext(TcpConnection connection)
        {
            m_connection = connection;
        }

        public override Connection Connection
        {
            get
            {
                return m_connection;
            }
        }
    }
}
