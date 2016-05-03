// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    public class EpoxyReceiveContext : ReceiveContext
    {
        EpoxyConnection m_connection;

        public EpoxyReceiveContext(EpoxyConnection connection)
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
