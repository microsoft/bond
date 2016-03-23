// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    public class SimpleInMemReceiveContext : ReceiveContext
    {
        SimpleInMemConnection m_connection;

        public SimpleInMemReceiveContext(SimpleInMemConnection connection)
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
