using System;

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
