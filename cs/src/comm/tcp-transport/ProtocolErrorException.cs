namespace Bond.Comm.Tcp
{
    using System;

    public class ProtocolErrorException : Exception
    {
        public ProtocolErrorException(string message) : base(message)
        {
        }
    }
}
