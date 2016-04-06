// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    using System;

    public class TcpProtocolErrorException : TransportException
    {
        public TcpProtocolErrorException(string message) : base(message)
        {
        }

        public TcpProtocolErrorException(string message, Exception innerException) : base(message, innerException)
        {
        }
    }
}