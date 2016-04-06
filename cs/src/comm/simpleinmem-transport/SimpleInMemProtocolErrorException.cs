// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;

    public class SimpleInMemProtocolErrorException : TransportException
    {
        public SimpleInMemProtocolErrorException(string message) : base(message)
        {
        }

        public SimpleInMemProtocolErrorException(string message, Exception innerException) : base(message, innerException)
        {
        }
    }
}