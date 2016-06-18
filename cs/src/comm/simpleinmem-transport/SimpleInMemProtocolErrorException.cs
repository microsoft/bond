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

        public SimpleInMemProtocolErrorException(string message, Exception innerException) : this(
            message,
            details: null,
            innerException: innerException)
        {
        }

        public SimpleInMemProtocolErrorException(
            string message,
            Error details,
            Exception innerException)
            : base(message, innerException)
        {
            Details = details;
        }

        public Error Details { get; }
    }
}
