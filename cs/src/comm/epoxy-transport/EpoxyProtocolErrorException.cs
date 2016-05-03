// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;

    public class EpoxyProtocolErrorException : TransportException
    {
        public EpoxyProtocolErrorException(string message) : base(message)
        {
        }

        public EpoxyProtocolErrorException(string message, Exception innerException) : base(message, innerException)
        {
        }
    }
}