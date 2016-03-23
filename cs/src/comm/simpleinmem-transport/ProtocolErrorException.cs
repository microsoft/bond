// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;

    public class ProtocolErrorException : Exception
    {
        public ProtocolErrorException(string message) : base(message)
        {
        }
    }

    public class InMemTransportListenerException : Exception
    {
        public InMemTransportListenerException(string message) : base(message)
        {
        }
    }
}
