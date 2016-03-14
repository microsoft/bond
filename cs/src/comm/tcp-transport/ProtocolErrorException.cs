// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

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
