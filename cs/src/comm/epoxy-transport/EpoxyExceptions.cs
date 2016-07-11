// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Epoxy
{
    using System;

    public class EpoxyFailedToResolveException : TransportException
    {
        public EpoxyFailedToResolveException(string message, Exception innerException = null)
            : base(message, innerException)
        {
        }
    }

    public class EpoxyProtocolErrorException : TransportException
    {
        public EpoxyProtocolErrorException(
            string message,
            Exception innerException = null,
            IBonded<Error> details = null)
            : base(message, innerException)
        {
            Details = details;
        }

        public IBonded<Error> Details { get; }
    }
}
