// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;

    // Make this immutable? But if we do, how does someone take the
    // defaults and the perturb only what they want to change for a
    // per-request basis?
    public class TransportArgs
    {
        public TimeSpan SendTimeout;
        public TimeSpan ReceiveTimeout;

        public virtual T Clone<T>() where T : TransportArgs, new()
        {
            return new T
            {
                SendTimeout = SendTimeout,
                ReceiveTimeout = ReceiveTimeout,
            };
        }
    }
}
