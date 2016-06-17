// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.Tls
{
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;
    using Void = Bond.Void;

    public class SimpleService : SimpleServiceBase
    {
        public override Task<IMessage<SimpleResult>> SimpleMethodAsync(IMessage<Void> param, CancellationToken ct)
        {
            return Task.FromResult(Message.FromPayload(new SimpleResult {int_value = 42}));
        }
    }
}
