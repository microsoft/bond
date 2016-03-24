// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Examples.PingPong
{
    using System.Threading.Tasks;
    using Bond.Comm;

    interface IPingPongService
    {
        Task<IMessage<PingResponse>> PingAsync(IMessage<PingRequest> request);
    }
}
