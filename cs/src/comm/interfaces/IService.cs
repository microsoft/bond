// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;

    public delegate Task<IMessage> ServiceCallback(IMessage request, ReceiveContext context, CancellationToken ct);

    public class ServiceMethodInfo
    {
        public string MethodName;
        public ServiceCallback Callback;
    }

    // TODO: replace this will reflection-based discovery of methods with the ServiceMethod attribute.
    public interface IService
    {
        IEnumerable<ServiceMethodInfo> Methods { get; }
    }
}
