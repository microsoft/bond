// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    internal class CalculatorService : IService
    {
        public const string ExpectedExceptionMessage = "This method is expected to throw.";

        private IEnumerable<ServiceMethodInfo> m_methods;

        internal CalculatorService()
        {
            var methodsInfo = new List<ServiceMethodInfo>();
            var methodInfo = new ServiceMethodInfo
            {
                MethodName = "Add",
                Callback = Add,
            };
            methodsInfo.Add(methodInfo);

            methodInfo = new ServiceMethodInfo
            {
                MethodName = "Subtract",
                Callback = Subtract,
            };
            methodsInfo.Add(methodInfo);

            methodInfo = new ServiceMethodInfo
            {
                MethodName = "Multiply",
                Callback = Multiply,
            };
            methodsInfo.Add(methodInfo);

            m_methods = methodsInfo;
        }

        public IEnumerable<ServiceMethodInfo> Methods
        {
            get
            {
                return m_methods;
            }
        }

        internal Task<IMessage> Add(IMessage request, ReceiveContext context, CancellationToken ct)
        {
            PairedInput req = request.Convert<PairedInput>().Payload.Deserialize();
            var res = new Output { Result = req.First + req.Second };

            return Task.FromResult<IMessage>(Message.FromPayload(res));
        }

        internal Task<IMessage> Subtract(IMessage request, ReceiveContext context, CancellationToken ct)
        {
            PairedInput req = request.Convert<PairedInput>().Payload.Deserialize();
            var res = new Output { Result = req.First - req.Second };

            return Task.FromResult<IMessage>(Message.FromPayload(res));
        }

        internal Task<IMessage> Multiply(IMessage request, ReceiveContext context, CancellationToken ct)
        {
            throw new NotImplementedException(ExpectedExceptionMessage);
        }
    }
}
