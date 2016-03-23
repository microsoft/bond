// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace UnitTest.SimpleInMem
{
    using Bond;
    using Bond.Comm;
    using System;
    using System.Collections.Generic;
    using System.Threading.Tasks;

    internal class CalculatorService : IService
    {
        private IEnumerable<ServiceMethodInfo> m_methods;

        internal CalculatorService()
        {
            var methodsInfo = new List<ServiceMethodInfo>();
            var methodInfo = new ServiceMethodInfo
            {
                MethodName = "Add",
                Callback = new ServiceCallback(Add),
            };
            methodsInfo.Add(methodInfo);

            methodInfo = new ServiceMethodInfo
            {
                MethodName = "Subtract",
                Callback = new ServiceCallback(Subtract),
            };
            methodsInfo.Add(methodInfo);

            methodInfo = new ServiceMethodInfo
            {
                MethodName = "Multiply",
                Callback = new ServiceCallback(Multiply),
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

        internal Task<IBonded> Add(IBonded request, ReceiveContext context)
        {
            PairedInput req = request.Deserialize<PairedInput>();
            var res = new Output { Result = req.First + req.Second };

            return Task.FromResult<IBonded>(new Bonded<Output>(res));
        }

        internal Task<IBonded> Subtract(IBonded request, ReceiveContext context)
        {
            PairedInput req = request.Deserialize<PairedInput>();
            var res = new Output { Result = req.First - req.Second };

            return Task.FromResult<IBonded>(new Bonded<Output>(res));
        }

        internal Task<IBonded> Multiply(IBonded request, ReceiveContext context)
        {
            throw new NotImplementedException();
        }

        private IBonded GetBonded<T>(T t)
        {
            return new Bonded<T>(t);
        }
    }
}