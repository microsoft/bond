// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System.Collections.Generic;
    using System.Threading.Tasks;

    internal class SimpleInMemServiceHost
    {
        private object m_lock;
        private Dictionary<string, ServiceCallback> m_dispatchTable;


        public SimpleInMemServiceHost()
        {
            m_lock = new object();
            m_dispatchTable = new Dictionary<string, ServiceCallback>();
        }

        public void Register(IService service)
        {
            lock (m_lock)
            {
                foreach (var serviceMethod in service.Methods)
                {
                    m_dispatchTable.Add(serviceMethod.MethodName, serviceMethod.Callback);
                }
            }
        }

        public void Deregister(IService service)
        {
            lock (m_lock)
            {
                foreach (var serviceMethod in service.Methods)
                {
                    m_dispatchTable.Remove(serviceMethod.MethodName);
                }
            }
        }

        public async Task<IBonded> DispatchRequest(SimpleInMemHeaders headers, SimpleInMemConnection connection, IBonded request, TaskCompletionSource<IBonded> taskSource)
        {
            ServiceCallback callback;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(headers.method_name, out callback))
                {
                    // TODO: this should just be something we log
                    throw new ProtocolErrorException("Method not found: " + headers.method_name);
                }
            }

            var context = new SimpleInMemReceiveContext(connection);
            return await callback(request, context);
        }
    }
}
