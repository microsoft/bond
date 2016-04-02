// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;

    internal class SimpleInMemServiceHost
    {
        private readonly SimpleInMemTransport m_parentTransport;
        private readonly object m_lock;
        private readonly Dictionary<string, ServiceCallback> m_dispatchTable;


        public SimpleInMemServiceHost(SimpleInMemTransport parentTransport)
        {
            m_parentTransport = parentTransport;
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

        public async Task<IMessage> DispatchRequest(SimpleInMemHeaders headers, SimpleInMemConnection connection, IMessage message, TaskCompletionSource<IMessage> taskSource)
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

            IMessage result;

            try
            {
                result = await callback(message, context, CancellationToken.None);
            }
            catch (Exception callbackEx)
            {
                Error error = null;

                try
                {
                    error = m_parentTransport.UnhandledExceptionHandler(callbackEx);
                }
                catch (Exception handlerEx)
                {
                    Transport.FailFastExceptionHandler(handlerEx);
                }

                if (error == null)
                {
                    Transport.FailFastExceptionHandler(callbackEx);
                }

                result = Message.FromError(error);
            }

            return result;
        }
    }
}
