// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    using System;
    using System.Collections.Generic;
    using System.Threading.Tasks;

    internal class TcpServiceHost
    {
        private TcpTransport m_parentTransport;

        private object m_lock;
        private Dictionary<string, ServiceCallback> m_dispatchTable;

        public TcpServiceHost(TcpTransport parentTransport)
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

        public void DispatchRequest(TcpHeaders headers, TcpConnection connection, IMessage request)
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

            var context = new TcpReceiveContext(connection);

            // explicitily queue in the thread pool so that we can read the next frame from the connection
            Task.Run(async () =>
            {
                IMessage result;

                try
                {
                    result = await callback(request, context);
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

                await connection.SendReplyAsync(headers.request_id, result);
            });
        }
    }
}
