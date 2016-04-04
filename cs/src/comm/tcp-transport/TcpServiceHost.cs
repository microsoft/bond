// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Tcp
{
    using System;
    using System.Collections.Generic;
    using System.Linq;
    using System.Threading;
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

        public void Register<T>(T service) where T : IService
        {
            var methodNames = new SortedSet<string>();
            lock (m_lock)
            {
                foreach (var serviceMethod in service.Methods)
                {
                    m_dispatchTable.Add(serviceMethod.MethodName, serviceMethod.Callback);
                    methodNames.Add(serviceMethod.MethodName);
                }
            }

            Log.Information("{0}.{1}: Registered {2} with methods: {3}",
                nameof(TcpServiceHost), nameof(Register), typeof(T).Name, string.Join(", ", methodNames));
        }

        public void DispatchRequest(TcpHeaders headers, TcpConnection connection, IMessage request)
        {
            Log.Information("{0}.{1}: Got request {2}/{3} from {4}.",
                nameof(TcpServiceHost), nameof(DispatchRequest), headers.request_id, headers.method_name, connection);
            ServiceCallback callback;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(headers.method_name, out callback))
                {
                    var message = LogUtil.FatalAndReturnFormatted("{0}.{1}: Got request for unknown method {2}.",
                        nameof(TcpServiceHost), nameof(DispatchRequest), headers.method_name);
                    throw new ProtocolErrorException(message);
                }
            }

            var context = new TcpReceiveContext(connection);

            // explicitly queue in the thread pool so that we can read the next frame from the connection
            Task.Run(async () =>
            {
                IMessage result;

                try
                {
                    result = await callback(request, context, CancellationToken.None);
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

                Log.Debug("{0}.{1}: Replying to request {2}/{3}.",
                    nameof(TcpServiceHost), nameof(DispatchRequest), headers.request_id, headers.method_name);
                await connection.SendReplyAsync(headers.request_id, result);
            });
        }
    }
}
