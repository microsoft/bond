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
            lock (m_lock)
            {
                foreach (var serviceMethod in service.Methods)
                {
                    m_dispatchTable.Add(serviceMethod.MethodName, serviceMethod.Callback);
                }
            }

            var methods = service.Methods.Select(m => m.MethodName).ToList();
            methods.Sort();
            Log.Information($"TcpServiceHost.Register: Registered {typeof(T).Name} with methods: {string.Join(", ", methods)}");
        }

        public void DispatchRequest(TcpHeaders headers, TcpConnection connection, IMessage request)
        {
            Log.Information($"TcpServiceHost.DispatchRequest: Got request {headers.request_id}/{headers.method_name}."
                + $"from {connection}.");
            ServiceCallback callback;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(headers.method_name, out callback))
                {
                    var message = $"Got request for unknown method {headers.method_name}.";
                    Log.Error("TcpServiceHost.DispatchRequest: " + message);
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

                Log.Debug($"TcpServiceHost.DispatchRequest: Replying to request {headers.request_id}/{headers.method_name}.");
                await connection.SendReplyAsync(headers.request_id, result);
            });
        }
    }
}
