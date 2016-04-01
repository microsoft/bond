// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.SimpleInMem
{
    using System;
    using System.Collections;
    using System.Collections.Generic;
    using System.Linq;
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

            Log.Information($"SimpleInMemServiceHost.Register: Registered {typeof(T).Name} with methods: {String.Join(", ", methodNames)}");
        }

        public void Deregister<T>(T service) where T : IService
        {
            lock (m_lock)
            {
                foreach (var serviceMethod in service.Methods)
                {
                    m_dispatchTable.Remove(serviceMethod.MethodName);
                }
            }
            Log.Information($"SimpleInMemServiceHost.Deregister: Deregistered {typeof(T).Name}.");
        }

        public async Task<IMessage> DispatchRequest(SimpleInMemHeaders headers, SimpleInMemConnection connection, IMessage message, TaskCompletionSource<IMessage> taskSource)
        {
            Log.Information($"SimpleInMemServiceHost.DispatchRequest: Got request {headers.request_id}/{headers.method_name} "
                + $"from {connection}.");
            ServiceCallback callback;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(headers.method_name, out callback))
                {
                    var errMsg = $"Got request for unknown method {headers.method_name}.";
                    Log.Error("SimpleInMemServiceHost.DispatchRequest: " + errMsg);
                    throw new ProtocolErrorException(errMsg);
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
