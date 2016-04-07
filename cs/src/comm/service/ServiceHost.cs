// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Service
{
    using System;
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    public class ServiceHost
    {
        private readonly Transport m_parentTransport;
        private readonly object m_lock;
        private readonly Dictionary<string, ServiceCallback> m_dispatchTable;


        public ServiceHost(Transport parentTransport)
        {
            m_parentTransport = parentTransport;
            m_lock = new object();
            m_dispatchTable = new Dictionary<string, ServiceCallback>();
        }

        public bool IsRegistered(string serviceMethodName)
        {
            lock (m_lock)
            {
                return m_dispatchTable.ContainsKey(serviceMethodName);
            }
        }

        public void Register(IService service)
        {
            var methodNames = new SortedSet<string>();
            lock (m_lock)
            {
                foreach (var serviceMethod in service.Methods)
                {
                    if (m_dispatchTable.ContainsKey(serviceMethod.MethodName))
                    {
                        throw new ArgumentException($"{serviceMethod.MethodName} already registered");
                    }
                }

                foreach (var serviceMethod in service.Methods)
                {
                    m_dispatchTable.Add(serviceMethod.MethodName, serviceMethod.Callback);
                    methodNames.Add(serviceMethod.MethodName);
                }
            }

            Log.Information("{0}.{1}: Registered {2} with methods: {3}",
                nameof(ServiceHost), nameof(Register), nameof(service), string.Join(", ", methodNames));
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
            Log.Information("{0}.{1}: Deregistered {2} with methods: {3}",
                nameof(ServiceHost), nameof(Deregister), typeof (T).Name);
        }

        public async Task<IMessage> DispatchRequest(string methodName, ReceiveContext context, IMessage message)
        {
            Log.Information("{0}.{1}: Got request {2} from {3}.",
                nameof(ServiceHost), nameof(DispatchRequest), methodName, context.Connection);
            ServiceCallback callback;
            IMessage result = null;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(methodName, out callback))
                {
                    var errorMessage = LogUtil.FatalAndReturnFormatted("{0}.{1}: Got request for unknown method {2}.",
                        nameof(ServiceHost), nameof(DispatchRequest), methodName);

                    var error = new Error()
                    {
                        message = errorMessage,
                        error_code = (int)ErrorCode.MethodNotFound
                    };
                    result = Message.FromError(error);
                }
            }

            if (result == null)
            {
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
            }

            return result;
        }
    }
}
