// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Service
{
    using System;
    using System.Collections.Generic;
    using System.Reflection;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    public class ServiceHost
    {
        private readonly Transport m_parentTransport;
        private readonly object m_lock;
        private readonly Dictionary<string, ServiceMethodInfo> m_dispatchTable;


        public ServiceHost(Transport parentTransport)
        {
            m_parentTransport = parentTransport;
            m_lock = new object();
            m_dispatchTable = new Dictionary<string, ServiceMethodInfo>();
        }

        public bool IsRegistered(string serviceMethodName)
        {
            lock (m_lock)
            {
                return m_dispatchTable.ContainsKey(serviceMethodName);
            }
        }

        public void ValidateServiceMethods(IEnumerable<ServiceMethodInfo> serviceMethods)
        {
            Type returnParameter;

            foreach (var serviceMethod in serviceMethods)
            {
                switch (serviceMethod.CallbackType)
                {
                    case ServiceCallbackType.RequestResponse:
                        returnParameter = serviceMethod.Callback.GetMethodInfo().ReturnType;
                        if (returnParameter != typeof(Task<IMessage>))
                        {
                            throw new ArgumentException($"{serviceMethod.MethodName} registered as " +
                                                        $"{serviceMethod.CallbackType} but callback not implemented as such.");
                        }
                        break;
                    case ServiceCallbackType.Event:
                        returnParameter = serviceMethod.Callback.GetMethodInfo().ReturnType;
                        if (returnParameter != typeof(Task))
                        {
                            throw new ArgumentException($"{serviceMethod.MethodName} registered as " +
                                                        $"{serviceMethod.CallbackType} but callback not implemented as such.");
                        }
                        break;
                    default:
                        throw new ArgumentException($"{serviceMethod.MethodName} registered as invalid type " +
                                                    $"{serviceMethod.CallbackType}.");
                }
            }
        }

        public void Register(IService service)
        {
            var methodNames = new SortedSet<string>();

            ValidateServiceMethods(service.Methods);

            lock (m_lock)
            {
                // Service methods are registerd as a unit - either register all or none.
                // This code could have been greedy to do both check and register in a single loop,
                // plus a nested loop to clean up / rollback in case of a method already registered.
                // Now services registration is expected to be infrequent and very light weight event, 
                // so we decided in favor of looping twice over the same collection to avoid nested loop 
                // for clean up.
                foreach (var serviceMethod in service.Methods)
                {
                    if (m_dispatchTable.ContainsKey(serviceMethod.MethodName))
                    {
                        throw new ArgumentException($"{serviceMethod.MethodName} already registered");
                    }
                }

                foreach (var serviceMethod in service.Methods)
                {
                    m_dispatchTable.Add(serviceMethod.MethodName, serviceMethod);
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
            ServiceMethodInfo methodInfo;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(methodName, out methodInfo))
                {
                    var errorMessage = LogUtil.FatalAndReturnFormatted("{0}.{1}: Got request for unknown method {2}.",
                        nameof(ServiceHost), nameof(DispatchRequest), methodName);

                    var error = new Error
                    {
                        message = errorMessage,
                        error_code = (int)ErrorCode.MethodNotFound
                    };
                    return Message.FromError(error);
                }
            }

            if (methodInfo.CallbackType != ServiceCallbackType.RequestResponse)
            {
                var errorMessage = LogUtil.FatalAndReturnFormatted("{0}.{1}: Method {2} invoked as if it were {3}, but it was registered as {4}.",
                    nameof(ServiceHost), nameof(DispatchRequest), methodName, ServiceCallbackType.RequestResponse, methodInfo.CallbackType);

                var error = new Error
                {
                    message = errorMessage,
                    error_code = (int)ErrorCode.InvalidInvocation
                };
                return Message.FromError(error);
            }

            IMessage result = null;

            try
            {
                // Cast to appropriate return type which we validated when registering the service 
                result = await (Task<IMessage>)methodInfo.Callback(message, context, CancellationToken.None);
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

        public async Task DispatchEvent(string methodName, ReceiveContext context, IMessage message)
        {
            Log.Information("{0}.{1}: Got event {2} from {3}.",
                nameof(ServiceHost), nameof(DispatchEvent), methodName, context.Connection);
            ServiceMethodInfo methodInfo;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(methodName, out methodInfo))
                {
                    LogUtil.FatalAndReturnFormatted("{0}.{1}: Got request for unknown method {2}.",
                        nameof(ServiceHost), nameof(DispatchRequest), methodName);
                    return;
                }
            }

            if (methodInfo.CallbackType != ServiceCallbackType.Event)
            {
                LogUtil.FatalAndReturnFormatted("{0}.{1}: Method {2} invoked as if it were {3}, but it was registered as {4}.",
                    nameof(ServiceHost), nameof(DispatchRequest), methodName, ServiceCallbackType.Event, methodInfo.CallbackType);
                return;
            }

            try
            {
                await methodInfo.Callback(message, context, CancellationToken.None);
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

               LogUtil.FatalAndReturnFormatted("{0}.{1}: Failed to complete method {2}. With exception: {3}",
                   nameof(ServiceHost), nameof(DispatchRequest), methodName, callbackEx.ToString());
            }
        }
    }
}
