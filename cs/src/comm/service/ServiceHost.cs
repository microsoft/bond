// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Service
{
    using System;
    using System.Collections.Generic;
    using System.Reflection;
    using System.Diagnostics;
    using System.Threading;
    using System.Threading.Tasks;
    using Bond.Comm;

    public class ServiceHost
    {
        private readonly object dispatchTableLock;
        private readonly Dictionary<string, ServiceMethodInfo> dispatchTable;
        private readonly Logger logger;

        public ServiceHost(Logger logger)
        {
            dispatchTableLock = new object();
            dispatchTable = new Dictionary<string, ServiceMethodInfo>();
            this.logger = logger;
        }

        private static void Update(RequestMetrics requestMetrics, string serviceName, string methodName, Stopwatch serviceTime)
        {
            requestMetrics.service_name = serviceName ?? string.Empty;
            requestMetrics.method_name = methodName ?? string.Empty;
            requestMetrics.service_method_time_millis = serviceTime?.Elapsed.TotalMilliseconds ?? 0;
        }

        public bool IsRegistered(string serviceMethodName)
        {
            lock (dispatchTableLock)
            {
                return dispatchTable.ContainsKey(serviceMethodName);
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

        public void Register<T>(T service) where T : IService
        {
            var methodNames = new SortedSet<string>();

            ValidateServiceMethods(service.Methods);

            lock (dispatchTableLock)
            {
                // Service methods are registerd as a unit - either register all or none.
                // This code could have been greedy to do both check and register in a single loop,
                // plus a nested loop to clean up / rollback in case of a method already registered.
                // Now services registration is expected to be infrequent and very light weight event, 
                // so we decided in favor of looping twice over the same collection to avoid nested loop 
                // for clean up.
                foreach (var serviceMethod in service.Methods)
                {
                    if (dispatchTable.ContainsKey(serviceMethod.MethodName))
                    {
                        throw new ArgumentException($"{serviceMethod.MethodName} already registered");
                    }
                }

                foreach (var serviceMethod in service.Methods)
                {
                    dispatchTable.Add(serviceMethod.MethodName, serviceMethod);
                    methodNames.Add($"[{serviceMethod.MethodName}]");
                }
            }

            logger.Site().Information("Registered {0} with methods: {1}", typeof(T).Name, string.Join(", ", methodNames));
        }

        public void Deregister<T>(T service) where T : IService
        {
            lock (dispatchTableLock)
            {
                foreach (var serviceMethod in service.Methods)
                {
                    dispatchTable.Remove(serviceMethod.MethodName);
                }
            }
            logger.Site().Information("Deregistered {0}.", typeof(T).Name);
        }

        public async Task<IMessage> DispatchRequest(string serviceName, string methodName, ReceiveContext context, IMessage message)
        {
            string qualifiedName = serviceName + "." + methodName;
            Stopwatch serviceTime = null;
            logger.Site().Information("Got request [{0}] from {1}.", qualifiedName, context.Connection);

            try
            {
                ServiceMethodInfo methodInfo;
                lock (dispatchTableLock)
                {
                    if (!dispatchTable.TryGetValue(qualifiedName, out methodInfo))
                    {
                        var errorMessage = "Got request for unknown method [" + qualifiedName + "].";

                        logger.Site().Error(errorMessage);
                        var error = new Error
                        {
                            message = errorMessage,
                            error_code = (int) ErrorCode.METHOD_NOT_FOUND
                        };
                        return Message.FromError(error);
                    }
                }

                if (methodInfo.CallbackType != ServiceCallbackType.RequestResponse)
                {
                    var errorMessage = "Method [" + qualifiedName + "] invoked as if it were " +
                                       ServiceCallbackType.RequestResponse + ", but it was registered as " +
                                       methodInfo.CallbackType + ".";

                    logger.Site().Error(errorMessage);
                    var error = new Error
                    {
                        message = errorMessage,
                        error_code = (int) ErrorCode.INVALID_INVOCATION
                    };
                    return Message.FromError(error);
                }

                IMessage result;

                try
                {
                    serviceTime = Stopwatch.StartNew();
                    // Cast to appropriate return type which we validated when registering the service 
                    result = await (Task<IMessage>) methodInfo.Callback(message, context, CancellationToken.None);
                    serviceTime.Stop();
                }
                catch (Exception ex)
                {
                    logger.Site()
                        .Error(ex, "Failed to complete method [{0}]. With exception: {1}", qualifiedName, ex.Message);
                    result = Message.FromError(
                        Errors.MakeInternalServerError(ex, context.RequestMetrics.request_id, includeDetails: false));
                }
                return result;
            }
            finally
            {
                Update(context.RequestMetrics, serviceName, methodName, serviceTime);
            }
        }

        public async Task DispatchEvent(string serviceName, string methodName, ReceiveContext context, IMessage message)
        {
            string qualifiedName = serviceName + "." + methodName;
            Stopwatch serviceTime = null;
            logger.Site().Information("Got event [{0}] from {1}.", qualifiedName, context.Connection);

            try
            {
                ServiceMethodInfo methodInfo;
                lock (dispatchTableLock)
                {
                    if (!dispatchTable.TryGetValue(qualifiedName, out methodInfo))
                    {
                        logger.Site().Error("Got request for unknown method [{0}].", qualifiedName);
                        return;
                    }
                }

                if (methodInfo.CallbackType != ServiceCallbackType.Event)
                {
                    logger.Site().Error("Method [{0}] invoked as if it were {1}, but it was registered as {2}.",
                        qualifiedName, ServiceCallbackType.Event, methodInfo.CallbackType);
                    return;
                }

                try
                {
                    serviceTime = Stopwatch.StartNew();
                    await methodInfo.Callback(message, context, CancellationToken.None);
                }
                catch (Exception ex)
                {
                    logger.Site()
                        .Error(ex, "Failed to complete method [{0}]. With exception: {1}", qualifiedName, ex.Message);
                }
            }
            finally
            {
                Update(context.RequestMetrics, serviceName, methodName, serviceTime);
            }
        }
    }
}
