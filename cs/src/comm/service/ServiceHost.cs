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

        public readonly Transport ParentTransport;

        public ServiceHost(Transport parentTransport)
        {
            ParentTransport = parentTransport;
            dispatchTableLock = new object();
            dispatchTable = new Dictionary<string, ServiceMethodInfo>();
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

            Log.Site().Information("Registered {0} with methods: {1}", typeof(T).Name, string.Join(", ", methodNames));
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
            Log.Site().Information("Deregistered {0}.", typeof(T).Name);
        }

        public async Task<IMessage> DispatchRequest(
            string methodName, ReceiveContext context, IMessage message, ConnectionMetrics connectionMetrics)
        {
            var totalTime = Stopwatch.StartNew();
            Stopwatch serviceTime = null;
            var requestMetrics = StartRequestMetrics(methodName, connectionMetrics);
            Log.Site().Information("Got request [{0}] from {1}.", methodName, context.Connection);

            ServiceMethodInfo methodInfo;

            lock (dispatchTableLock)
            {
                if (!dispatchTable.TryGetValue(methodName, out methodInfo))
                {
                    var errorMessage = "Got request for unknown method [" + methodName + "].";

                    Log.Site().Error(errorMessage);
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
                var errorMessage = "Method [" + methodName + "] invoked as if it were " +
                                   ServiceCallbackType.RequestResponse + ", but it was registered as " +
                                   methodInfo.CallbackType + ".";

                Log.Site().Error(errorMessage);
                var error = new Error
                {
                    message = errorMessage,
                    error_code = (int)ErrorCode.InvalidInvocation
                };
                return Message.FromError(error);
            }

            IMessage result;

            try
            {
                serviceTime = Stopwatch.StartNew();
                // Cast to appropriate return type which we validated when registering the service 
                result = await (Task<IMessage>)methodInfo.Callback(message, context, CancellationToken.None);
                serviceTime.Stop();
            }
            catch (Exception ex)
            {
                Log.Site().Error(ex, "Failed to complete method [{0}]. With exception: {1}", methodName, ex.Message);
                result = Message.FromError(Errors.MakeInternalServerError(ex, includeDetails: false));
            }

            FinishRequestMetrics(requestMetrics, totalTime, serviceTime);
            Metrics.Emit(requestMetrics);

            return result;
        }

        public async Task DispatchEvent(
            string methodName, ReceiveContext context, IMessage message, ConnectionMetrics connectionMetrics)
        {
            var totalTime = Stopwatch.StartNew();
            Stopwatch serviceTime = null;
            var requestMetrics = StartRequestMetrics(methodName, connectionMetrics);
            Log.Site().Information("Got event [{0}] from {1}.", methodName, context.Connection);
            ServiceMethodInfo methodInfo;

            lock (dispatchTableLock)
            {
                if (!dispatchTable.TryGetValue(methodName, out methodInfo))
                {
                    Log.Site().Error("Got request for unknown method [{0}].", methodName);
                    return;
                }
            }

            if (methodInfo.CallbackType != ServiceCallbackType.Event)
            {
                Log.Site().Error("Method [{0}] invoked as if it were {1}, but it was registered as {2}.",
                    methodName, ServiceCallbackType.Event, methodInfo.CallbackType);
                return;
            }

            try
            {
                serviceTime = Stopwatch.StartNew();
                await methodInfo.Callback(message, context, CancellationToken.None);
            }
            catch (Exception ex)
            {
                Log.Site().Error(ex, "Failed to complete method [{0}]. With exception: {1}", methodName, ex.Message);
            }

            FinishRequestMetrics(requestMetrics, totalTime, serviceTime);
            Metrics.Emit(requestMetrics);
        }

        private static RequestMetrics StartRequestMetrics(string methodName, ConnectionMetrics connectionMetrics)
        {
            var requestMetrics = new RequestMetrics
            {
                request_id = Guid.NewGuid().ToString(),
                connection_id = connectionMetrics.connection_id,
                local_endpoint = connectionMetrics.local_endpoint,
                remote_endpoint = connectionMetrics.remote_endpoint,
                method_name = methodName,
                error = null
            };
            return requestMetrics;
        }

        private static void FinishRequestMetrics(RequestMetrics requestMetrics, Stopwatch totalTime, Stopwatch serviceTime)
        {
            requestMetrics.total_time_millis = (float)totalTime.Elapsed.TotalMilliseconds;
            requestMetrics.service_method_time_millis = (float)(serviceTime?.Elapsed.TotalMilliseconds ?? 0);
        }
    }
}
