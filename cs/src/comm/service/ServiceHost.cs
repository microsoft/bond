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
        private readonly object m_lock;
        private readonly Dictionary<string, ServiceMethodInfo> m_dispatchTable;

        public readonly Transport ParentTransport;

        public ServiceHost(Transport parentTransport)
        {
            ParentTransport = parentTransport;
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
                    methodNames.Add($"[{serviceMethod.MethodName}]");
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

        public async Task<IMessage> DispatchRequest(
            string methodName, ReceiveContext context, IMessage message, ConnectionMetrics connectionMetrics)
        {
            var totalTime = Stopwatch.StartNew();
            Stopwatch serviceTime = null;
            var requestMetrics = StartRequestMetrics(methodName, connectionMetrics);
            Log.Information("{0}.{1}: Got request [{2}] from {3}.",
                nameof(ServiceHost), nameof(DispatchRequest), methodName, context.Connection);

            ServiceMethodInfo methodInfo;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(methodName, out methodInfo))
                {
                    var errorMessage = "Got request for unknown method [" + methodName + "].";

                    Log.Error("{0}.{1}: {2}", nameof(ServiceHost), nameof(DispatchRequest), errorMessage);
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

                Log.Error("{0}.{1}: {2}", nameof(ServiceHost), nameof(DispatchRequest), errorMessage);
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
                serviceTime = Stopwatch.StartNew();
                // Cast to appropriate return type which we validated when registering the service 
                result = await (Task<IMessage>)methodInfo.Callback(message, context, CancellationToken.None);
                serviceTime.Stop();
            }
            catch (Exception ex)
            {
                Log.Error(ex, "{0}.{1}: Failed to complete method [{2}]. With exception: {3}",
                    nameof(ServiceHost), nameof(DispatchRequest), methodName, ex.Message);
                result = Message.FromError(Transport.MakeInternalServerError(ex));
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
            Log.Information("{0}.{1}: Got event [{2}] from {3}.",
                nameof(ServiceHost), nameof(DispatchEvent), methodName, context.Connection);
            ServiceMethodInfo methodInfo;

            lock (m_lock)
            {
                if (!m_dispatchTable.TryGetValue(methodName, out methodInfo))
                {
                    Log.Error("{0}.{1}: Got request for unknown method [{2}].",
                        nameof(ServiceHost), nameof(DispatchEvent), methodName);
                    return;
                }
            }

            if (methodInfo.CallbackType != ServiceCallbackType.Event)
            {
                Log.Error("{0}.{1}: Method [{2}] invoked as if it were {3}, but it was registered as {4}.",
                    nameof(ServiceHost), nameof(DispatchEvent), methodName, ServiceCallbackType.Event,
                    methodInfo.CallbackType);
                return;
            }

            try
            {
                serviceTime = Stopwatch.StartNew();
                await methodInfo.Callback(message, context, CancellationToken.None);
            }
            catch (Exception ex)
            {
                Log.Error(ex, "{0}.{1}: Failed to complete method [{2}]. With exception: {3}",
                    nameof(ServiceHost), nameof(DispatchEvent), methodName, ex.Message);
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
