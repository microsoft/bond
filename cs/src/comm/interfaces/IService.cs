// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System.Collections.Generic;
    using System.Threading;
    using System.Threading.Tasks;

    /// <summary>
    /// Represents a method that implements a Bond service method.
    /// </summary>
    /// <param name="request">The request message.</param>
    /// <param name="context">
    /// The context in which the message was received.
    /// </param>
    /// <param name="ct">
    /// The cancellation token for cooperative cancellation.
    /// </param>
    /// <returns>A task representing the response.</returns>
    /// <remarks>
    /// This class is primarily used by Bond's generated service
    /// implementations.
    /// </remarks>
    public delegate Task ServiceCallback(IMessage request, ReceiveContext context, CancellationToken ct);

    /// <summary>
    /// Indicates the type of ServiceCallback
    /// </summary>
    public enum ServiceCallbackType
    {
        /// <summary>
        /// Method returns a Task&lt;IMessage&gt;
        /// </summary>
        RequestResponse,

        /// <summary>
        /// Method returns a Task
        /// </summary>
        Event
    };

    /// <summary>
    /// Data about a Bond service method.
    /// </summary>
    public class ServiceMethodInfo
    {
        /// <summary>
        /// The fully qualified method name in the Bond namespace.
        /// </summary>
        public string MethodName;

        /// <summary>
        /// The delegate to invoke for this method.
        /// </summary>
        public ServiceCallback Callback;
        public ServiceCallbackType CallbackType;
    }

    /// <summary>
    /// Provides a mechanism for exposing Bond service methods.
    /// </summary>
    public interface IService
    {
        /// <summary>
        /// Gets a collection of the Bond service methods exposed by the
        /// implementor.
        /// </summary>
        IEnumerable<ServiceMethodInfo> Methods { get; }
    }
}
