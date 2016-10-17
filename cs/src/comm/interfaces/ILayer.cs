// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    /// <summary>
    /// Interface for layer implementations. Layers provide extensibility hooks
    /// in the comm framework. Layers have the the opportunity to act on each outgoing request
    /// on the client side and each incoming request on the service side (and vice versa for
    /// responses). Layers are aggregated into a stack of layers -- see <see cref="ILayerStack"/>.
    /// </summary>
    /// <typeparam name="TLayerData">The layer data type for this layer provider and the layers it provides.</typeparam>
    /// <remarks>
    /// Layers are assumed to cooperatively share the same instance of type
    /// <typeparamref name="TLayerData" />. TLayerData must be a Bond-serializable class.
    /// </remarks>
    public interface ILayer<TLayerData> where TLayerData : class, new()
    {
        /// <summary>
        /// Take action on sending of a message.
        /// </summary>
        /// <param name="messageType">The type of message</param>
        /// <param name="context">The send context</param>
        /// <param name="layerData">The layer data for this layer</param>
        /// <param name="logger">The logger</param>
        /// <returns>An error if something went wrong; null otherwise </returns>
        /// <remarks>
        /// Layers may not interact with the message payload itself so it is not provided.
        /// If this method throws, the exception will be caught and converted into an error.
        /// Errors returned from layers for request-response methods will replace the response.
        /// </remarks>
        Error OnSend(MessageType messageType, SendContext context, TLayerData layerData, Logger logger);

        /// <summary>
        /// Take action on receipt of a message.
        /// </summary>
        /// <param name="messageType">The type of message</param>
        /// <param name="context">The receive context</param>
        /// <param name="layerData">The layer data for this layer</param>
        /// <param name="logger">The logger</param>
        /// <returns>An error if something went wrong; null otherwise </returns>
        /// <remarks>
        /// Layers may not interact with the message payload itself so it is not provided.
        /// If this method throws, the exception will be caught and converted into an error.
        /// Errors returned from layers for request-response methods will replace the response.
        /// </remarks>
        Error OnReceive(MessageType messageType, ReceiveContext context, TLayerData layerData, Logger logger);
    }

    /// <summary>
    /// Tag interface to indicate that the layer is stateless and does not require a new instance
    /// to be made for each request. This provides potential optimization opportunities to the
    /// <see cref="ILayerStackProvider" />.
    /// </summary>
    /// <typeparam name="TLayerData">The layer data type for this layer provider and the layers it provides.</typeparam>
    public interface IStatelessLayer<TLayerData> : ILayer<TLayerData> where TLayerData : class, new()
    {
        // This space intentionally left blank.
    }

    /// <summary>
    /// Interface for factories of layer instances.
    /// </summary>
    /// <typeparam name="TLayerData">The layer data type for this layer provider and the layers it provides.</typeparam>
    public interface ILayerProvider<TLayerData> where TLayerData : class, new()
    {
        /// <summary>
        ///  Provide a layer instance.
        /// </summary>
        /// <returns>A layer instance.</returns>
        /// <remarks>Even for stateless layers, this may be invoked more than once. Returning null
        /// or throwing an exception is considered invalid and will prevent
        /// execution of all layers and cause an error for the message being processed.</remarks>
        ILayer<TLayerData> GetLayer();
    }
}
