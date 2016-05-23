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
        /// <remarks>
        /// Layers may not interact with the message payload itself so it is not provided.
        /// If this method throws, the exception will be caught by the exception handler of the layer stack
        /// if configured or of the transport.
        /// </remarks>
        Error OnSend(MessageType messageType, SendContext context, TLayerData layerData);

        /// <summary>
        /// Take action on receipt of a message.
        /// </summary>
        /// <param name="messageType">The type of message</param>
        /// <param name="context">The receive context</param>
        /// <param name="layerData">The layer data for this layer</param>
        /// <remarks>
        /// Layers may not interact with the message payload itself so it is not provided.
        /// If this method throws, the exception will be caught by the exception handler of the layer stack
        /// if configured or of the transport.
        /// </remarks>
        Error OnReceive(MessageType messageType, ReceiveContext context, TLayerData layerData);
    }
}
