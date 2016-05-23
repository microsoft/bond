// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;

    /// <summary>
    /// Interface for layer stacks. The layer stack encapsulates
    /// a list of layers which work together over the same layer data.
    /// </summary>
    public interface ILayerStack
    {
        /// <summary>
        /// Take action on sending of a message, by invoking a stack of layers (in forward order).
        /// </summary>
        /// <param name="messageType">The type of message</param>
        /// <param name="context">The send context. May not be null.</param>
        /// <param name="layerData">The layer data for this layer, provided as an instance in an <see cref="IBonded"/> ready to be serialized</param>
        /// <returns>An error, or null if there is no error.</returns>
        /// <remarks>
        /// In general, the layer is not expected to interact with the message payload itself.
        /// </remarks>
        Error OnSend(MessageType messageType, SendContext context, out IBonded layerData);

        /// <summary>
        /// Take action on receipt of a message, by invoking a stack of layers (in reverse order).
        /// </summary>
        /// <param name="messageType">The type of message</param>
        /// <param name="context">The receive context. May not be null.</param>
        /// <param name="layerData">The layer data for this layer, provided as an <see cref="IBonded"/> to be deserialized</param>
        /// <returns>An error, or null if there is no error.</returns>
        /// <remarks>
        /// In general, the layer is not expected to interact with the message payload itself.
        /// </remarks>
        Error OnReceive(MessageType messageType, ReceiveContext context, IBonded layerData);
    }


    /// <summary>
    /// Concrete layer stack implementation. The layer stack encapsulates
    /// a list of layers which work together over the same layer data.
    /// </summary>
    /// <typeparam name="TLayerData">Type shared by the layers of this stack. Must be a Bond-serailizable type.</typeparam>
    /// <remarks>
    /// Layers are assumed to cooperatively share the same instance of type
    /// TLayerData. TLayerData must be a Bond-serializable class.
    /// </remarks>
    public class LayerStack<TLayerData> : ILayerStack where TLayerData : class, new()
    {
        readonly List<ILayer<TLayerData>> layers;
        readonly ExceptionHandler exceptionHandler;

        /// <summary>
        /// Construct an empty layer stack instance.
        /// </summary>
        /// <param name="exceptionHandler">Exception handler for this instance. May be null.</param>
        /// <param name="layers">Layers for this stack. Must be at least one and all must not be null.</param>
        public LayerStack(ExceptionHandler exceptionHandler, params ILayer<TLayerData>[] layers)
        {
            if ((layers == null) || (layers.Length == 0)) { throw new ArgumentException("At least one layer must be provided"); }

            this.exceptionHandler = exceptionHandler;
            this.layers = new List<ILayer<TLayerData>>(layers.Length);
            for (int i = 0; i < layers.Length; i++)
            {
                if (layers[i] == null)
                {
                    throw new ArgumentNullException($"Layer {i} was null");
                }

                this.layers.Add(layers[i]);
            }
        }

        public Error OnSend(MessageType messageType, SendContext context, out IBonded layerData)
        {
            if (context == null) { throw new ArgumentNullException(nameof(context)); }

            var realLayerData = new TLayerData();
            var error = OnSendImpl(messageType, context, realLayerData);
            layerData = (error == null
                            ? (IBonded<TLayerData>)new Bonded<TLayerData>(realLayerData)
                            : null);

            // We don't serialize here because the Transport owns the serialization format
            return error;
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, IBonded layerData)
        {
            if (context == null) { throw new ArgumentNullException(nameof(context)); }

            TLayerData realLayerData;
            Error error = DeserializeLayerData(layerData, out realLayerData);

            if (error == null)
            {
                Debug.Assert(realLayerData != null);
                error = OnReceiveImpl(messageType, context, realLayerData);
            }

            return error;
        }

        private Error OnSendImpl(MessageType messageType, SendContext context, TLayerData layerData)
        {
            Error error = null;

            // Walk the layers in forward order
            for (int layerIndex = 0; (error == null) && (layerIndex < this.layers.Count); ++layerIndex)
            {
                try
                {
                    error = layers[layerIndex].OnSend(messageType, context, layerData);
                }
                catch (Exception ex)
                {
                    if (exceptionHandler != null)
                    {
                        error = exceptionHandler(ex);
                    }
                    else
                    {
                        error = new Error()
                        {
                            error_code = (int) ErrorCode.UnhandledLayerError,
                        };
                        Log.Error(ex, "{0}.{1}: While handling layer {2}: {3}",
                                    nameof(LayerStack<TLayerData>), nameof(OnSendImpl), layerIndex, ex.Message);
                    }
                }
            }

            return error;
        }

        private Error OnReceiveImpl(MessageType messageType, ReceiveContext context, TLayerData layerData)
        {
            Error error = null;

            // Walk the layers in reverse order
            for (int layerIndex = layers.Count - 1; (error == null) && (layerIndex >= 0); --layerIndex)
            {
                try
                {
                    error = layers[layerIndex].OnReceive(messageType, context, layerData);
                }
                catch (Exception ex)
                {
                    if (exceptionHandler != null)
                    {
                        error = exceptionHandler(ex);
                    }
                    else
                    {
                        error = new Error
                        {
                            error_code = (int)ErrorCode.UnhandledLayerError
                        };

                        Log.Error(ex, "{0}.{1}: While handling layer {2}: {3}",
                                    nameof(LayerStack<TLayerData>), nameof(OnReceiveImpl), layerIndex, ex.Message);
                    }
                }
            }

            return error;
        }

        private Error DeserializeLayerData(IBonded layerData, out TLayerData realLayerData)
        {
            Error error = null;
            if (layerData == null)
            {
                realLayerData = new TLayerData();
            }
            else
            {
                try
                {
                    realLayerData = layerData.Deserialize<TLayerData>();
                }
                catch (Exception ex)
                {
                    if (exceptionHandler != null)
                    {
                        error = exceptionHandler(ex);
                    }
                    else
                    {
                        error = new Error
                        {
                            error_code = (int)ErrorCode.UnhandledLayerError
                        };

                        Log.Error(ex, "{0}.{1}: While unmarshaling layer data: {2}",
                                    nameof(LayerStack<TLayerData>), nameof(DeserializeLayerData), ex.Message);
                    }

                    realLayerData = new TLayerData();
                }
            }

            return error;
        }
    }
}
