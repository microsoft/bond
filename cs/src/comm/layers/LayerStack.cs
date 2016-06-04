// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Layers
{
    using System;
    using System.Collections.Generic;
    using System.Diagnostics;

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

        /// <summary>
        /// Construct an empty layer stack instance.
        /// </summary>
        /// <param name="layers">Layers for this stack. Must be at least one and all must not be null.</param>
        public LayerStack(params ILayer<TLayerData>[] layers)
        {
            if ((layers == null) || (layers.Length == 0)) { throw new ArgumentException("At least one layer must be provided"); }

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
                    error = new Error()
                    {
                        error_code = (int)ErrorCode.UnhandledLayerError,
                    };
                    Log.Site().Error(ex, "While handling layer {0}: {1}", layerIndex, ex.Message);
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
                    error = new Error
                    {
                        error_code = (int)ErrorCode.UnhandledLayerError
                    };

                    Log.Site().Error(ex, "While handling layer {0}: {1}", layerIndex, ex.Message);
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
                    error = new Error
                    {
                        error_code = (int)ErrorCode.UnhandledLayerError
                    };

                    Log.Site().Error(ex, "While unmarshaling layer data: {0}", ex.Message);

                    realLayerData = new TLayerData();
                }
            }

            return error;
        }
    }
}
