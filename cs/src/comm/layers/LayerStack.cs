// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Layers
{
    using System;
    using System.Diagnostics;
    using System.Linq;

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
        readonly ILayer<TLayerData>[] layers;
        readonly Logger logger;

        /// <summary>
        /// Construct a layer stack instance from a set of layers.
        /// </summary>
        /// <param name="logger"></param>
        /// <param name="layers">Layers for this stack. Must be at least one and all must not be null.</param>
        public LayerStack(Logger logger, params ILayer<TLayerData>[] layers)
        {
            if ((layers == null) || (layers.Length == 0)) { throw new ArgumentException("At least one layer must be provided"); }

            for (int i = 0; i < layers.Length; i++)
            {
                if (layers[i] == null)
                {
                    throw new ArgumentNullException($"Layer {i} was null");
                }
            }

            this.layers = layers;
            this.logger = logger;
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
            for (int layerIndex = 0; (error == null) && (layerIndex < this.layers.Length); ++layerIndex)
            {
                try
                {
                    error = layers[layerIndex].OnSend(messageType, context, layerData);
                }
                catch (Exception ex)
                {
                    logger.Site().Error(ex, "While handling layer {0}", layerIndex);
                    error = Errors.MakeInternalServerError(ex, includeDetails: true);
                }
            }

            return error;
        }

        private Error OnReceiveImpl(MessageType messageType, ReceiveContext context, TLayerData layerData)
        {
            Error error = null;

            // Walk the layers in reverse order
            for (int layerIndex = layers.Length - 1; (error == null) && (layerIndex >= 0); --layerIndex)
            {
                try
                {
                    error = layers[layerIndex].OnReceive(messageType, context, layerData);
                }
                catch (Exception ex)
                {
                    logger.Site().Error(ex, "While handling layer {0}", layerIndex);
                    error = Errors.MakeInternalServerError(ex, includeDetails: true);
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
                    logger.Site().Error(ex, "Unmarshaling layer data threw exception");
                    error = Errors.MakeInternalServerError(ex, includeDetails: true);
                    realLayerData = new TLayerData();
                }
            }

            return error;
        }
    }

    /// <summary>
    /// Concrete layer stack provider implementation. The layer stack encapsulates
    /// a list of layers providers which will produce layer instances. Will cache a single
    /// layer stack instance if all the layer providers provide themselves when their <c>GetLayer</c>
    /// method is invoked; otherwise, a new layer stack instance will be produced each time.
    /// </summary>
    /// <typeparam name="TLayerData">Type shared by the layers produced by this stack provider. Must be a Bond-serailizable type.</typeparam>
    public class LayerStackProvider<TLayerData> : ILayerStackProvider where TLayerData : class, new()
    {
        readonly ILayerProvider<TLayerData>[] layerProviders;
        readonly LayerStack<TLayerData> cachedLayerStack;
        readonly Logger logger;

        /// <summary>
        /// Construct a layer stack provider from a set of layer providers.
        /// </summary>
        /// <param name="logger"></param>
        /// <param name="layerProviders">Layer providers for this stack provider. Must be at least one and all must not be null.</param>
        public LayerStackProvider(Logger logger, params ILayerProvider<TLayerData>[] layerProviders)
        {
            if ((layerProviders == null) || (layerProviders.Length == 0))
            {
                throw new ArgumentException("At least one layer provider must be provided", nameof(layerProviders));
            }

            for (int i = 0; i < layerProviders.Length; i++)
            {
                if (layerProviders[i] == null)
                {
                    throw new ArgumentNullException($"Layer provider {i} was null");
                }
            }

            this.layerProviders = layerProviders;

            ILayer<TLayerData>[] layers;
            Error error = GetNewLayers(out layers);

            if (error == null)
            {
                bool stateless = layers.All(layer => layer is IStatelessLayer<TLayerData>);

                if (stateless)
                {
                    cachedLayerStack = new LayerStack<TLayerData>(logger, layers);
                }
            }
            else
            {
                throw new InvalidOperationException(error.message);
            }

            this.logger = logger;
        }

        public Error GetLayerStack(out ILayerStack stack)
        {
            Error error = null;

            if (cachedLayerStack != null)
            {
                stack = cachedLayerStack;
            }
            else
            {
                ILayer<TLayerData>[] layers;
                error = GetNewLayers(out layers);
                if (error == null)
                {
                    stack = new LayerStack<TLayerData>(logger, layers);
                }
                else
                {
                    stack = null;
                }
            }

            return error;
        }

        Error GetNewLayers(out ILayer<TLayerData>[] layers)
        {
            layers = new ILayer<TLayerData>[layerProviders.Length];
            Error result = null;
            for (int i = 0; i < layerProviders.Length; i++)
            {
                try
                {
                    layers[i] = layerProviders[i].GetLayer();
                    if (layers[i] == null)
                    {
                        result = Errors.MakeInternalServerError($"Layer provider {i} produced null layer");
                        logger.Site().Error(result.message);
                        layers = null;
                        break;
                    }
                }
                catch (Exception ex)
                {
                    logger.Site().Error(ex, "Layer provider {0} threw exception", i);
                    result = Errors.MakeInternalServerError(ex, includeDetails: true);
                    layers = null;
                    break;
                }
            }

            return result;
        }
    }
}
