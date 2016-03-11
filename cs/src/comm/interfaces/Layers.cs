// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm
{
    using System;
    using System.Collections.Generic;

    public interface ILayer<TLayerData> where TLayerData : class
    {
        void OnSend(Message<Bond.Void> message, SendContext context, ref TLayerData layerData);

        void OnReceive(Message<Bond.Void> message, ReceiveContext context, ref TLayerData layerData);
    }

    public interface ILayerStack
    {
        IBonded<Bond.Void> OnSend(Message<Bond.Void> message, SendContext context, object layerData);
        object OnReceive(Message<Bond.Void> message, ReceiveContext context, IBonded<Bond.Void> layerData);
    }

    public class LayerStack<TLayerData> : ILayerStack where TLayerData : class
    {
        private List<ILayer<TLayerData>> m_layers = new List<ILayer<TLayerData>>();
        private UnhandledExceptionHandler m_exceptionHandler;

        public LayerStack()
        {
            m_exceptionHandler = (ex) =>
            {
                Environment.FailFast("Unhandled exception", ex);
                return new Error { error_code = (int)ErrorCode.Impossible, message = "Unhandled exception" };
            };
        }

        // Order matters when adding a Layer. Messages being sent will
        // be passed through layer in the order they were added. When
        // messages are received, the opposite order will be used.
        public LayerStack<TLayerData> AddLayer(ILayer<TLayerData> layer)
        {
            m_layers.Add(layer);
            return this;
        }

        public IBonded<Bond.Void> OnSend(Message<Bond.Void> message, SendContext context, object layerData)
        {
            TLayerData realLayerData;

            if (layerData == null)
            {
                realLayerData = default(TLayerData);
            }
            else
            {
                realLayerData = layerData as TLayerData;
                if (realLayerData == null)
                {
                    throw new ArgumentException("layerData is not of the expected type");
                }
            }

            OnSendImpl(message, context, ref realLayerData);

            // TODO: will want to serialize here to catch any errors
            return ((IBonded<TLayerData>)new Bonded<TLayerData>(realLayerData)).Convert<Bond.Void>();
        }

        public object OnReceive(Message<Bond.Void> message, ReceiveContext context, IBonded<Bond.Void> layerData)
        {
            TLayerData realLayerData;

            if (layerData == null)
            {
                realLayerData = default(TLayerData);
            }
            else
            {
                try
                {
                    realLayerData = layerData.Deserialize<TLayerData>();
                }
                catch (Exception)
                {
                    // call into transport unhandled exception handler
                    // but, for interface exposition purpuses, just rethrow
                    throw;
                }
            }

            OnReceiveImpl(message, context, ref realLayerData);
            return realLayerData;
        }

        private void OnSendImpl(Message<Bond.Void> message, SendContext context, ref TLayerData layerData)
        {
            try
            {
                for (int layerIndex = 0; layerIndex < m_layers.Count; ++layerIndex)
                {
                    m_layers[layerIndex].OnSend(message, context, ref layerData);
                }
            }
            catch (Exception)
            {
                // call into transport unhandled exception handler
                // but, for interface exposition purpuses, just rethrow
                throw;
            }
        }

        private void OnReceiveImpl(Message<Bond.Void> message, ReceiveContext context, ref TLayerData layerData)
        {
            try
            {
                for (int layerIndex = m_layers.Count; layerIndex >= 0; --layerIndex)
                {
                    m_layers[layerIndex].OnReceive(message, context, ref layerData);
                }
            }
            catch (Exception ex)
            {
                // TODO: figure out a more specific set of exceptions to catch
                // TODO: figure out error handshake
                m_exceptionHandler(ex);
            }
        }
    }
}
