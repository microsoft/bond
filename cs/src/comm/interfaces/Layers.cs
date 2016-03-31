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
        IBonded OnSend(Message<Bond.Void> message, SendContext context, object layerData);
        object OnReceive(Message<Bond.Void> message, ReceiveContext context, IBonded layerData);
    }

    public class LayerStack<TLayerData> : ILayerStack where TLayerData : class
    {
        private List<ILayer<TLayerData>> m_layers = new List<ILayer<TLayerData>>();

        // Order matters when adding a Layer. Messages being sent will
        // be passed through layer in the order they were added. When
        // messages are received, the opposite order will be used.
        public LayerStack<TLayerData> AddLayer(ILayer<TLayerData> layer)
        {
            m_layers.Add(layer);
            return this;
        }

        public IBonded OnSend(Message<Bond.Void> message, SendContext context, object layerData)
        {
            // apply the interfaces in the SAME order they were added on the sending side
            // catch any exceptions that leak out and use the unhandled exception handler
            throw new NotImplementedException();
        }

        public object OnReceive(Message<Bond.Void> message, ReceiveContext context, IBonded layerData)
        {
            // apply the interfaces in the REVERSE order they were added on the sending side
            // catch any exceptions that leak out and use the unhandled exception handler
            throw new NotImplementedException();
        }
    }
}
