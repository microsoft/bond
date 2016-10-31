// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.


namespace PingPongNS
{
    using Bond.Comm;

    public class TestLayer : IStatelessLayer<PingLayerData>, ILayerProvider<PingLayerData>
    {
        public TestLayer(int i)
        {
            _i = i;
        }

        public ILayer<PingLayerData> GetLayer()
        {
            return this;
        }

        public Error OnSend(MessageType messageType, SendContext context, PingLayerData layerData, Logger logger)
        {
            NumReached++;

            // Bit fiddling done to check layer ordering
            layerData.data = (layerData.data << 8) + _i;
            return null;
        }

        public Error OnReceive(MessageType messageType, ReceiveContext context, PingLayerData layerData, Logger logger)
        {
            NumReached++;

            // Bit fiddling done to check layer ordering
            if ((layerData.data & 0x0FF) != _i)
            {
                NumError++;
            }
            layerData.data = (layerData.data >> 8);
            return null;
        }

        int _i;

        public int NumReached = 0;
        public int NumError = 0;
    }

}