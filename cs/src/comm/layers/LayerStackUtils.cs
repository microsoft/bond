// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Layers
{

    public class LayerStackUtils
    {
        public static Error ProcessOnSend(ILayerStack layerStack, MessageType messageType, SendContext sendContext, out IBonded layerData)
        {
            Error error = null;
            layerData = null;

            if (layerStack != null)
            {
                error = layerStack.OnSend(messageType, sendContext, out layerData);
                if (error != null)
                {
                    Log.Warning("{0}.{1}: Layer error occurred sending message of type {2} (Code: {3} Message: {4}).",
                                    nameof(LayerStackUtils), nameof(ProcessOnSend), messageType, error.error_code, error.message);
                }
            }

            return error;
        }

        public static Error ProcessOnReceive(ILayerStack layerStack, MessageType messageType, ReceiveContext receiveContext, IBonded layerData)
        {
            Error error = null;

            if (layerStack != null)
            {
                if (layerData == null)
                {
                    Log.Warning("{0}.{1}: Layer stack present but no layer data received.", nameof(LayerStackUtils), nameof(ProcessOnReceive));
                }

                error = layerStack.OnReceive(messageType, receiveContext, layerData);

                if (error != null)
                {
                    Log.Warning("{0}.{1}: Layer error occurred receiving message of type {2} (Code: {3} Message: {4}).",
                                    nameof(LayerStackUtils), nameof(ProcessOnReceive), messageType, error.error_code, error.message);
                }
            }
            return error;
        }
    }
}
