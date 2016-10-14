// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

namespace Bond.Comm.Layers
{

    /// <summary>
    /// Provides common methods related to layer stack processing useful for Transport implementations.
    /// </summary>
    public class LayerStackUtils
    {
        public static Error ProcessOnSend(
            ILayerStack layerStack, MessageType messageType, SendContext sendContext, out IBonded layerData, Logger logger)
        {
            Error error = null;
            layerData = null;

            if (layerStack != null)
            {
                error = layerStack.OnSend(messageType, sendContext, out layerData, logger);
                if (error != null)
                {
                    logger.Site().Warning("Layer error occurred sending message of type {0} (Code: {1} Message: {2}).",
                        messageType, error.error_code, error.message);
                }
            }

            return error;
        }

        public static Error ProcessOnReceive(
            ILayerStack layerStack, MessageType messageType, ReceiveContext receiveContext, IBonded layerData, Logger logger)
        {
            Error error = null;

            if (layerStack != null)
            {
                if (layerData == null)
                {
                    logger.Site().Warning("Layer stack present but no layer data received.");
                }

                error = layerStack.OnReceive(messageType, receiveContext, layerData, logger);

                if (error != null)
                {
                    logger.Site().Warning("Layer error occurred receiving message of type {0} (Code: {1} Message: {2}).",
                        messageType, error.error_code, error.message);
                }
            }
            return error;
        }
    }
}
