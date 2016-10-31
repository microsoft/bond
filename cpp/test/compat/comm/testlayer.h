// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include "pingpong_types.h"


struct LayerStats
{
    LayerStats()
        : reached(0), error(0)
    {}

    int reached;
    int error;
};


// This is a template because currently the layer implementation does not allow
// two layers of the exact same type in the same layer stack
template <int i>
struct TestLayer
{
    TestLayer(LayerStats& stats)
        : _stats(stats)
    {}

    TestLayer(const TestLayer& other)
        : _stats(other._stats)
    {}

    void OnSend(bond::comm::MessageType /* message_type */,
        const std::string& /* service_name */,
        const std::string& /* method_name */,
        PingPongNS::PingLayerData& layer_data)
    {
        _stats.reached++;

        // Bit fiddling done to check layer ordering
        layer_data.data = (layer_data.data << 8) + i;
    }

    void OnReceive(bond::comm::MessageType /* message_type */,
        const std::string& /* service_name */,
        const std::string& /* method_name */,
        PingPongNS::PingLayerData& layer_data)
    {
        _stats.reached++;

        // Bit fiddling done to check layer ordering
        if ((layer_data.data & 0x0FF) != i)
        {
            _stats.error++;
        }

        layer_data.data = (layer_data.data >> 8);
    }

    LayerStats& _stats;
};

