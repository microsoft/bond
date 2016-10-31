// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <future>
#include <stdio.h>

#include <boost/thread/thread.hpp>

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

// Include auto-generated files
#include "pingpong_reflection.h"
#include "pingpong_comm.h"

#include "testlayer.h"

using namespace PingPongNS;

const int NumRequestsExpected = 10;
const int NumEventsExpected = 9;
const int NumErrorsExpected = 8;
static int NumRequests = 0;
static int NumEvents = 0;
static int NumErrors = 0;

// Implement service
struct PingPongImpl : PingPong
{
    void Ping(const bond::comm::payload<PingRequest>& request_payload,
              const std::function<void(const bond::comm::message<PingResponse>&)>& callback) override
    {
        PingRequest request = request_payload.value().Deserialize();

        switch (request.Action)
        {
        case PingAction::Identity:
        {
            printf("Received request \"%s\"\n", request.Payload.c_str());
            fflush(stdout);

            PingResponse response;
            response.Payload = request.Payload;
            NumRequests++;
            callback(std::move(response));
            break;
        }

        case PingAction::Error:
        {
            printf("Received error request \"%s\"\n", request.Payload.c_str());
            fflush(stdout);

            bond::comm::Error err;
            err.error_code = 1234;
            err.message = request.Payload;
            NumErrors++;
            callback(bond::comm::error(err));
            break;
        }

        default:
            printf("Received unknown request \"%s\"\n", request.Payload.c_str());
            fflush(stdout);

            throw new std::invalid_argument("Unknown PingAction");
        }
    }

    void PingEvent(const bond::comm::payload<PingPongNS::PingRequest> & theEvent) override
    {
        printf("Received request \"%s\"\n", theEvent.value().Deserialize().Payload.c_str());
        fflush(stdout);

        NumEvents++;
    }
};

int main()
{
    LayerStats layer_stats1, layer_stats2;
    TestLayer<1> layer1(layer_stats1);
    TestLayer<2> layer2(layer_stats2);

    bond::comm::LayerStack<
        PingLayerData,
        TestLayer<1>,
        TestLayer<2>
    > layers(layer1, layer2);

    bond::comm::SocketAddress loopback("127.0.0.1", 25188);
    bond::comm::epoxy::EpoxyTransport transport(layers);

    auto server = transport.Bind(loopback, boost::make_shared<PingPongImpl>());

    printf("Server ready\n");
    fflush(stdout);

    boost::this_thread::sleep_for(boost::chrono::seconds(3));

    if ((NumRequests != NumRequestsExpected) ||
        (NumEvents != NumEventsExpected) ||
        (NumErrors != NumErrorsExpected))
    {
        printf("Server failed: Did not receive all expected messages\n");
        fflush(stdout);
        exit(1);
    }

    if ((layer_stats1.reached == 0) ||
        (layer_stats1.error != 0) ||
        (layer_stats2.reached == 0) ||
        (layer_stats2.error != 0))
    {
        printf("Server failed: Problem with layers\n");
        fflush(stdout);
        exit(1);
    }

    printf("Server completed\n");
    fflush(stdout);
    return 0;
}
