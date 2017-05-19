// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <future>
#include <stdio.h>

#include <bond/ext/detail/countdown_event.h>

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

// Include auto-generated files
#include "pingpong_reflection.h"
#include "pingpong_comm.h"

#include "testlayer.h"

using namespace PingPongNS;
using namespace bond::ext::detail;

static countdown_event Countdown(NumRequests + NumEvents + NumErrors);
static std::atomic<uint32_t> NumRequestsReceived(0);
static std::atomic<uint32_t> NumEventsReceived(0);
static std::atomic<uint32_t> NumErrorsReceived(0);

// Implement service
struct PingPongImpl : PingPong
{
public:
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
                NumRequestsReceived++;
                Countdown.set();
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
                NumErrorsReceived++;
                Countdown.set();
                callback(bond::comm::error(err));
                break;
            }

            default:
            {
                printf("Received unknown request \"%s\"\n", request.Payload.c_str());
                fflush(stdout);
                Countdown.set();
                throw new std::invalid_argument("Unknown PingAction");
            }
        }
    }

    void PingEvent(const bond::comm::payload<PingPongNS::PingRequest> & theEvent) override
    {
        printf("Received request \"%s\"\n", theEvent.value().Deserialize().Payload.c_str());
        fflush(stdout);

        NumEventsReceived++;
        Countdown.set();
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

    bond::comm::SocketAddress loopback("127.0.0.1", Port);
    bond::comm::epoxy::EpoxyTransport transport(layers);

    auto server = transport.Bind(loopback, boost::make_shared<PingPongImpl>());

    printf("Server ready\n");
    fflush(stdout);

    bool countdownSet = Countdown.wait(std::chrono::seconds(30));

    if (!countdownSet ||
        (NumRequestsReceived != NumRequests) ||
        (NumEventsReceived != NumEvents) ||
        (NumErrorsReceived != NumErrors))
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
