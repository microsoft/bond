// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <future>
#include <stdio.h>

#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

// Include preferred transport
#include <bond/comm/transport/epoxy.h>

// Include auto-generated files
#include "pingpong_reflection.h"
#include "pingpong_comm.h"

#include "testlayer.h"

using namespace PingPongNS;

int main()
{
    boost::this_thread::sleep_for(boost::chrono::seconds(1));

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

    printf("Start client\n");
    fflush(stdout);

    try
    {
        PingPong::Proxy::Using<std::promise> proxy(transport.Connect(loopback));

        for (int i = 0; i < NumRequests; i++)
        {

            PingRequest request;
            request.Payload = "request" + boost::lexical_cast<std::string>(i);
            request.Action = PingAction::Identity;

            printf("Sending\n");
            fflush(stdout);

            bond::comm::message<PingResponse> response = proxy.Ping(std::move(request)).get();

            if (response.is_error())
            {
                const bond::comm::Error &error = response.err();
                printf("Error response received: %s\n", error.message.c_str());
                printf("Client failed\n");
                fflush(stdout);
                exit(1);
            }
            else if (response.value().Deserialize().Payload != request.Payload)
            {
                printf("Response payload did not match request\n");
                printf("Client failed\n");
                fflush(stdout);
                exit(1);
            }
        }

        for (int i = 0; i < NumEvents; i++)
        {
            PingRequest request;
            request.Payload = "event" + boost::lexical_cast<std::string>(i);

            proxy.PingEvent(std::move(request));
        }

        for (int i = 0; i < NumErrors; i++)
        {
            PingRequest request;
            request.Payload = "error" + boost::lexical_cast<std::string>(i);
            request.Action = PingAction::Error;

            bond::comm::message<PingResponse> response = proxy.Ping(std::move(request)).get();

            if (!response.is_error())
            {
                printf("Non-error response received: %s\n", response.value().Deserialize().Payload.c_str());
                printf("Client failed\n");
                fflush(stdout);
                exit(1);
            }
            else
            {
                const bond::comm::Error &error = response.err();
                if (error.message != request.Payload)
                {
                    printf("Error message did not match request payload\n");
                    printf("Client failed\n");
                    fflush(stdout);
                    exit(1);
                }
            }
        }
    }
    catch (std::exception& e)
    {
        printf("Caught exception: %s\n", e.what());
        fflush(stdout);
    }
    catch (...)
    {
        printf("Caught something\n");
        fflush(stdout);
    }

    if ((layer_stats1.reached == 0) ||
        (layer_stats1.error != 0) ||
        (layer_stats2.reached == 0) ||
        (layer_stats2.error != 0))
    {
        printf("Client failed: Problem with layers\n");
        fflush(stdout);
        exit(1);
    }

    printf("Client succeeded\n");
    fflush(stdout);
    return 0;
}
