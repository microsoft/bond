// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// Include auto-generated files
#include "pingpong_apply.h"
#include "pingpong_grpc.h"
#include "pingpong_reflection.h"
#include "pingpong_types.h"

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/wait_callback.h>

#include <chrono>
#include <memory>
#include <stdio.h>
#include <string>
#include <thread>

using grpc::Channel;
using grpc::Status;

using namespace PingPongNS;

int main()
{
    auto ioManager = std::make_shared<bond::ext::gRPC::io_manager>();

    const std::string server_address("127.0.0.1:" + std::to_string(Port));
    std::shared_ptr<Channel> channel = grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials());

    std::this_thread::sleep_for(std::chrono::seconds(1));

    PingPong::Client client(channel, ioManager);

    printf("Start client\n");
    fflush(stdout);

    try
    {
        for (int i = 0; i < NumRequests; i++)
        {
            PingRequest request;

            request.Payload = "request" + std::to_string(i);
            request.Action = PingAction::Identity;

            printf("Sending request\n");
            fflush(stdout);

            bond::ext::gRPC::wait_callback<PingResponse> cb;
            client.AsyncPing(request, cb);
            bool gotResponse = cb.wait_for(std::chrono::seconds(1));

            if (!gotResponse)
            {
                printf("Client timed out waiting for response\n");
                fflush(stdout);
                exit(1);
            }

            Status status = cb.status();

            if (!status.ok())
            {
                printf("Error response received: %d\n", status.error_code());
                printf("Client failed\n");
                fflush(stdout);
                exit(1);
            }

            if (cb.response().Deserialize().Payload != request.Payload)
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

            request.Payload = "event" + std::to_string(i);

            printf("Sending event\n");
            fflush(stdout);

            client.AsyncPingEvent(request);
        }

        for (int i = 0; i < NumErrors; i++)
        {
            PingRequest request;

            request.Payload = "error" + std::to_string(i);
            request.Action = PingAction::Error;

            printf("Sending request\n");
            fflush(stdout);

            bond::ext::gRPC::wait_callback<PingResponse> cb;
            client.AsyncPing(request, cb);
            bool gotResponse = cb.wait_for(std::chrono::seconds(1));

            if (!gotResponse)
            {
                printf("Client timed out waiting for response\n");
                fflush(stdout);
                exit(1);
            }

            Status status = cb.status();

            if (status.ok())
            {
                printf("Non-error response received: %s\n", cb.response().Deserialize().Payload.c_str());
                printf("Client failed\n");
                fflush(stdout);
                exit(1);
            }
        }
    }
    catch (std::exception& e)
    {
        printf("Caught exception: %s\n", e.what());
        fflush(stdout);
        exit(1);
    }
    catch (...)
    {
        printf("Caught something\n");
        fflush(stdout);
        exit(1);
    }

    printf("Client succeeded\n");
    fflush(stdout);
    return 0;
}
