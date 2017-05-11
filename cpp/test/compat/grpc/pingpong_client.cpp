// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include <stdio.h>

#include <boost/lexical_cast.hpp>
#include <boost/thread/thread.hpp>

// Include auto-generated files
#include "pingpong_reflection.h"
#include "pingpong_types.h"
#include "pingpong_grpc.h"

#ifdef _MSC_VER
#pragma warning (push)
#pragma warning (disable: 4100)
#endif

#include <grpc++/grpc++.h>

#ifdef _MSC_VER
#pragma warning (pop)
#endif

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

using namespace PingPongNS;


int main()
{
    const std::string server_address("127.0.0.1:" + boost::lexical_cast<std::string>(Port));
    std::shared_ptr<Channel> channel = grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials());

    boost::this_thread::sleep_for(boost::chrono::seconds(1));

    std::unique_ptr<PingPong::Stub> stub = PingPong::NewStub(channel);

    printf("Start client\n");
    fflush(stdout);

    try
    {
        for (int i = 0; i < NumRequests; i++)
        {
            ClientContext context;
            PingRequest request;
            bond::comm::message<PingResponse> response;

            request.Payload = "request" + boost::lexical_cast<std::string>(i);
            request.Action = PingAction::Identity;

            printf("Sending\n");
            fflush(stdout);

            Status status = stub->Ping(&context, request, &response);

            if (!status.ok())
            {
                printf("Error response received: %d\n", status.error_code());
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

        for (int i = 0; i < NumErrors; i++)
        {
            ClientContext context;
            PingRequest request;
            bond::comm::message<PingResponse> response;

            request.Payload = "error" + boost::lexical_cast<std::string>(i);
            request.Action = PingAction::Error;

            Status status = stub->Ping(&context, request, &response);

            if (status.ok())
            {
                printf("Non-error response received: %s\n", response.value().Deserialize().Payload.c_str());
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
    }
    catch (...)
    {
        printf("Caught something\n");
        fflush(stdout);
    }

    printf("Client succeeded\n");
    fflush(stdout);
    return 0;
}
