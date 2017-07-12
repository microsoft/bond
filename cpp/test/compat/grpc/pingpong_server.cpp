// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// Include auto-generated files
#include "pingpong_grpc.h"
#include "pingpong_reflection.h"
#include "pingpong_types.h"

#include <bond/ext/detail/countdown_event.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/unary_call.h>

#include <chrono>
#include <memory>
#include <stdio.h>
#include <string>
#include <thread>

using grpc::Status;
using grpc::StatusCode;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;

using namespace PingPongNS;
using namespace bond::ext::detail;

static countdown_event Countdown(NumRequests + NumEvents + NumErrors);
static std::atomic<uint32_t> NumRequestsReceived(0);
static std::atomic<uint32_t> NumEventsReceived(0);
static std::atomic<uint32_t> NumErrorsReceived(0);

// Logic and data behind the server's behavior.
class PingPongServiceImpl final : public PingPong::Service
{
public:

    void Ping(
        bond::ext::gRPC::unary_call<
            bond::bonded<PingRequest>,
            PingResponse> call) override
    {
        PingRequest request = call.request().Deserialize();

        switch (request.Action)
        {
            case PingAction::Identity:
            {
                printf("Received identity request \"%s\"\n", request.Payload.c_str());
                fflush(stdout);

                PingResponse response;
                response.Payload = request.Payload;

                NumRequestsReceived++;

                call.Finish(response);
                Countdown.set();
                break;
            }

            case PingAction::Error:
            {
                printf("Received error request \"%s\"\n", request.Payload.c_str());
                fflush(stdout);

                NumErrorsReceived++;

                Status error(StatusCode::UNIMPLEMENTED, "Application Exception");
                call.FinishWithError(error);
                Countdown.set();
                break;
            }

            default:
            {
                printf("Received unknown request \"%s\"\n", request.Payload.c_str());
                fflush(stdout);

                Status error(StatusCode::UNIMPLEMENTED, "Unknown PingAction");
                call.FinishWithError(error);
                Countdown.set();
                break;
            }
        }
    }

    void PingEvent(
        bond::ext::gRPC::unary_call<
        bond::bonded<PingRequest>,
        bond::Void> call) override
    {
        PingRequest request = call.request().Deserialize();

        printf("Received event \"%s\"\n", request.Payload.c_str());
        fflush(stdout);

        NumEventsReceived++;

        // TODO: the current implementation requires that we respond with dummy data.
        // This will be fixed in a later release.
        call.Finish(bond::bonded<bond::Void>{bond::Void()});
        Countdown.set();
    }

};

int main()
{
    PingPongServiceImpl service;
    auto threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();

    bond::ext::gRPC::server_builder builder;
    builder.SetThreadPool(threadPool);
    const std::string server_address("127.0.0.1:" + std::to_string(Port));
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());

    printf("Server ready\n");
    fflush(stdout);

    bool countdownSet = Countdown.wait_for(std::chrono::seconds(30));

    server->Shutdown(std::chrono::system_clock::now() + std::chrono::seconds(10));
    server->Wait();

    if (!countdownSet ||
        (NumRequestsReceived != NumRequests) ||
        (NumEventsReceived != NumEvents) ||
        (NumErrorsReceived != NumErrors))
    {
        printf("Server failed: Did not receive all expected messages\n");
        fflush(stdout);
        exit(1);
    }

    printf("Server completed\n");
    fflush(stdout);
    return 0;
}
