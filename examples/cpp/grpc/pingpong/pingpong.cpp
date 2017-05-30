#include "pingpong_grpc.h"
#include "pingpong_types.h"

// countdown_event.h and event.h needed for test purposes
#include <bond/ext/detail/countdown_event.h>
#include <bond/ext/detail/event.h>

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>

#include <chrono>
#include <functional>
#include <iostream>
#include <memory>
#include <string>

using grpc::Channel;
using grpc::ClientContext;
using grpc::ServerBuilder;
using grpc::Status;
using grpc::StatusCode;

using bond::ext::detail::countdown_event;
using bond::ext::detail::event;
using bond::ext::gRPC::io_manager;

using namespace pingpong;

event pingNoResponse_event;

class DoublePingServiceImpl final : public DoublePing::Service
{
    void Ping(
        bond::ext::gRPC::unary_call<
            bond::bonded<PingRequest>,
            PingReply> call) override
    {
        PingRequest request = call.request().Deserialize();

        PingReply reply;
        reply.message = "ping " + request.name;

        call.Finish(reply);
    }

    void PingNoPayload(
        bond::ext::gRPC::unary_call<
            bond::bonded<bond::Void>,
            PingReply> call) override
    {
        PingReply reply;
        reply.message = "ping pong";

        call.Finish(reply, Status::OK);
    }

    void PingNoResponse(
        bond::ext::gRPC::unary_call<
            bond::bonded<PingRequest>,
            bond::Void> call) override
    {
        PingRequest request = call.request().Deserialize();

        // TODO: the current implementation requires that we respond with dummy data.
        // This will be fixed in a later release.
        call.Finish(bond::bonded<bond::Void>{bond::Void()});

        pingNoResponse_event.set();
    }

    void PingVoid(
        bond::ext::gRPC::unary_call<
            bond::bonded<bond::Void>,
            bond::Void> call) override
    {
        // TODO: the current implementation requires that we respond with dummy data.
        // This will be fixed in a later release.
        call.Finish(bond::Void());
    }

    void PingEventVoid(
        bond::ext::gRPC::unary_call<
            bond::bonded<bond::Void>,
            bond::Void> call) override
    {
        // TODO: the current implementation requires that we respond with dummy data.
        // This will be fixed in a later release.
        call.Finish(bond::Void());
    }

    void PingShouldThrow(
        bond::ext::gRPC::unary_call<
            bond::bonded<PingRequest>,
            PingReply> call) override
    {
        call.FinishWithError(Status(StatusCode::CANCELLED, "do not want to respond"));
    }
};

class PingPongServiceImpl final : public PingPong<PingRequest>::Service
{
    void Ping(
        bond::ext::gRPC::unary_call<
            bond::bonded<PingRequest>,
            PingReply> call) override
    {
        PingRequest request = call.request().Deserialize();

        PingReply reply;
        reply.message = "ping " + request.name;

        call.Finish(bond::bonded<PingReply>{reply}, Status::OK);
        // could also call:
        // call.Finish(reply);
    }
};

void printAndSet(
    countdown_event* print_event,
    bool* isCorrectResponse,
    const bond::bonded<PingReply>& response,
    const Status& status)
{

    *isCorrectResponse = false;

    if(status.ok())
    {
        const std::string& message = response.Deserialize().message;

        if (message.compare("ping pong") == 0)
        {
            std::cout << "Correct response: " << message;
            *isCorrectResponse = true;
        }
        else
        {
            std::cout << "Wrong response";
            *isCorrectResponse = false;
        }
    }

    print_event->set();
}

int main()
{
    auto ioManager = std::make_shared<io_manager>();
    auto threadPool = std::make_shared<bond::ext::gRPC::thread_pool>();

    DoublePingServiceImpl double_ping_service;
    PingPongServiceImpl ping_pong_service;

    bond::ext::gRPC::server_builder builder;
    builder.SetThreadPool(threadPool);
    const std::string server_address("127.0.0.1:50051");
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder
        .RegisterService(&double_ping_service)
        .RegisterService(&ping_pong_service);
    std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());

    DoublePing::Client doublePing(
        grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
        ioManager,
        threadPool);

    PingPong<PingRequest>::Client pingPong(
        grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
        ioManager,
        threadPool);


    const std::string user("pong");

    countdown_event ping_event(5);

    bool isCorrectResponse;
    bool pingNoPayloadIsCorrectResponse;
    bool pingVoidIsCorrectResponse;
    bool pingShouldThrowIsCorrectResponse;
    bool pingGenericIsCorrectResponse;

    ClientContext context;
    ClientContext pingNoPayloadContext;
    ClientContext pingNoResponseContext;
    ClientContext pingVoidContext;
    ClientContext pingShouldThrowContext;
    ClientContext pingGenericContext;

    PingRequest request;
    request.name = user;

    {
        auto f_print = [&ping_event, &isCorrectResponse](bond::bonded<PingReply> response, Status status)
            {
                printAndSet(&ping_event, &isCorrectResponse, response, status);
            };

        doublePing.AsyncPing(&context, request, f_print);
    }

    {
        auto f_print = [&ping_event, &pingNoPayloadIsCorrectResponse](bond::bonded<PingReply> response, Status status)
            {
                printAndSet(&ping_event, &pingNoPayloadIsCorrectResponse, response, status);
            };

        doublePing.AsyncPingNoPayload(&pingNoPayloadContext, f_print);
    }

    {
        doublePing.AsyncPingNoResponse(&pingNoResponseContext, request);
    }

    {
        auto f_print = [&ping_event, &pingVoidIsCorrectResponse](bond::bonded<bond::Void> response, Status status)
            {
                if(status.ok())
                {
                    pingVoidIsCorrectResponse = true;
                }
                else
                {
                    pingVoidIsCorrectResponse = false;
                }
                ping_event.set();
            };

        doublePing.AsyncPingVoid(&pingVoidContext, f_print);
    }

    {
        auto f_print = [&ping_event, &pingShouldThrowIsCorrectResponse](bond::bonded<PingReply> response, Status status)
            {
                pingShouldThrowIsCorrectResponse = false;

                if(status.error_code() == StatusCode::CANCELLED)
                {
                    pingShouldThrowIsCorrectResponse = true;
                }

                ping_event.set();
            };

        doublePing.AsyncPingShouldThrow(&pingShouldThrowContext, request, f_print);
    }

    {
        auto f_print = [&ping_event, &pingGenericIsCorrectResponse](bond::bonded<PingReply> response, Status status)
            {
                printAndSet(&ping_event, &pingGenericIsCorrectResponse, response, status);
            };

        pingPong.AsyncPing(&pingGenericContext, request, f_print);
    }

    bool waitResult = ping_event.wait(std::chrono::seconds(10));
    waitResult &= pingNoResponse_event.wait(std::chrono::seconds(10));

    if (!waitResult)
    {
        std::cout << "timeout ocurred";
    }

    if (waitResult
            && isCorrectResponse
            && pingNoPayloadIsCorrectResponse
            && pingVoidIsCorrectResponse
            && pingShouldThrowIsCorrectResponse
            && pingGenericIsCorrectResponse)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}
