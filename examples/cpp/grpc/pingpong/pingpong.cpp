#include "pingpong_grpc.h"
#include "pingpong_types.h"

// event.h needed for test purposes
#include <bond/ext/detail/event.h>

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/wait_callback.h>

#include <chrono>
#include <cstdlib>
#include <functional>
#include <iostream>
#include <memory>
#include <string>

using grpc::Channel;
using grpc::ClientContext;
using grpc::ServerBuilder;
using grpc::Status;
using grpc::StatusCode;

using bond::ext::detail::event;
using bond::ext::gRPC::io_manager;
using bond::ext::gRPC::wait_callback;

using namespace pingpong;

class DoublePingServiceImpl final : public DoublePing::Service
{
public:
    event pingNoResponse_event;

private:
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

template <typename T>
static void assertResponseReceived(wait_callback<T>& cb, size_t line)
{
    bool wasInvoked = cb.wait_for(std::chrono::seconds(2));
    if (!wasInvoked)
    {
        std::cerr << "Callback invocation at line " << line << " timed out." << std::endl;
        abort();
    }
}

static void assertStatus(StatusCode expected, StatusCode actual, size_t line)
{
    if (expected != actual)
    {
        std::cerr
            << "Expected status code " << expected << " but got " << actual
            << " at line " << line << std::endl;
        abort();
    }
}

static void assertResponseContents(const wait_callback<PingReply>& cb, size_t line)
{
    assertStatus(StatusCode::OK, cb.status().error_code(), line);

    const std::string& message = cb.response().Deserialize().message;
    if (message.compare("ping pong") != 0)
    {
        std::cerr << "Response at line " << line << " had unexpected message: " << message << std::endl;
        abort();
    }
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

    PingRequest request;
    request.name = user;

    {
        ClientContext context;
        wait_callback<PingReply> cb;
        doublePing.AsyncPing(&context, request, cb);
        assertResponseReceived(cb, __LINE__);
        assertResponseContents(cb, __LINE__);
    }

    {
        ClientContext context;
        wait_callback<PingReply> cb;
        doublePing.AsyncPingNoPayload(&context, cb);
        assertResponseReceived(cb, __LINE__);
        assertResponseContents(cb, __LINE__);
    }

    {
        ClientContext context;
        doublePing.AsyncPingNoResponse(&context, request);
        bool wasEventHandled = double_ping_service.pingNoResponse_event.wait_for(std::chrono::seconds(10));

        if (!wasEventHandled)
        {
            std::cerr << "timeout ocurred waiting for event to be handled at line " << __LINE__ << std::endl;
            abort();
        }
    }

    {
        ClientContext context;
        wait_callback<bond::Void> cb;
        doublePing.AsyncPingVoid(&context, cb);
        assertResponseReceived(cb, __LINE__);
        assertStatus(StatusCode::OK, cb.status().error_code(), __LINE__);
    }

    {
        ClientContext context;
        wait_callback<PingReply> cb;
        doublePing.AsyncPingShouldThrow(&context, request, cb);
        assertResponseReceived(cb, __LINE__);
        assertStatus(StatusCode::CANCELLED, cb.status().error_code(), __LINE__);
    }

    {
        ClientContext context;
        wait_callback<PingReply> cb;
        pingPong.AsyncPing(&context, request, cb);
        assertResponseReceived(cb, __LINE__);
        assertResponseContents(cb, __LINE__);
    }

    return 0;
}
