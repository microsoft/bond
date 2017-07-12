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
using grpc::ServerBuilder;
using grpc::Status;
using grpc::StatusCode;

using bond::ext::detail::event;
using bond::ext::gRPC::io_manager;
using bond::ext::gRPC::wait_callback;

using namespace pingpong;

static const char* metadata_key = "metadata-key";

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

        // the server context can been accessed to add, for example,
        // additional metadata to the response
        call.context().AddInitialMetadata(metadata_key, "metadata-value");
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
        // A bonded object can also be used for the request. Here we use a
        // bonded object backed by an instance of PingRequest, but we could
        // also use one backed by a reader.
        bond::bonded<PingRequest> bondedRequest(request);
        wait_callback<PingReply> cb;
        doublePing.AsyncPing(bondedRequest, cb);
        assertResponseReceived(cb, __LINE__);
        assertResponseContents(cb, __LINE__);
    }

    {
        // We explicitly pass a client context to this method so we can, for
        // example, inspect the metadata the service included in the
        // response.
        auto context = std::make_shared<grpc::ClientContext>();
        wait_callback<PingReply> cb;
        doublePing.AsyncPingNoPayload(context, cb);
        assertResponseReceived(cb, __LINE__);
        assertResponseContents(cb, __LINE__);

        // After the response has been received, the server metadata can be
        // inspected.
        const auto& initialMetadata = context->GetServerInitialMetadata();
        auto it = initialMetadata.find(metadata_key);
        if (it == initialMetadata.end())
        {
            std::cerr << "Server was expected to include metadata with key \"" << metadata_key << "\" but it did not." << std::endl;
            abort();
        }
        else if (it->second.compare("metadata-value") != 0)
        {
            std::cerr << "Server metadata value is unexpected. \"" << it->first << "\": \"" << it->second << "\"" << std::endl;
            abort();
        }
    }

    {
        doublePing.AsyncPingNoResponse(request);
        bool wasEventHandled = double_ping_service.pingNoResponse_event.wait_for(std::chrono::seconds(10));

        if (!wasEventHandled)
        {
            std::cerr << "timeout ocurred waiting for event to be handled at line " << __LINE__ << std::endl;
            abort();
        }
    }

    {
        wait_callback<bond::Void> cb;
        doublePing.AsyncPingVoid(cb);
        assertResponseReceived(cb, __LINE__);
        assertStatus(StatusCode::OK, cb.status().error_code(), __LINE__);
    }

    {
        wait_callback<PingReply> cb;
        doublePing.AsyncPingShouldThrow(request, cb);
        assertResponseReceived(cb, __LINE__);
        assertStatus(StatusCode::CANCELLED, cb.status().error_code(), __LINE__);
    }

    {
        wait_callback<PingReply> cb;
        pingPong.AsyncPing(request, cb);
        assertResponseReceived(cb, __LINE__);
        assertResponseContents(cb, __LINE__);
    }

    return 0;
}
