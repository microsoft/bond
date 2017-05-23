#include "helloworld_grpc.h"
#include "helloworld_types.h"

// event.h needed for test purposes
#include <bond/ext/detail/event.h>

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/thread_pool.h>

#include <chrono>
#include <functional>
#include <iostream>
#include <memory>
#include <string>

using grpc::Channel;
using grpc::ClientContext;
using grpc::ServerBuilder;
using grpc::Status;

using bond::ext::detail::event;
using bond::ext::gRPC::io_manager;

using namespace helloworld;

// Logic and data behind the server's behavior.
class GreeterServiceImpl final : public Greeter::Service
{
    void SayHello(
        bond::ext::gRPC::unary_call<
        bond::bonded<HelloRequest>,
        bond::bonded<HelloReply>> call) override
    {
        HelloRequest request = call.request().Deserialize();

        HelloReply reply;
        reply.message = "hello " + request.name;

        call.Finish(bond::bonded<HelloReply>{reply}, Status::OK);
    }
};

void printAndSet(
    event* print_event,
    bool* isCorrectResponse,
    const bond::bonded<HelloReply>& response,
    const Status& status)
{

    *isCorrectResponse = false;

    if(status.ok())
    {
        const std::string& message = response.Deserialize().message;

        if (message.compare("hello world") == 0)
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
    bond::ext::thread_pool threadPool;

    GreeterServiceImpl service;

    bond::ext::gRPC::server_builder builder(&threadPool);
    const std::string server_address("127.0.0.1:50051");
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());

    std::unique_ptr<grpc::CompletionQueue> cq_(new grpc::CompletionQueue());
    auto ioManager = std::make_shared<io_manager>(std::move(cq_));

    Greeter::Client greeter(
        grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
        ioManager,
        &threadPool);

    ClientContext context;

    const std::string user("world");

    HelloRequest request;
    request.name = user;
    bond::bonded<HelloRequest> req(request);
    event print_event;
    bool isCorrectResponse;

    std::function<void(const bond::bonded<HelloReply>&, const Status&)> f_print =
        [&print_event, &isCorrectResponse](bond::bonded<HelloReply> response, Status status)
        {
            printAndSet(&print_event, &isCorrectResponse, response, status);
        };

    greeter.AsyncSayHello(&context, req, f_print);

    bool waitResult = print_event.wait(std::chrono::seconds(10));

    if (!waitResult)
    {
        std::cout << "timeout ocurred";
    }

    if (waitResult && isCorrectResponse)
    {
        return 0;
    }
    else
    {
        return 1;
    }
}
