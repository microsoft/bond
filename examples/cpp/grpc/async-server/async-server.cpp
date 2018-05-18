#include "async-server_grpc.h"
#include "async-server_types.h"

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/shared_unary_call.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>

#include <chrono>
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <thread>

using namespace helloworld;

class GreeterServiceImpl final : public Greeter::Service
{
    class PerRequestState
    {
    public:
        explicit PerRequestState(bond::ext::gRPC::unary_call<HelloRequest, HelloReply> call)
            : _call(std::move(call).share())
        { }

        // In this example, the PerRequestState doubles as the thread pool
        // callback. This need not be the case.
        void operator()()
        {
            // Simulate doing some other work...
            std::this_thread::sleep_for(std::chrono::milliseconds(750));

            HelloRequest request = _call.request().Deserialize();

            HelloReply reply;
            reply.message = "hello " + request.name;

            _call.Finish(reply);
        }

    private:
        // The thread pool implementation that we're using requires its
        // Callbacks to be copyable, so we switch to using
        // shared_unary_call, which is copyable.
        bond::ext::gRPC::shared_unary_call<HelloRequest, HelloReply> _call;

        // Other state could be added as needed.
    };

public:
    // In this example, we use the same thread pool to perform asynchronous
    // processing of requests as is used in the rest of the program.
    explicit GreeterServiceImpl(bond::ext::gRPC::thread_pool tp)
        : Greeter::Service(std::move(tp))
    { }

    void SayHello(bond::ext::gRPC::unary_call<HelloRequest, HelloReply> call) override
    {
        scheduler()(PerRequestState{ std::move(call) });
    }
};

int main()
{
    const std::string server_address("127.0.0.1:50051");

    auto ioManager = std::make_shared<bond::ext::gRPC::io_manager>();
    bond::ext::gRPC::thread_pool threadPool;

    std::unique_ptr<GreeterServiceImpl> service{ new GreeterServiceImpl{ threadPool } };

    auto server = bond::ext::gRPC::server_builder{}
        .AddListeningPort(server_address, grpc::InsecureServerCredentials())
        .RegisterService(std::move(service))
        .BuildAndStart();

    Greeter::Client greeter(
        grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
        ioManager,
        threadPool);

    const std::string user("world");

    HelloRequest request;
    request.name = user;

    auto result = greeter.AsyncSayHello(request);
    if (result.wait_for(std::chrono::seconds(10)) == std::future_status::timeout)
    {
        std::cout << "timeout ocurred";
        return 1;
    }

    HelloReply reply;
    try
    {
        result.get().response().Deserialize(reply);
    }
    catch (const bond::ext::gRPC::UnaryCallException& e)
    {
        std::cout << "request failed: " << e.status().error_message();
        return 1;
    }

    if (reply.message.compare("hello world") != 0)
    {
        std::cout << "Wrong response: " << reply.message;
        return 1;
    }

    std::cout << "Correct response: " << reply.message;
    return 0;
}
