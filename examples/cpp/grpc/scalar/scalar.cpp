// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include "scalar_grpc.h"
#include "scalar_types.h"

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>

#include <bond/core/bond_apply.h>
#include <bond/core/bond_types.h>
#include <bond/core/box.h>

#include <chrono>
#include <functional>
#include <iostream>
#include <memory>
#include <string>

using namespace scalar;

// Logic and data behind the server's behavior.
class ScalarMethodsImpl final : public ScalarMethods::Service
{
public:
    using ScalarMethods::Service::Service;

private:
    void Negate(bond::ext::gRPC::unary_call<bond::Box<int32_t>, bond::Box<int32_t>> call) override
    {
        bond::Box<int32_t> request = call.request().Deserialize();
        call.Finish(bond::make_box(-request.value));
    }

    void Sum(bond::ext::gRPC::unary_call<bond::Box<std::vector<uint64_t>>, bond::Box<uint64_t>> call) override
    {
        bond::Box<std::vector<uint64_t>> request = call.request().Deserialize();

        bond::Box<uint64_t> reply;
        for (auto v : request.value)
        {
            reply.value += v;
        }

        call.Finish(reply);
    }
};

template <typename T>
void ValidateResponseOrDie(
    const char* what,
    const T& expected,
    std::future<bond::ext::gRPC::unary_call_result<bond::Box<T>>> result)
{
    if (result.wait_for(std::chrono::seconds(10)) == std::future_status::timeout)
    {
        std::cout << what << ": timeout ocurred\n";
        exit(1);
    }

    bond::Box<T> reply;
    try
    {
        result.get().response().Deserialize(reply);
    }
    catch (const bond::ext::gRPC::UnaryCallException& e)
    {
        std::cout << "request failed: " << e.status().error_message();
        exit(1);
    }

    if (reply.value != expected)
    {
        std::cout << what << ": expected '" << expected << "' but got '" << reply.value << "'.\n";
        exit(1);
    }

    std::cout << what << ": correct response '" << reply.value << "'.\n";
}

static void MakeNegateRequest(ScalarMethods::Client& client)
{
    ValidateResponseOrDie("negate", int32_t{-10}, client.AsyncNegate(bond::make_box(10)));
}

static void MakeSumRequest(ScalarMethods::Client& client)
{
    ValidateResponseOrDie("sum", uint64_t{15}, client.AsyncSum(bond::make_box(std::vector<uint64_t>{1, 2, 3, 4, 5})));
}

int main()
{
    auto ioManager = std::make_shared<bond::ext::gRPC::io_manager>();
    bond::ext::gRPC::thread_pool threadPool;

    std::unique_ptr<ScalarMethodsImpl> service{ new ScalarMethodsImpl{ threadPool } };

    const std::string server_address("127.0.0.1:50051");

    grpc::ServerBuilder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());

    auto server = bond::ext::gRPC::server::Start(builder, std::move(service));

    ScalarMethods::Client client(
        grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
        ioManager,
        threadPool);

    MakeNegateRequest(client);
    MakeSumRequest(client);
}
