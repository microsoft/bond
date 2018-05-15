// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#include "scalar_grpc.h"
#include "scalar_types.h"

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/thread_pool.h>
#include <bond/ext/grpc/unary_call.h>
#include <bond/ext/grpc/wait_callback.h>

#include <bond/core/bond_apply.h>
#include <bond/core/bond_types.h>
#include <bond/core/box.h>

#include <chrono>
#include <functional>
#include <iostream>
#include <memory>
#include <string>

using grpc::Channel;
using grpc::ServerBuilder;
using grpc::Status;

using namespace scalar;

// Logic and data behind the server's behavior.
class ScalarMethodsImpl final : public ScalarMethods::Service
{
    void Negate(
        bond::ext::gRPC::unary_call<
            bond::bonded<bond::Box<int32_t>>,
            bond::Box<int32_t>> call) override
    {
        bond::Box<int32_t> request = call.request().Deserialize();
        call.Finish(bond::make_box(-request.value));
    }

    void Sum(
         bond::ext::gRPC::unary_call<
             bond::bonded<bond::Box<std::vector<uint64_t>>>,
             bond::Box<uint64_t>> call) override
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
static void ValidateResponseOrDie(
    const char* what,
    const T& expected,
    bond::ext::gRPC::wait_callback<bond::Box<T>>& cb)
{
    bool waitResult = cb.wait_for(std::chrono::seconds(10));

    if (!waitResult)
    {
        std::cout << what << ": timeout ocurred\n";
        exit(1);
    }
    else if (!cb.status().ok())
    {
        std::cout << what <<": request failed\n";
        exit(1);
    }

    bond::Box<T> reply;
    cb.response().Deserialize(reply);

    if (reply.value != expected)
    {
        std::cout << what << ": expected '" << expected << "' but got '" << reply.value << "'.\n";
        exit(1);
    }

    std::cout << what << ": correct response '" << reply.value << "'.\n";
}

static void MakeNegateRequest(ScalarMethods::Client& client)
{
    bond::Box<int32_t> request = bond::make_box(10);

    bond::ext::gRPC::wait_callback<bond::Box<int32_t>> cb;
    client.AsyncNegate(request, cb);

    ValidateResponseOrDie("negate", int32_t{-10}, cb);
}

static void MakeSumRequest(ScalarMethods::Client& client)
{
    bond::Box<std::vector<uint64_t>> request = bond::make_box(std::vector<uint64_t>{1, 2, 3, 4, 5});

    bond::ext::gRPC::wait_callback<bond::Box<uint64_t>> cb;
    client.AsyncSum(request, cb);

    ValidateResponseOrDie("sum", uint64_t{15}, cb);
}

int main()
{
    ScalarMethodsImpl service;

    const std::string server_address("127.0.0.1:50051");

    auto server = bond::ext::gRPC::server_builder{}
        .AddListeningPort(server_address, grpc::InsecureServerCredentials())
        .RegisterService(&service)
        .BuildAndStart();

    ScalarMethods::Client client(
        grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()),
        std::make_shared<bond::ext::gRPC::io_manager>());

    MakeNegateRequest(client);
    MakeSumRequest(client);
}
