// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

// Include auto-generated files
#include "pingpong_grpc.h"
#include "pingpong_reflection.h"
#include "pingpong_types.h"

#include <bond/ext/grpc/io_manager.h>
#include <bond/ext/detail/event.h>

#include <boost/optional/optional.hpp>

#include <chrono>
#include <memory>
#include <stdio.h>
#include <string>
#include <thread>

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

using namespace PingPongNS;

template <typename TResponse>
class wait_callback
{
public:
    wait_callback() :
        _response(),
        _status(),
        _event()
    { }

    wait_callback(const wait_callback&) = delete;
    wait_callback(wait_callback&&) = delete;
    wait_callback& operator=(const wait_callback&) = delete;
    wait_callback& operator=(wait_callback&&) = delete;

    std::function<void(const ::bond::comm::message<TResponse>&, const ::grpc::Status&)> callback()
    {
        return std::function<void(const ::bond::comm::message<TResponse>&, const ::grpc::Status&)>(
            [this](const ::bond::comm::message<TResponse>& response, const ::grpc::Status& status)
            {
                _response.emplace(response);
                _status.emplace(status);
                _event.set();
            });
    }

    operator std::function<void(const ::bond::comm::message<TResponse>&, const ::grpc::Status&)>()
    {
        return callback();
    }

    void wait()
    {
        _event.wait();
    }

    template <typename Rep, typename Period>
    bool wait(std::chrono::duration<Rep, Period> timeout)
    {
        return _event.wait(timeout);
    }

    const ::bond::comm::message<TResponse>& response() const
    {
        return _response.get();
    }

    const grpc::Status& status() const
    {
        return _status.get();
    }

private:
    boost::optional<::bond::comm::message<TResponse>> _response;
    boost::optional<grpc::Status> _status;

    bond::ext::detail::event _event;
};

int main()
{
    auto ioManager = std::make_shared<bond::ext::gRPC::io_manager>(
        std::unique_ptr<grpc::CompletionQueue>(new grpc::CompletionQueue));

    const std::string server_address("127.0.0.1:" + std::to_string(Port));
    std::shared_ptr<Channel> channel = grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials());

    std::this_thread::sleep_for(std::chrono::seconds(1));

    PingPong::PingPongClient client(channel, ioManager);

    printf("Start client\n");
    fflush(stdout);

    try
    {
        for (int i = 0; i < NumRequests; i++)
        {
            ClientContext context;
            PingRequest request;

            request.Payload = "request" + std::to_string(i);
            request.Action = PingAction::Identity;

            printf("Sending\n");
            fflush(stdout);

            wait_callback<PingResponse> cb;
            client.AsyncPing(&context, request, cb);
            bool gotResponse = cb.wait(std::chrono::seconds(1));

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

            if (cb.response().value().Deserialize().Payload != request.Payload)
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

            request.Payload = "error" + std::to_string(i);
            request.Action = PingAction::Error;

            wait_callback<PingResponse> cb;
            client.AsyncPing(&context, request, cb);
            bool gotResponse = cb.wait(std::chrono::seconds(1));

            if (!gotResponse)
            {
                printf("Client timed out waiting for response\n");
                fflush(stdout);
                exit(1);
            }

            Status status = cb.status();

            if (status.ok())
            {
                printf("Non-error response received: %s\n", cb.response().value().Deserialize().Payload.c_str());
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
