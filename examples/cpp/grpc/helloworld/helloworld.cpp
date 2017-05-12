#include "helloworld_types.h"
#include "helloworld_grpc.h"

// TODO: this should be generated
#include "helloworld_service.h"

#ifdef _MSC_VER
    #pragma warning (push)
    #pragma warning (disable: 4100)
#endif

#include <grpc++/grpc++.h>

#ifdef _MSC_VER
    #pragma warning (pop)
#endif

#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/unary_call.h>

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;

using namespace helloworld;

class GreeterClient {
 public:
  GreeterClient(std::shared_ptr<Channel> channel)
      : stub_(Greeter::NewStub(channel)) {}

  // Assembles the client's payload, sends it and presents the response back
  // from the server.
  std::string SayHello(const std::string& user) {
    ClientContext context;

    HelloRequest request;
    request.name = user;

    HelloReply reply;

    // The actual RPC.
    bond::comm::message<HelloRequest> req(request);
    bond::comm::message<HelloReply> rep;
    Status status = stub_->SayHello(&context, req, &rep);

    if (status.ok()) {
      return rep.value().Deserialize().message;
    } else {
      std::cout << status.error_code() << ": " << status.error_message()
                << std::endl;
      return "RPC failed";
    }
  }

 private:
  std::unique_ptr<Greeter::Stub> stub_;
};

// Logic and data behind the server's behavior.
class GreeterServiceImpl final : public GreeterServiceAsync {
    void SayHello(
        bond::ext::gRPC::unary_call<
            bond::comm::message<::helloworld::HelloRequest>,
            bond::comm::message<::helloworld::HelloReply>> call) override
    {
        HelloReply real_reply;
        real_reply.message = "hello " + call.request().value().Deserialize().name;

        bond::comm::message<HelloReply> rep(real_reply);

        call.Finish(rep, Status::OK);
    }
};

int main()
{
    const std::string server_address("127.0.0.1:50051");
    GreeterServiceImpl service;

    bond::ext::gRPC::server_builder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<bond::ext::gRPC::server> server(builder.BuildAndStart());

    GreeterClient greeter(grpc::CreateChannel(server_address, grpc::InsecureChannelCredentials()));
    std::string user("world");
    std::string reply = greeter.SayHello(user);

    if (strcmp(reply.c_str(), "hello world") == 0)
    {
        return 0;
    }
    else
    {
        std::cout << "Did not receive correct response. received: " << reply;
        return 1;
    }
}
