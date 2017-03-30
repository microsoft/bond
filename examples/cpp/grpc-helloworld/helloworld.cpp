#include "helloworld_types.h"
#include "helloworld_grpc.h"

#pragma warning (push)
#pragma warning (disable: 4100)

#include <grpc++/grpc++.h>

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
    request.name = "world";

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
class GreeterServiceImpl final : public Greeter::Service {
  Status SayHello(ServerContext* context, const bond::comm::message<HelloRequest>* request,
                  bond::comm::message<HelloReply>* reply) override {
    HelloReply real_reply;
    real_reply.message = "hello " + request->value().Deserialize().name;

    bond::comm::message<HelloReply> rep(real_reply);
    *reply = rep;

    return Status::OK;
  }
};

int main()
{
    const std::string server_address("127.0.0.1:50051");
    GreeterServiceImpl service;

    ServerBuilder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);
    std::unique_ptr<Server> server(builder.BuildAndStart());

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

#pragma warning (pop)
