#include <bond/ext/grpc/detail/service.h>
#include <bond/ext/grpc/detail/service_call_data.h>
#include <bond/ext/grpc/server.h>
#include <bond/ext/grpc/server_builder.h>
#include <bond/ext/grpc/unary_call.h>

#include <bond/ext/grpc/bond_utils.h>


// TODO: generate this code instead of writing it by hand
class GreeterServiceAsync : public bond::ext::gRPC::detail::service
{
public:
    GreeterServiceAsync()
    {
        // code gen a call to AddMethod for each method with the right name. Order will matter.
        AddMethod("/helloworld.Greeter/SayHello");
    }

    // code gen a virtual method with a signature like this
    virtual void SayHello(
        bond::ext::gRPC::unary_call<
            bond::comm::message<::helloworld::HelloRequest>,
            bond::comm::message<::helloworld::HelloReply>> call) = 0;

    void start(grpc::ServerCompletionQueue* cq) override
    {
        BOOST_ASSERT(cq);

        // for each method, actually initialize the service_unary_call_data
        _receiveHello.emplace(
            this,
            0,
            cq,
            std::bind(
                &GreeterServiceAsync::SayHello,
                this,
                std::placeholders::_1));

        // code gen enqueueing a receive to start processing the method
        queue_receive(
            0,
            &_receiveHello->_receivedCall->_context,
            &_receiveHello->_receivedCall->_request,
            &_receiveHello->_receivedCall->_responder,
            cq,
            &_receiveHello.get());
    }

private:
    // code gen a data member for each method
    boost::optional<bond::ext::gRPC::detail::service_unary_call_data<
        bond::comm::message<::helloworld::HelloRequest>,
        bond::comm::message<::helloworld::HelloReply>>> _receiveHello;
};
