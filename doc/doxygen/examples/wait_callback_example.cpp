#include <bond/ext/grpc/wait_callback.h>
// ...

int main()
{
    Example::Client client(/* ... */);

    bond::ext::gRPC::wait_callback<ExampleResponse> cb;
    client.AsyncExampleMethod(/* ... */, cb);

    cb.wait();
    if (cb.status().ok())
    {
        DoSomeThingWith(cb.response());
    }
}
