% Bond-over-gRPC

# About #

Bond-over-gRPC provides code generation from Bond IDL service definitions
to send Bond objects via gRPC. The gRPC functionality supercedes the
(now deprecated) Bond Comm framework.

# Features #

## Defining Services ##

The Bond IDL has been extended to support the definition of
[services](compiler.html#service-definition) and
[generic services](compiler.html#generic-service). These definitions are
used by the Bond compiler to generate:

* service stubs that can be used as the basis for implementing service
  methods
* proxy stubs that can be used by clients to invoke those methods

To generate these stubs, pass the `--grpc` flag to `gbc` (the Bond compiler
tool).

Note that gRPC doesn't provide a messaging pattern that matches
the semantics of methods with a return type of `nothing`; to compensate,
`gbc` provides generated wrappers to simulate the appropriate semantics.

# Implementations #

Bond-over-gRPC is available for C# now and will be released for C++ in a
few weeks.

## Bond-over-gRPC for C# ##

Given a service definition like the following:

    service ExampleService
    {
        ExampleResponse Method(ExampleRequest);
    }

`gbc` will produce stubs for gRPC with the `--grpc` flag:

    gbc c# --grpc example.bond

The service stub allows for the definition of the service implementation like this:

    public class ExampleService : ExampleServiceBase
    {
        public override async Task<IMessage<ExampleResponse>> Method(IMessage<ExampleRequest> param, ServerCallContext context)
        {
            ExampleRequest request = param.Payload.Deserialize();

            // Service business logic

            var response = new ExamplePingResponse();
            return Message.From(response);
        }
    }

The proxy stub allows the client to invoke the remote service method like this:

    var channel = new Channel("localhost", Port, ChannelCredentials.Insecure);
    var client = new ExampleClient(channel);

    var request = new ExampleRequest();
    IMessage<ExampleResponse> responseMessage = await client.Method(request);
    var response = responseMessage.Payload.Deserialize().Payload;

See also the following example:

- `examples/cs/comm/grpc_pingpong`
