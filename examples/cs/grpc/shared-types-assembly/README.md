# Using a shared types assembly

Sometimes you want to compile the types from a .bond file separately from
the gRPC services and clients. This example shows how to split the types
into their own assembly.

The [`gbc`](https://microsoft.github.io/bond/manual/compiler.html) options
`--structs` and `--grpc` can be used to control whether codegen is performed
for structs and services.

In the `types` directory, codegen is performed without the `--grpc` switch,
so just types are generated and compiled.

In the `client` and `server` directories, however, codegen is performed with
the `--grpc` switch. Also, `--structs=false` is passed to disable the
default behavior of generating C# code for the types. If `--structs=false`
were not specified, there would be duplicate types in different assemblies,
resulting in a conflict that would need to be resolved via
[`extern alias`](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/extern-alias).

The `client` directory shows how to pass `--gprc` via per-item metadata with
`%(BondCodegen.Options)`, while the `server` directory shows how to do this
via the global `$(BondOptions)` property.
