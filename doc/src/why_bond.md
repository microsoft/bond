% Why Bond?

Bond is an [open source framework](https://github.com/microsoft/bond/) which
addresses similar scenarios to Protocol Buffers, Thrift and Avro. In this 
document we try to address similarities and differences between Bond and other 
solutions. We try to keep it objective and uncolored by opinions but inevitably 
our perspective is surely informed by specific use cases we most care about and 
the depth of knowledge we have of the various frameworks. If you find factual 
inaccuracies, or if you have written your own informed comparison and would 
like to include a link to it in this document, please send a pull request.

Meta schema
-----------

Bond has a very rich [type system](manual/compiler.html#idl-syntax). It is
probably most similar to Thrift in this respect. Some notable additions in Bond
are inheritance, [type aliases](manual/bond_cpp.html#type-aliases) and
[generics](manual/bond_cpp.html#generic-struct). One type system feature not
present in Bond that Avro, and recently Protocol Buffers, have are unions. Bond
uses schemas with optional fields to represent unions.

Programming model
-----------------

In terms of mapping to target languages, Bond again is much more similar to 
Thrift than Protocol Buffers. Like Thrift, Bond generates native types to 
represent schemas in the target language and uses native collections. Bond 
however doesn't hard-code type mappings. For example in C++ the defaults are 
STL containers like `std::vector` however user can easily map custom types 
(e.g. use `boost::multi_index_container`  in a generated C++ struct or map 
a `uint64` schema field to a `System.DateTime` field in a generated C# class). 
Bond generated C++ structs can also use custom allocators. See [custom type 
mappings](manual/bond_cpp.html#custom-type-mappings) for [more 
details](manual/bond_cs.html#custom-type-mappings).

Protocols
---------

Bond support three kinds of protocols. Tagged binary protocols are very similar 
to Thrift protocols and Protocol Buffers wire format. We use those in RPC 
scenarios because they don't require any schema pre-negotiation. Bond untagged 
protocols are like Avro wire format. The payload is compact because it doesn't 
contain any schema information, only data, but you need to provide schema of 
the payload at runtime in order to support schema versioning. We use these 
protocols in data storage scenarios, when many records using the same schema 
are stored in a file/stream. Finally Bond has first class support for text 
protocol like JSON and Xml.

In Bond, like in Thrift, protocols are pluggable. Where possible, Bond 
implements protocols through generics so that there is no performance overhead: 
neither C++ or C# implementation incurs virtual dispatch cost when calling 
protocol implementation.

Architecture
------------

One unique feature of Bond is that serialization and deserialization are not 
fundamental operations hard-coded in the generated code. In fact there is no 
code generated that is specific to serialization and deserialization. Instead 
Bond programming model exposes parsers and 
[transforms](manual/bond_cpp.html#transforms) which are composable by the user 
using meta-programming techniques. Some examples how this is used internally 
should give a taste of the flexibility of the architecture:

- Bond Python implementation doesn't involve *any* Python specific generated 
  code. It is fully built by driving Boost Python library using 
  meta-programming interfaces in Bond C++ implementation. A basic Python 
  [example](manual/bond_py.html#basic-example) is literally 7 lines of code.

- Bond can serialize and deserialize arbitrary instances of `std::tuple<T...>` 
  without any generated code. This is possible because serializer and 
  deserializer are constructed at C++ compile time and the C++ parameter pack 
  expansion effectively gives us compile-time C++ reflection for 
  [tuples](manual/bond_cpp.html#tuples).

- In C# implementation we have parsers for payload and for objects and we have 
  transforms that can consume data from a parser and serialize or deserialize 
  it. These primitives are internally composed into multiple high level APIs:

    - ObjectParser + Serializer -> Serialization
    - PayloadParser + Deserializer -> Deserialization
    - ObjectParser + Deserializer -> Cloning
    - PayloadParser + Serializer -> Transcoding
