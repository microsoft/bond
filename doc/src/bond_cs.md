% A Young Person's Guide to C# Bond

About
=====

Bond is an extensible framework for working with schematized data. It is 
suitable for scenarios ranging from service communications to Big Data storage 
and processing.

Bond defines a rich type system and schema versioning rules which allow
forward and backward compatibility. The core Bond features include high 
performance serialization/deserialization and a very powerful, generic data 
transform mechanism. The framework is highly extensible via pluggable 
serialization protocols, data streams, user defined type aliases and more.

By design Bond is language and platform independent and is currently supported 
for C++, C#, and Python on Linux, OS X and Windows.

Bond is published on GitHub at [https://github.com/Microsoft/bond/](https://github.com/Microsoft/bond/).

Basic example
=============

In Bond data schemas are defined using idl-like 
[syntax](compiler.html#idl-syntax):

    namespace Examples

    struct Record
    {
        0: string Name;
        1: vector<double> Constants;
    }

In order to use the schema in a C# program, it needs to be compiled using the
Bond compiler. This step is sometimes also referred to as code generation (or
codegen) because the compilation generates C# code corresponding to the schema
definition.

    gbc c# example.bond

Using the generated C# code, we can write a simple program that will
serialize and deserialize an instance of the Record schema using [Compact 
Binary](bond_cpp.html#compact-binary) protocol:

    namespace Examples
    {
        using Bond;
        using Bond.Protocols;
        using Bond.IO.Safe;

        class Program
        {
            static void Main()
            {
                var src = new Record
                {
                    Name = "FooBar",
                    Constants = { 3.14, 6.28 }
                };

                var output = new OutputBuffer();
                var writer = new CompactBinaryWriter<OutputBuffer>(output);

                // The first calls to Serialize.To and Deserialize<T>.From can take
                // a relatively long time because they generate the de/serializer 
                // for a given type and protocol.
                Serialize.To(writer, src);

                var input = new InputBuffer(output.Data);
                var reader = new CompactBinaryReader<InputBuffer>(input);

                var dst = Deserialize<Record>.From(reader);
            }
        }
    }

Code generation
===============

In order to use a Bond schema in a C# program, it needs to be compiled using the
Bond compiler [`gbc`](compiler.html). The compiler generates C# classes that
represent the schema. By default schema fields are represented by public
auto-properties initialized in the default constructor.

The mapping between Bond and C# type systems is mostly obvious but it is worth
noting that unlike C# reference types, Bond types are not nullable. This means,
for a example, that while `string` in Bond IDL will be mapped to C# `string`,
which is a reference type, the value `null` will not be valid. In order to allow
`null` value a types must be declared as
[`nullable`](bond_cpp.html#nullable-types), e.g.:

    struct Foo
    {
        0: list<nullable<string>> listOfNullableStrings;
    }

The value `null` is also legal for fields declared in Bond IDL to have a [default
of `nothing`](bond_cpp.html#default-value-of-nothing), e.g.:

    struct Bar
    {
        0: string str = nothing;
    }

Code generation can be customized by passing one or more of the following
command line options to `gbc`:

`--fields` 

Schema fields are represented by public fields with initializers and no 
constructor is generated.

`--readonly-properties`

Schema fields are represented by properties with public getter and private 
setter and initialized to the default values in the default constructor. 
Classes with read-only properties are fully supported by all Bond APIs.

`--collection-interfaces` 

Collection types `vector<T>`, `map<K, V>`, `list<T>` and `set<T>` are 
represented by respective generic collection interfaces: `IList<T>`, 
`IDictionary<K, V>`, `ICollection<T>` and `ISet<T>`.

Serializer
==========

Bond serialization API is provided by the `Serializer` class. It is a generic 
class parameterized with type of protocol writer used for serialization:

    Serializer<CompactBinaryWriter<OutputStream>>

The constructor of the `Serializer` class takes the type of a class or struct 
representing Bond schema:

    new Serializer<CompactBinaryWriter<OutputStream>>(typeof(Record))

The constructor is non-trivial so application usually should create an instance 
of `Serializer` outside of the inner loop and reuse it.

The `Serializer` class exposes one public method `Serialize` which takes two 
arguments, an object to be serialized and an instance of the protocol writer to 
be used for serialization.

    serializer.Serialize(obj, writer);

The object's type must be the same as the type passed to the `Serializer` 
constructor, otherwise the behaviour is undefined.

Bond provides a helper static API for applications that use schema types known 
at compile-time and don't need to manage lifetime of the `Serializer`:

    Serialize.To(writer, obj);

When the API is called for the first time, a static instance of appropriate 
`Serializer` is created. Because of this the first call to the API for given 
type and protocol may take relatively long time. Subsequent calls for the same 
writer/object types reuse the static instance.

Deserializer
============

Bond serialization API is provided by the `Deserializer` class. It is a generic 
class parameterized with type of the protocol reader to be used for 
deserialization:

    Deserializer<CompactBinaryReader<InputStream>>

The constructor of the `Deserializer` class takes the type of a class or struct
representing Bond schema:

    new Deserializer<CompactBinaryReader<InputStream>>(typeof(Record))

The constructor is non-trivial so application usually should create an instance 
of `Deserializer` outside of the inner loop and reuse it.

The `Deserializer` class exposes one generic public method `Deserialize` which 
takes as an argument an instance of the protocol reader and returns 
a deserialized object:

    var record = deserializer.Deserialize<Record>(reader);

The object created by `Deserialize` is always of the type specified during 
`Deserializer` construction. The type parameter of the method is only used to 
cast the result.

Deserializing from payload encoded in an untagged protocol like Simple usually 
requires specifying schema of the payload. To address this scenario the 
`Deserializer` class has an additional constructor taking 
a [`RuntimeSchema`](#runtime-schema) as an argument:

    RuntimeSchema schema;
    // ...
    var d = new Deserializer<SimpleReader<InputStream>>(typeof(Record), schema);
    var obj = d.Deserialize<Record>(reader);

An instance of `Deserializer` created this way is tied to the triplet of 
protocol, object type and payload schema. In order to deserialize from payload 
in another schema a new instance of `Deserializer` needs to be created.

Bond provides a helper static API for applications that use schema types known 
at compile-time, don't need to specify payload schema and don't need to manage 
lifetime of the `Deserializer`:

    var obj = Deserialize<Record>.From(reader);

When an application calls this API for the first time, a static instance of 
appropriate `Deserializer` is created. Because of this the first call to the 
API for given type and protocol may take relatively long time. Subsequent calls 
for the same reader/object types reuse the static instance.

See also the following example:

- `examples/cs/core/untagged_protocols`

Marshaling
==========

Since Bond supports multiple serialization 
[protocols](bond_cpp.html#protocols), application endpoints either have to 
agree on a particular protocol, or include protocol metadata in the payload. 
Marshaling APIs provide the standard way to do the latter, by automatically 
adding a payload header with the protocol identifier and version.

`Marshal` and `Unmarshal` APIs are very similar to `Serialize` and 
`Deserialize`, except that when calling `Unmarshal` the application simply 
provides an input stream with payload data, rather than an instance of a 
particular protocol reader:

    var src = new Example
    {
        Name = "foo",
        Constants = { 3.14, 6.28 }
    };

    var output = new OutputBuffer();
    var writer = new CompactBinaryWriter<OutputBuffer>(output);

    Marshal.To(writer, src);

    var input = new InputBuffer(output.Data);

    var dst = Unmarshal<Example>.From(input);

See also the following example:

- `examples/cs/core/marshaling`

Transcoder
==========

The `Transcoder` class provides API for converting payloads from one protocol 
into another. Transcoder operates directly on serialized data and doesn't need 
to know the C# type representing payload schema. 

The `Transcoder` is a generic class parameterized with types of source protocol 
reader and destination protocol writer, e.g.:

    Transcoder<CompactBinaryReader<InputStream>, SimpleWriter<OutputStream>>

The constructor of the `Transcoder` class takes as an optional argument the 
[runtime schema](#runtime-schema) of the payload. The argument is optional when 
transcoding between tagged protocols but must be specified when transcoding 
from an untagged protocol or to a text protocol.

    RuntimeSchema schema;
    // ...
    var t = new Transcoder<<SimpleReader<InputStream>, CompactBinaryWriter<OutputStream>>(schema);

The `Transcoder` constructor is non-trivial so application usually should 
create an instance of `Transcoder` outside of the inner loop and reuse it.

The `Transcoder` class exposes one public method `Transcode` which takes two 
arguments, protocol reader representing payload to transcode from, and protocol 
writer to be used to write the result.

    t.Transcode(reader, writer);

Bond provides a static helper API for application that don't need to explicitly 
manage `Transcoder` lifetime and don't specify payload schema or use a schema 
known at compile-time:

    // transcoding w/o schema
    Transcode.FromTo(reader, writer);

    // transcoding with compile-time schema Foo
    Transcode<Foo>.FromTo(reader, writer);

When an application calls this API for the first time, a static instance of 
appropriate `Transcoder` is created. Because of this the first call to the API 
for given type/schema and protocol may take relatively long time. Subsequent 
calls for the same reader/writer types reuse the static instance. Note that the 
static API can't be used if source protocol is untagged and will result in 
runtime exception.

See also the following example:

- `examples/cs/core/protocol_transcoding`

Input and output streams
========================

The input and output for binary protocols is provided by `IInputStream` and 
`IOutputStream` interfaces. Bond comes with standard implementations of these 
interfaces for memory buffers and `System.IO.Stream`, and applications can 
provide their own custom implementations.

The `OutputBuffer` class implements `IOutputStream` interface on top of 
a memory buffer. It comes in two variants. `Bond.IO.Safe.OutputBuffer` uses 
only safe managed code and is included in `Bond.dll` assembly which is 
compatible with Portable Class Library. `Bond.IO.Unsafe.OutputBuffer` uses 
unsafe code to optimize for performance. It is included in `Bond.IO.dll` 
assembly which requires full .NET runtime. Both implementations have identical 
class names and APIs, the only difference is the namespace in which they are 
defined.

    // Create an output buffer with initial size of 16KB
    var output = new OutputBuffer(16 * 1024);
    var writer = new CompactBinaryWriter<OutputBuffer>(output);

    Serialize.To(writer, obj);

    // Get the serialized payload form the output buffer
    ArraySegment<byte> data = output.Data;

The `InputBuffer` class implements `IInputStream` interface on top of a memory 
buffer. Like `OutputBuffer` it comes in two flavors, the safe and portable 
`Bond.IO.Safe.OutputBuffer`, and the performance optimized via use of unsafe 
code `Bond.IO.Unsafe.OutputBuffer`.

    // Create an input buffer on top of a byte[]
    var input = new InputBuffer(byteArray);
    var reader = new CompactBinaryReader<InputBuffer>(input);

The `InputStream` and `OutputStream` classes provide implementations of 
`IInputStream` and `IOutputStream` on top of `System.IO.Stream`. These classes 
are included in `Bond.IO.dll` and thus are only available to applications using 
full .NET runtime and allowing unsafe code. `In/OutputStream` can be used with 
any `Stream`, including `MemoryStream`, however aforementioned the 
`In/OutputBuffer` provide significantly better performance and are recommended 
when working with in-memory payloads.

    using (var stream = new FileStream("example.bin", FileMode.Open))
    {
        var input = new InputStream(stream);
        var reader = new CompactBinaryReader<InputStream>(input);
        var example = Deserialize<Example>.From(reader);
    }

Cloner
======

The `Cloner` class provides API for deep cloning of objects representing Bond 
schemas. Unlike typical cloning, Bond `Cloner` is not limited to making clones 
that are of the same type as the source object. The type of the source and the 
clone merely need to represent compatible Bond schemas.

The `Cloner` is a generic class parameterized with the source type and its 
constructor takes one argument representing the type of clones to be created 
e.g.:

    var cloner = new Cloner<Foo>(typeof(Bar));

The constructor is non-trivial so application usually should create an instance 
of `Cloner` outside of the inner loop and reuse it.

The `Cloner` exposes one public, generic method `Clone` which takes as the 
argument the source object and returns a clone:

    var clone = cloner.Clone<Bar>(foo);

The object created by `Clone` is always of the type specified during `Cloner` 
construction. The type parameter of the method is only used to cast the result. 

Bond provides a helper static API which creates and caches appropriate instance 
of `Cloner` the first time it is used, e.g.:

    var foo = new Foo();
    var clone = Clone<Bar>.From(foo);

See also the following example:

- `examples/cs/core/cloning`

Comparer
========

The `Comparer` class provides API for deep comparison for equality of objects 
representing Bond schemas. The class exposes one public, static, generic method 
`Equal` which takes two parameters representing objects to be compared.

    var left = new Foo();
    var right = new Foo();

    bool equal = Comparer.Equal(left, right);

Note that the `Comparer` doesn't compare arbitrary C# objects, it compares 
instances of Bond schemas. Only fields/properties decorated with Bond 
[attributes](#idattribute) and base classes/interface representing Bond 
[schemas](#schemaattribute) are considered during comparison.

Performance
===========

Bond offers very fast serialization and deserialization. Here are some tips on 
how to achieve the best performance. 

1. Explicitly create instances of `Serializer/Deserializer/Transcoder`

    Instead of using simplified APIs like `Serialize.To` and 
    `Deserialize<T>.From` it is usually better to explicitly instantiate and 
    cache appropriate `Serializer/Deserializer/Transcoder` objects. Creation of 
    these objects involves generation and JIT'ing of specific code to handle 
    the particular operation for a given schema type and protocol(s). This may 
    take a relatively long time, especially for large schemas, and usually it 
    is best to do it during program initialization. Once the object is created 
    it can be reused repeatedly and calling the Serialize/Deserialize/Transcode 
    methods will be very fast.

    ``` {.cs .numberLines}
    var exampleSerializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Example));
    var exampleDeserializer = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Example));

    var output = new OutputBuffer();
    var writer = new CompactBinaryWriter<OutputBuffer>(output);

    exampleSerializer.Serialize(src, writer);

    var input = new InputBuffer(output.Data);
    var reader = new CompactBinaryReader<InputBuffer>(input);

    var dst = exampleDeserializer.Deserialize<Example>(reader);
    ```

    Note that the type of `Serializer/Deserializer` doesn't depend on the 
    schema type so it is easy to cache these objects for multiple schemas used 
    in an application:

    ``` {.cs .numberLines}
    var serializerCache = new Dictionary<Type, Serializer<CompactBinaryWriter<OutputBuffer>>>
        {
            {
                typeof(Foo),
                new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Foo))
            },
            {
                typeof(Bar),
                new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Bar))
            }
        };
    ```

2. Prefer `InputBuffer` and `OutputBuffer` over `MemoryStream`

    When working with Bond payloads in a memory buffer (`byte[]` or 
    `ArraySegment<byte>`) Bond-defined `InputBuffer` and `OutputBuffer` classes 
    will provide significantly better performance than `InputStream` and 
    `OutputStream` used together with `System.MemoryStream`.

    `OutputBuffer` by default preallocates 64 KB of memory. When serializing small 
    objects the cost of allocating and zeroing the memory may dominate the actual 
    cost of serialization. Conversely, when serializing very large objects the 
    initial buffer of 64KB may be too small, leading to unnecessary reallocations 
    and memory copying.

    The `OutputBuffer` constructor accepts an argument specifying the size of 
    initial buffer in bytes. For optimal performance the size should be set to be 
    a little bigger than expect size of serialized data.

3. Prefer `using Bond.IO.Unsafe;` over `using Bond.IO.Safe;`

    Bond defines two variants of `InputBuffer` and `OutputBuffer` in two 
    namespaces `Bond.IO.Safe` and `Bond.IO.Unsafe`. The classes have identical 
    interface and can be used interchangeably. The only difference is that the 
    latter uses some low level memory access constructs and is implemented in 
    `Bond.IO.dll` assembly which is compiled with `/unsafe` flag. The unsafe 
    version is faster.

4. Pool memory buffers

    Creating a new `InputBuffer`/`OutputBuffer` every time may be more costly 
    than the actual serialization or deserialization and it increases GC 
    pressure. Whenever possible pool and reuse buffers, simply resetting their 
    position after or before use:

    ```
    buffer.Position = 0;
    ```
5. Choose the right protocol

    The Fast Binary protocol is a little faster than Compact Binary, although the
    difference is not big. Untagged protocols like Simple Binary can provide much
    better performance (up to 4 times faster for some schemas). They are most
    applicable in scenarios where runtime schema of the data is available during
    deserialization and the same schema applies to many instances of the data, so
    that the cost of creating the [`Deserializer`](#deserializer) can be
    amortized. The canonical use case for an untagged protocol is record-based
    data storage.

6. Using .NET 4.5 will give better performance than 4.0.

Runtime schema
==============

Some generic applications may need to work with Bond schemas unknown at 
compile-time. In order to address such scenarios Bond defines a type 
`SchemaDef` to represent schemas at runtime. Applications can obtain an 
instance of `SchemaDef` for a particular type using the `Schema` class:

    // from a type T
    var schema = Schema<T>.RuntimeSchema;

    // from type of an object
    var schema = Schema.GetRuntimeSchema(typeof(obj));

The APIs return an object of type `RuntimeSchema`, which is a thin wrapper over 
`SchemaDef`. Access to underlying schema is provided via public properties:

    var schemaDef = schema.SchemaDef

The `SchemaDef` object is always self contained, including the runtime schema 
definitions for all nested types (if any). The `RuntimeSchema` class instance 
can be constructed from a `SchemaDef`, and then it represents the whole schema, 
or from any embedded `TypeDef`:

    // runtime schema of Foo
    var schema = Schema<Foo>.RuntimeSchema;

    // runtime schema of the first field of Foo
    var fieldSchema = new RuntimeSchema(schema, schema.StructDef.fields[0].type);

`SchemaDef` is a Bond type, defined in `bond.bond`, and as such can be 
de/serialized like any other Bond type:

    Serialize.To(writer, Schema<T>.RuntimeSchema.SchemaDef);

A serialized representation of `SchemaDef` can be also obtained directly from
a schema definition IDL file using [bond compiler](compiler.html#runtime-schema).

See also the following example:

- `examples/cs/core/runtime_schema`

Understanding `bonded<T>`
=========================

The generic type `bonded<T>` is a simple yet powerful abstraction which is a 
fundamental part of Bond APIs and enables such usage scenarios as lazy 
deserialization, pass-through and polymorphism.

In C# `bonded<T>` maps to `IBonded<T>` interface which supports three 
operations: `Serialize`, `Deserialize` and `Convert`. Bond provides two 
standard implementation of the `IBonded<T>` interface, `Bonded<T>` which can 
hold and instance of type `T`, and `Bonded<T, R>` which can hold a serialized 
payload represented by a protocol reader `R`. The former is usually used by 
producers to initialize `bonded<T>` values, the latter is implicitly used 
during deserialization.

Lazy deserialization
--------------------

Because `bonded<T>` can store (or more accurately, refer to) data representing 
a serialized data, it can be used to de facto delay deserialization of some 
parts of payload:

    struct Example
    {
        0: Always always;
        1: bonded<Sometimes> sometimes;
    }

The schema defined above contains two nested fields. When an object of type 
`Example` is deserialized, the field `always` will be fully instantiated and 
deserialized, but field `sometimes`, which is declared as `bonded<Sometimes>`, 
will be merely initialized with a reference to its serialized representation. 
Application can then deserialize the object only when needed:

    var example = Deserialize<Example>.From(reader);

    // Deserialize sometimes only when needed
    if (needSometimes)
    {
        var sometimes = example.sometimes.Deserialize();
    }

Pass-through
------------

When `bonded<T>` containing a payload is serialized all the field from the 
original payload are preserved. This property very useful when building 
multi-stage service pipelines. Intermediary nodes often need to pass data 
through with full fidelity. At the same time, it is desirable that every schema 
change doesn't necessitate redeployment of all the nodes in a pipeline. Using 
`bonded<T>` for pass-through is often the right solution. 

As an example let's imagine a simple aggregator which receives responses from 
upstream services and aggregates top results.

    struct Upstream
    {
        0: bonded<Response> response;
        1: float ranking;
    }

    struct Aggregated
    {
        0: list<bonded<Response>> responses;
    }

Using `bonded<Response>` allows the intermediary to aggregate responses, 
preserving their full content, even if the aggregator doesn't use the same 
version of the `Response` schema as the upstream. 

    void ProcessResponse(Upstream upstream)
    {
        if (upstream.ranking > threshold)
        {
            aggregated.responses.Add(upstream.response);
        }
    }

Polymorphism
------------

The type parameter `T` in `IBonded<T>` interface is covariant which enables 
polymorphism. A `IBonded<Base>` can be initialized with an instance of 
`Bonded<Derived>`. For example, given the following schema:

    enum Kind
    {
        rectangle,
        circle,
        none
    }

    struct Shape
    {
        0: Kind kind = none;
    }

    struct Rectangle: Shape
    {
        0: int32 width;
        1: int32 height;
    }

    struct Circle : Shape
    {
        0: int32 radius;
    }

    struct Example
    {
        0: vector<bonded<Shape>> shapes;
    }

The type of the `shapes` field in C# class `Example` will be 
`List<IBonded<Shape>>` and an instance of the `Example` can be initialized as 
following:

    var src = new Example
    {
        shapes = 
        {
            new Bonded<Circle>(new Circle {kind = Kind.circle, radius = 10),
            new Bonded<Rectangle>(new Rectangle {kind = Kind.rectangle, width = 4, height = 5)
        }
    };

See also the following example:

- `examples/cs/core/polymorphic_container`

Custom type mappings
====================

Bond codegen provides a simple extensibility mechanism allowing use of custom 
C# types to represent types in a Bond schema. One common scenario is replacing 
the default collections with a different implementation that is semantically 
identical, e.g. `SortedSet<T>` instead of `HashSet<T>`. Custom type mappings 
can be also used to introduce completely new types which can be serialized as 
one of the built-in Bond schema types. For example time could be represented 
using the `DateTime` class and serialized as `int64`.

Defining a custom type mapping involves three steps:

- Define a [type alias](compiler.html#type-aliases) in the schema.
- Specify during codegen a C# type to represent the alias.
- Implement an appropriate converter for the custom C# type.

Codegen parameters
------------------

When generating code for a schema that uses [type 
aliases](compiler.html#type-aliases), the user can specify a custom type to 
represent each alias in the generated code:

    gbc c# --using="DateTime=System.DateTime" date_time.bond

The value of the `/using` parameter consists of one or more alias substitutions 
separated by semicolons, each in the following format:

    alias-name=generated-type-name

Custom containers
-----------------

Type aliases of container types can be mapped to user defined collection 
classes as long as they implement the same interfaces as the default 
collections: `IEnumerable<T>` as well as `ICollection<T>` for aliases of 
`list<T>` and `vector<T>`, `ISet<T>` for aliases of `set<T>` and 
`IDictionary<K, V>` for aliases of `map<K ,V>`. Custom mappings of container 
type aliases don't require a user defined converter.

- `examples/cs/core/container_alias`

Converter
---------

Applications using custom mappings for aliases of scalar types, string or blob 
must provide converter between the custom type and the default type. The 
converter is a public class named `BondTypeAliasConverter` defining a pair of 
public static `Convert` methods for each type alias:

    public static CustomType Convert(AliasedType value, CustomType unused)
    public static AliasedType Convert(CustomType value, AliasedType unused)

For example if `System.DateTime` was mapped to an alias of `int64` the 
following class could be defined to provide conversions between `DateTime` and 
`long` (the default type for `int64`):

    public static class BondTypeAliasConverter
    {
        public static long Convert(DateTime value, long unused)
        {
            return value.Ticks;
        }

        public static DateTime Convert(long value, DateTime unused)
        {
            return new DateTime(value);
        }
    }

The converter class must be defined in the same assembly and namespace as the 
class representing the Bond schema(s) in which the type alias is used or 
assembly/namespace of one of the types being converted.

- `examples/cs/core/date_time`
- `examples/cs/core/decimal`
- `examples/cs/core/guid`

Xml
===

Bond supports Xml serialization via Simple Xml protocol implemented by the 
`SimpleXmlReader` and `SimpleXmlWriter` classes:

    var stream = new System.IO.MemoryStream();
    var writer = new SimpleXmlWriter(stream);

    Serialize.To(writer, src);

    output.Flush();
    stream.Position = 0;

    var reader = new SimpleXmlReader(stream);

    var dst = Deserialize<Record>.From(reader);

In the example above the Xml reader and writer are constructed directly from an 
instance of `System.Stream`. Underneath however they use `System.Xml.XmlReader` 
and `System.Xml.XmlWriter` which provide fast, non-cached, forward-only Xml 
parsing and generation on top of many different data readers and writers. For 
example to deserialize Xml payload from a string:

    var xmlString = "<Record><Name>test</Name><Numbers><Item>3.14</Item></Numbers></Record>";

    var reader = new SimpleXmlReader(XmlReader.Create(new StringReader(xmlString)));
    var record = Deserialize<Record>.From(reader);

The Simple Xml protocol flattens the inheritance hierarchy, putting fields from 
base and derived classes together under a single element. In order to prevent 
name conflicts, Simple Xml protocol provides support for optional use of fully 
qualified schema names as field element namespaces, e.g.:

    <Derived xmlns:b="urn:Examples.Base" xmlns:d="urn:Examples.Derived">
        <d:Field>10</d:Field>
        <b:Field>foo</b:Field>
    </Derived>

Namespaces can be enabled when serializing to Xml via the `UseNamespaces` flag 
in SimpleXmlWriter.Settings:

    var writer = new SimpleXmlWriter(stream, new SimpleXmlWriter.Settings 
    {
        UseNamespaces = true
    });

There is no need to enable namespace support for the SimpleXmlReader. The 
elements representing fields are always matched against field names and their 
namespaces, if specified in the Xml document, against the qualified names of 
the containing structs.

Using Xml namespace inherently limits some of flexibility of Bond 
deserialization. In particular a document with namespaces can't be deserialized 
into a schema that is compatible but has a different name, for example 
a [view](compiler.html#struct-views) of the payload schema.

See also the following example:

- `examples/cs/core/simple_xml`

JSON
====

Bond supports JSON serialization via the Simple JSON protocol implemented by 
the `SimpleJsonReader` and `SimpleJsonWriter` classes. The JSON protocol 
depends on the Newtonsoft JSON parser and the classes are in a separate 
assembly `Bond.JSON.dll`.

    var stream = new System.IO.MemoryStream();
    var writer = new SimpleJsonWriter(stream);

    Serialize.To(writer, src);

    output.Flush();
    stream.Position = 0;

    var reader = new SimpleJsonReader(stream);

    var dst = Deserialize<Record>.From(reader);

In the example above the JSON reader and writer are constructed directly from 
an instance of `System.Stream`. Alternatively they can be also constructed from 
`System.IO.TextReader` and `System.IO.TextWriter`. For example to deserialize 
JSON payload from a string:

    var jsonString = "{Name: test, Numbers: [3.14]}";

    var reader = new SimpleJsonReader(new StringReader(jsonString));
    var record = Deserialize<Record>.From(reader);

The Simple JSON protocol flattens the inheritance hierarchy, putting fields 
from base and derived schemas together in the same JSON object. Name conflicts 
in the JSON representation between fields of base and derived schema can be 
resolved using `JsonName` schema field attribute:

    struct Base
    {
        0: string name;
    }
    
    struct Derived : Base
    {
        [JsonName("DerivedName")]
        0: string name;
    }

See also the following example:

- `examples/cs/core/simple_json`

Attributes
==========

Bond defines several attributes which are used to decorate user defined types 
with extra information required by Bond.

Schema decoration
-----------------

User defined types that represent Bond schemas and their members are decorated 
with following attributes. 

### SchemaAttribute ###

The `Schema` attribute is used to mark types that represent Bond schemas and 
thus can be used with Bond APIs. The attribute can be applied to classes, 
structs and interfaces. 

    [Bond.Schema]
    public class Foo {}

    [Bond.Schema]
    public struct Bar {}

    [Bond.Schema]
    public interface IFoo {}

The `Schema` attribute applies to the specific type only and is not inherited. 
When a class decorated with the `Schema` attribute derives from another class 
also decorated with the attribute then it represents Bond schema hierarchy. 
When a class representing schema derives from a class that is not marked with 
the `Schema` attribute then it represents a simple schema without a base, the 
C# base class is ignored by Bond.

When a C# class/interfaces representing a schema derives from multiple 
interfaces, at most one can be an interface representing a schema (i.e. 
decorated with `[Schema]` attribute).

### NamespaceAttribute ###

The `Namespace` attribute can be optionally used to annotate C# classes, 
interfaces and enums if their C# namespace is different than the schema 
namespace (i.e. namespace in .bond idl file), for example when C# code is 
generated with `--namespace` flag.

Bond will use the `Namespace` attribute, when present, to create qualified name 
of the type. If the attribute is absent the qualified name will use the C# 
namespace in which the type is defined.

### IdAttribute ###

All public fields and properties that represents fields of a Bond schema must 
be decorated with the `Id` attribute to specify the field's identifier (also 
called field ordinal). The ordinal value must an unsigned 16-bit integer, 
unique for each field within a type.

    [Bond.Schema]
    public class Foo
    {
        [Bond.Id(0)]
        public string message { get; set; }
    }

A type representing a schema may have additional fields/properties that don't 
represent schema fields and thus are not decorated with Bond attributes.

### RequiredAttribute ###

By default fields of Bond schemas are optional. [Required 
fields](bond_cpp.html#required-fields) must be marked with the `Required` 
attribute.

    /* Bond schema
    struct Foo
    {
        0: required string message;
    }
    */
    [Bond.Schema]
    public class Foo
    {
        [Bond.Id(0), Bond.Required]
        public string message { get; set; }
    }

### TypeAttribute ###

The `Type` attribute is used to provide additional type information about
schema fields. The attribute is optional because in most cases Bond can infer 
type from the C# type of field/property. For example C# type `short` always 
maps to Bond type `int16`. However not all C# types have an unambiguous mapping 
to Bond type system. For example C# `string` can represent either Bond type 
`string` or `wstring`. Similarly C# reference types which are always *nullable* 
can represent both *nullable* and *non-nullable* type in Bond schema. One of 
the uses for the `Type` is resolving such ambiguities. 

    using Bond.Tag;

    /* Bond schema
    struct Foo
    {
        0: nullable<wstring> str1;
        1: string str2 = nothing;
    } 
    */
    [Bond.Schema]
    public class Foo
    {
        [Bond.Id(0), Bond.Type(typeof(nullable<wstring>))]
        public string str1 = null;

        [Bond.Id(1)]
        public string str2 = null;
    }

Bond defines the following tag types that can be used in a `Type` attribute:

- `nullable`: specifies that a reference or nullable C# type represents 
  a *nullable* type in the Bond type system. 
- `wstring`: specifies that a string is UTF16 (i.e. `wstring` in the Bond 
  type system).
- `blob`: specifies that the type represents the schema type `blob`. 

The `Type` attribute can also be used to specify type of object to be created 
during deserialization when a field/property type is an interface.

    [Bond.Schema]
    public class Foo
    {
        [Bond.Id(0), Bond.Type(typeof(SortedSet<string>))]
        public ISet<string> strings1 { get; set; }

        [Bond.Id(1), Bond.Type(typeof(HashSet<string>))]
        public ISet<string> strings2 { get; set; }
    }

### DefaultAttribute ###

Bond infers default field values for classes and structs representing schemas 
from the field initializers or the class/struct constructor. For interfaces the 
default field values must be specified explicitly by decorating the interface 
properties with the `Default` attribute. The value specified in the attribute 
must be compatible with the field type, otherwise the behaviour is undefined.

    [Bond.Schema]
    public interface IFoo
    {
        [Bond.Id(0), Default("default value")]
        public string { get; set; }

        [Bond.Id(0), Default(3.14f)]
        public float { get; set; }
    }

The `Default` attribute is optional for properties that are decorated with 
`nullable` tag (the default is implicitly `null`). For non-nullable collections 
the `Default` attribute can either specify `null`, which means default of 
`nothing` in the schema, or it can be omitted, which means the default is an 
empty collection. Non-nullable properties of a type representing a schema have 
no concept of a default value thus the `Default` attribute is not applicable.

### AttributeAttribute ###

The `Attribute` attribute can be used to specify user defined attribute(s) for 
schemas, fields and enums.

    [Bond.Schema]
    [Bond.Attribute("name", "value")]
    public class Foo
    {
        [Bond.Id(0)]
        [Bond.Attribute("custom1", "value1")]
        [Bond.Attribute("custom2", "value2")]
        public string foo;
    }

Schema attributes are usually used by transforms to customize code generation
but they can also be accessed by applications via reflection.

Protocol decoration
-------------------

Bond defines several attributes that are used to decorate implementation of 
custom protocols with extra information.

### ReaderAttribute ###

The `Reader` attribute is used on a protocol writer implementation and 
specifies the type that implements the reader for that protocol.

    [Bond.Reader(typeof(SimpleXmlReader))]
    public struct SimpleXmlWriter : IProtocolWriter
    {
        // ...
    }

### ParserAttribute ###

The `Parser` attribute can be used on a protocol reader implementation and 
specifies the type of parser to be used for the protocol. It is optional for 
protocols that implement `IUntaggedReader` or `ITaggedReader` because they 
implicitly default to use `UntaggedParser` and `TaggedParser` respectively. 
When specified, the `Parser` attribute value must be a generic type definition 
with one type parameter, it must implement `IParser` interface and have two
public constructors, one accepting `RuntimeSchema` argument and one accepting 
`Type` argument (compile-time schema).

    [Bond.Parser(typeof(SimpleXmlParser<>))]
    public struct SimpleXmlReader : IXmlReader
    {
        // ...
    }

    public class SimpleXmlParser<R> : IParser
    {
        SimpleXmlParser(RuntimeSchema schema)
        {
            // ...
        }

        SimpleXmlParser(Type schema)
        {
            // ...
        }
    }

### SerializerAttribute ###

The `Serializer` attribute can be used on a protocol writer implementation to 
specify custom serializer for the writer. If the attribute is not specified 
then the default serializer implementation is used. The value of `Serializer` 
attribute must be a generic type definition with two type parameters `R` and 
`W`, it must implement `ISerializerGenerator<R, W>` interface and define two 
public constructors:

    [Bond.Serializer(typeof(CustomSerializer<,>))]
    public struct SimpleXmlWriter : IProtocolWriter
    {
        // ...
    }

    public class CustomSerializer<R, W> : ISerializerGenerator<R, W>
    {
        public CustomSerializer(Expression<Action<R, W, int>> deferredSerialize, RuntimeSchema schema)
        {
            // ...
        }

        public CustomSerializer(Expression<Action<R, W, int>> deferredSerialize, Type type)
        {
            // ...
        }
    }

References
==========

[Bond compiler reference][compiler]
---------------------------

[C++ User's Manual][bond_cpp]
---------------------------

[Python User's Manual][bond_py]
----------------------------

[compiler]: compiler.html

[bond_py]: bond_py.html

[bond_cpp]: bond_cpp.html

