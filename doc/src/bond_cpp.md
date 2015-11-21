% A Young Person's Guide to C++ Bond

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

In Bond data schemas are defined using idl-like [syntax](compiler.html#idl-syntax):

    namespace example

    struct Record
    {
        0: string          name;
        1: vector<double>  items;
    }

In order to use the schema in a C++ program, it needs to be compiled using the
Bond compiler [`gbc`](compiler.html). This step is sometimes also referred to as 
code generation (or codegen) because the compilation generates C++ code 
corresponding to the schema definition.

    gbc c++ example.bond

Using the generated C++ code, we can write a simple program that will
serialize and deserialize an instance of the Record schema using [Compact 
Binary](#compact-binary) protocol:

    #include "example_reflection.h"

    #include <bond/core/bond.h>
    #include <bond/stream/output_buffer.h>

    int main()
    {
        example::Record src;

        src.name = "test";
        src.items.push_back(3.14);

        bond::OutputBuffer output;
        bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

        Serialize(src, writer);

        bond::InputBuffer input(output.GetBuffer());
        bond::CompactBinaryReader<bond::InputBuffer> reader(input);

        example::Record dst;

        Deserialize(reader, dst);

        return 0;
    }

Serialization
=============

The core feature provided by Bond is the ability to serialize and deserialize 
instances of user-defined schemas. The serialization APIs are declared in the 
`bond\core\bond.h` header file:

    template <typename T, typename Writer>
    void Serialize(const T& obj, Writer& output);

    template <typename T, typename Reader>
    void Deserialize(Reader input, T& obj);

    template <typename T, typename Reader>
    T Deserialize(Reader input);

The `Reader` and `Writer` template parameters specify the serialization 
protocol, and are one of the layers at which Bond serialization can be 
customized to meet applications' needs. The protocol defines how serialized 
data is encoded (e.g. binary, text). Bond ships with several [built-in 
protocols](#protocols) optimized for various scenarios, and also supports 
user-defined [protocols](#custom-protocols). By convention, protocol 
implementation is split between two classes implementing the reader, and the 
writer:

    template <typename Buffer>
    class CompactBinaryReader;

    template <typename Buffer>
    class CompactBinaryWriter;

The `Buffer` template parameter specifies where the serialized payload is 
respectively read from and written to. This constitutes the second layer of 
customization. Bond comes with built-in buffers implemented on top of memory 
[blobs][blob_reference], `InputBuffer` and `OutputBuffer`, but applications can 
also define [custom buffers](#custom-buffers) by implementing simple stream 
interfaces.

The full protocol class names can be unwieldy and it is often convenient to 
define shorter type aliases:

    typedef bond::InputBuffer Input;
    typedef bond::CompactBinaryReader<Input> Reader;
    typedef bond::OutputBuffer Output;
    typedef bond::CompactBinaryWriter<Output> Writer;

which then can be used throughout application code:

    Output output;
    Writer writer(output);
    Serialize(obj1, writer);

    Reader reader(output.GetBuffer());
    Deserialize(reader, obj2);

In storage scenarios, when untagged protocols, such as the [Simple 
Protocol](#simple-protocol), are used, applications need to specify the payload 
schema during deserialization. The deserialization API has an overloaded 
version to accommodate this usage:

    template <typename T, typename Reader>
    void Deserialize(Reader input, T& obj, const RuntimeSchema& schema);

    template <typename T, typename Reader>
    T Deserialize(Reader input, const RuntimeSchema& schema);

Typically the [runtime schema](#runtime-schema) is stored together with the 
data, for example in a system table or a header of a data file. Since Bond may 
need to access the runtime schema after the `Deserialize` function returns (to 
support [lazy deserialization](#lazy-deserialization)), it is recommended that 
applications manage lifetime of the schema object using smart pointers:

    boost::shared_ptr<bond::SchemaDef> schema(boost::make_shared<bond::SchemaDef>());

    // Deserialize the runtime schema
    bond::CompactBinaryReader<bond::InputBuffer> cb_reader(schema_data);
    bond::Deserialize(cb_reader, *schema);

    // Deserialize the object using the runtime schema
    bond::SimpleBinaryReader<bond::InputBuffer> simple_reader(object_data);
    Deserialize(simple_reader, obj, schema);

See examples:

- `examples/cpp/core/serialization`

Marshaling
==========

Since Bond supports multiple serialization [protocols](#protocols), application 
endpoints either have to agree on a particular protocol, or include protocol 
metadata in the payload. Marshaling APIs provide the standard way to do the 
latter, by automatically adding a payload header with the protocol identifier 
and version.

`Marshal` and `Unmarshal` APIs are very similar to `Serialize` and 
`Deserialize`, except that when calling `Unmarshal` the application simply 
provides an input stream with payload data, rather than an instance of a 
particular protocol reader:

        bond::OutputBuffer output;
        bond::CompactBinaryWriter<bond::OutputBuffer> writer(output);

        Marshal(src, writer);

        bond::InputBuffer input(output.GetBuffer());

        Unmarshal(input, dst);

See example: `examples/cpp/core/marshaling`.

Default value of `nothing`
==========================

Fields of a Bond defined struct always have a default value, either explicitly 
specified in the .bond file, or the implicit system default (`false` for 
`bool`, 0 for arithmetic types, empty for strings/containers and `null` for 
`nullable<T>`). Default values of fields matter because this is what an 
application will see after deserialization for any optional field that wasn't 
present in the payload (e.g. when the payload was created from an older version 
of the schema). Sometimes it is necessary to distinguish between any of the 
possible values of a field and absence of a value. To support such scenarios 
Bond allows fields' default values to be explicitly set to `nothing` [^1]:

    struct AboutNothing
    {
        0: uint16 n = nothing;
        1: string name = nothing;
        2: list<float> floats = nothing;
    }

Setting a field's default to `nothing` doesn't affect schema type of the field, 
however it may affect what type the field is mapped to in the generated code. 
The reason why is pretty obvious: some types such as `uint16_t` just can't 
represent absence of a value. In C++ fields with default of `nothing` always 
map to [`bond::maybe<T>`][maybe_reference]. In C# reference types already have 
a way to represent absence of value: `null`. For these types specifying default 
of `nothing` doesn't change the field type in the generated code. For C# value 
types such as `Int16`, generated code will use C# nullable type (e.g. 
`Int16?`).

The fact that setting default value of a field to `nothing` doesn't affect the 
field's schema type has an important consequence: the default value of the 
field doesn't have serialized representation. What this means in practice 
depends on whether the field is `optional` or `required`. Optional fields set 
to `nothing` are usually omitted during serialization [^2], just like for any 
other default values. [Required fields](#required-fields), by definition, can 
never be omitted. Since `nothing` has no serialized representation, an attempt 
to serialize an object with required fields set to `nothing` will result in a 
runtime exception. If a null value needs to be represented in the serialized 
form, then default of `nothing` is a wrong choice and a [nullable 
type](#nullable-types) should be used instead.


[^1]: In Bond there is no concept of default for structs and thus default of
`nothing` can't be set for fields of struct types or `bonded<T>`.

[^2]: Some protocols might not support omitting optional fields (e.g. Simple
Protocol) or omitting fields may be disabled by specializing
`bond::may_omit_fields` trait. In such cases an attempt to serialize an object
with field(s) set to `nothing` will result in a runtime exception.


Nullable types
==============

For any type in Bond meta-schema `nullable<T>` defines a nullable type. A 
nullable type can store all the same values as the its base type plus one 
additional value: `null`.

    struct Nullables
    {
        0: nullable<bool>         b;
        1: list<nullable<string>> l;
    }

Default value for a field of a nullable type is always implicitly set to 
`null`. Explicit default values for nullable fields are not supported.

In generated C++ code nullable types are represented by
[`nullable<T>`][nullable_reference] class template.

Since a nullable type must represent the additional value of `null`, its 
serialized representation necessarily incurs some overhead compared to the base 
type. Often it is more efficient to avoid using nullable type and instead 
designate one of normal values to handle the special case that otherwise would 
be represented by `null`. For example _empty_ is usually a good choice for 
string and container types and 0 for arithmetic types. Another option that may 
sometimes be appropriate is setting default value of a field to 
[`nothing`](#default-value-of-nothing).

The canonical scenario where a nullable type is the right choice is recursive 
structures. For example here's how Bond `TypeDef` struct is defined:

    struct TypeDef
    {
        // Type identifier
        0: BondDataType id = BT_STRUCT;

        // Index of struct definition in SchemaDef.structs when id == BT_STRUCT
        1: uint16 struct_def = 0;

        // Type definition for:
        //  list elements (id == BT_LIST),
        //  set elements (id == BT_SET),
        //  or mapped value (id == BT_MAP)
        2: nullable<TypeDef> element;

        // Type definition for map key when id == BT_MAP
        3: nullable<TypeDef> key;

        // True if the type is bonded<T>; used only when id == BT_STRUCT
        4: bool bonded_type;
    }

`TypeDef` struct is used to represent type of a field in a Bond schema. If the 
type is a container such as list or map, the type definition becomes recursive. 
For example, a list type definition contains type of the list element which of 
course itself can be a container of elements of some other type, and so on, 
until recursion is terminated with a `null` value for the `element` and `key` 
fields.

Runtime schema
==============

Some generic applications may need to work with Bond schemas unknown at 
compile-time. In order to address such scenarios Bond defines a type 
`SchemaDef` to represent schemas at runtime. Applications can obtain an 
instance of `SchemaDef` for a particular type using the `GetRuntimeSchema` API:

    // from type T
    auto schema = bond::GetRuntimeSchema<Example>();

    // from an instance
    auto schema = bond::GetRuntimeSchema(obj);

The value returned by `GetRuntimeSchema` is of type `bond::RuntimeSchema`, 
which is a thin wrapper over `SchemaDef`. The runtime schema object returned by 
the API is always self contained, including the runtime schema definitions for 
all nested types (if any). Note that GetRuntimeSchema returns a static object 
and can't be called during module initialization (e.g. from a constructor of 
a static object).

`SchemaDef` is a Bond type, defined in `bond.bond`, and as such can be 
de/serialized like any other Bond type:

    Serialize(bond::GetRuntimeSchema<Example>(), writer);

A runtime schema is often used to describe the schema of a serialized payload, 
in particular when using an untagged [protocol](#protocols):

    bond::SimpleBinaryReader reader(dataPayload);
    auto schema = boost::make_shared<bond::SchemaDef>();

    Unmarshal(schemaPayload, *schema);
    Deserialize(reader, obj, schema);

The Deserialize API in the above code snippet is a thin wrapper around the 
generic way to describe payload with a runtime schema: `bonded<void>`:

    bond::bonded<void> data(reader, schema);

A `bonded<void>` object can be used like any 
[`bonded<T>`](#understanding-bondedt), e.g. it can be serialized/transcoded:

    Serialize(data, writer);

When an application creates an instance of `SchemaDef` in order to use it with 
Bond APIs, it is strongly recommended to always dynamically allocate the object 
and wrap it in a `boost::shared_ptr`, like in the example above. There are some 
subtle cases when Bond may need to keep a reference to the schema beyond the 
scope where it was created, and using a `shared_ptr` provides a safe and 
efficient way to achieve this. In order to encourage this, the 
`bond::RuntimeSchema` wrapper used by many Bond APIs is implicitly 
constructable from `boost::shared_ptr<SchemaDef>` but only explicitly from 
`const SchemaDef&`. 

A serialized representation of `SchemaDef` can be also obtained directly from
a schema definition IDL file using [bond compiler](compiler.html#runtime-schema).

See example: `examples/cpp/core/runtime_schema`.

Compile-time schema
===================

Bond generated C++ classes define a nested struct called `Schema` which 
describes the type's schema. It is called compile-time schema because it 
supports reflection on the schema during C++ compilation, enabling various 
meta-programming techniques. The struct `Schema` has the following members:

- `base`

    A typedef which is defined to either `bond::no_base` or the compile-time 
    schema of the base schema.

- `metadata`

    A static data member of type `bond::Metadata` (defined in `bond::bond`). 
    Describes schema's metadata, such a name and optional attributes.

- `var`

    A struct with a typedef member for each schema field. The typedefs have the 
    same name as the fields, e.g. `Example::Schema::var::m_example` represents 
    a field `m_example` of class `Example`. The typedefs are defined to 
    instances of [`bond::reflection::FieldTemplate`][field_template_reference] 
    template.

- `fields`

    A `boost::mpl::list` of the fields. The elements of the list are typedef 
    memebers of the `var` struct described above.

Compile-time schema can also be defined in a non-intrusive way, e.g. for 
classes that can't be modified, by specializing `schema` meta-function:

    template <typename T, typename Enable = void> struct
    schema;

For an example see the compile-time schema definition for `std::tuple<T...>` in 
`bond/core/tuple.h`.

Code examples:

- `examples/cpp/core/compile_time_schema`
- `examples/cpp/core/polymorphic_container_visitor`

Understanding `bonded<T>`
=========================

The generic type `bonded<T>` is a simple yet powerful abstraction which is a 
fundamental part of Bond APIs and enables such usage scenarios as lazy 
deserialization, protocol transcoding, pass-through and polymorphism.

Fundamentally, `bonded<T>` is used to represents a struct data, but it is much 
more versatile than a simple struct instance. Before we explore its 
capabilities, let's first look at C++ declaration of [`bonded` 
class][bonded_reference]:

    template <typename T, typename Reader = ProtocolReader<T, InputBuffer>>
    class bonded;

The class template has two parameters. The first one, `T`, represents schema 
(or type) of the data. Usually it is a struct defined via Bond 
[IDL](compiler.html#idl-syntax) but it can also be `void` (see 
[`bonded<void>`][bonded_void_reference]) if we want to work with data for which 
schema is not known at compile-time. The second parameter, `Reader`, specifies 
representation of the data. The default, `ProtocolReader` is a variant type 
which can hold data serialized using any of the Bond protocols, or an instance 
of struct `T`.

The `bonded` class defines several constructors which allow creation of 
`bonded` objects from following inputs:

- Instance of `T` or a type derived from `T` [^slicing]

<!-- -->

    using bond::bonded;

    MyStruct obj;

    // Copy obj by value
    bonded<MyStruct> b1(obj);

    // Store reference to obj
    bonded<MyStruct> b2(boost::ref(obj));

    // Store shared_ptr to object
    auto ptr = boost::make_shared<MyStruct>();
    bonded<MyStruct> b3(ptr);

- Serialized payload

<!-- -->

    bond::CompactBinaryReader<bond::InputBuffer> reader(payload);

    bonded<MyStruct> b4(reader);

    bonded<void> b5(reader, schema);

- Compatible instance of `bonded`

<!-- -->

    // Implicit up-casting
    bonded<MyStructBase> b6(b4);

    // Explicit down-casting
    bonded<MyStruct> b7(b6);

    // Explicit cast to bonded<void>
    bonded<void> b8(b7);

    // Explicit cast from bonded<void>
    bonded<MyStruct> b9(b8);

APIs associated with `bonded<T>` are very simple. Given an instance of 
`bonded<T>` we can essentially perform two operations on the contained data:

- Deserialize an object from the data
- Serialize the data using a protocol writer

Versatility of `bonded<T>` comes from how these two operations apply to various 
kinds of data.

[^slicing]: An object of a derived type will be sliced to the type `T`.

Lazy deserialization
--------------------

Because `bonded<T>` can store (or more accurately, refer to) data representing 
a serialized struct, it can be used to de facto delay deserialization of some 
parts of payload:

    struct Example
    {
        0: Always            m_always;
        1: bonded<Sometimes> m_sometimes;
    }

The schema defined above contains two nested fields. When an object of type 
`Example` is deserialized, the field `m_always` will be fully instantiated and 
deserialized, but field `m_sometimes`, which is declared as 
`bonded<Sometimes>`, will be merely initialized with a reference to its 
serialized representation [^bonded_cost].

    Example example;

    Deserialize(reader, example);

    // Deserialize m_sometimes only when needed
    if (needSometimes)
    {
        Sometimes sometimes;

        example.m_sometimes.Deserialize(sometimes);
    }

[^bonded_cost]: Cost of deserializing `bonded<T>` is protocol dependent. Most 
protocols need to parse the payload in order to find where the corresponding 
struct ends. Compact Binary version 2 stores length prefix for structs and thus 
can deserialize a `bonded<T>` field in constant time.

Protocol transcoding
--------------------

If `bonded<T>` contains data representing a serialized struct, what does it 
mean to serialize it? The answer to this questions is the key to understanding 
the power and versatility of `bonded<T>`. When serializing a `bonded<T>` 
object, Bond will iterate through the serialized data, decode each field and 
write it to target protocol writer
[^same_protocol_opt]. What's more, if the source data is encoded using a tagged 
protocol (e.g. Compact Binary) Bond doesn't depend on definition of struct `T` 
to know what fields the payload contains. The payload is self-described and 
Bond is able to preserve all fields, even those that are not part of struct `T` 
(e.g. because the payload was created using a newer version of the schema). In 
fact `bonded<T>` can be serialized *even* if definition of type `T` is not 
known!  

    // Declare Unknown; actual definition is not needed
    struct Unknown;

    // Transcode data from Compact Binary to JSON
    bond::CompactBinary<bond::InputBuffer> reader(data);
    bond::bonded<Unknown> payload(reader);

    bond::OutputBuffer json;
    bond::SimpleJsonWriter<bond::OutputBuffer> writer(json);

    Serialize(payload, writer);

The sample code above would preserve all the fields from the source data, 
however it would not preserve the field names, producing JSON output looking 
something like this:

    {
        "10": "Sample Konfabulator Widget",
        "30": 500,
        "40": 500
    }

The output is sufficient to deserialize the object using Bond, but it is not 
particularly human-readable. If we wanted to preserve fields names in JSON 
output, we would need to specify the payload schema, by using either `bonded` 
of a defined Bond struct rather than `Unknown`, or `bonded<void>` with a schema 
provided at runtime.

    // Transcode data from Compact Binary to JSON using runtime schema
    bond::CompactBinary<bond::InputBuffer> reader(data);
    bond::bonded<void> payload(reader, schema);

    bond::OutputBuffer json;
    bond::SimpleJsonWriter<bond::OutputBuffer> writer(json);

    Serialize(payload, writer);

The `schema` object in the example above is an instance of 
[`bond::SchemaDef`](#runtime-schema). With full schema information transcoded 
JSON output will be more human-friendly:

    {
        "title": "Sample Konfabulator Widget",
        "width": 500,
        "height": 500
    }

See also: `examples/cpp/core/protocol_transcoding`

[^same_protocol_opt]: As an optimization, if the data is already encoded in the 
target protocol Bond will simply copy the payload.

Pass-through
------------

The fact that `bonded<T>` preserves unknown fields is very useful when building 
service pipelines. Intermediary nodes often need to pass data through with full 
fidelity. At the same time, it is desirable that every schema change doesn't 
necessitate redeployment of all the nodes in a pipeline. Using `bonded<T>` for 
pass-through is often the right solution. 

As an example let's imagine a simple aggregator which receives responses from 
upstream services and aggregates top results.

    struct Response;

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
preserving their full content, even though schema of `Response` is not known 
when the aggregator is built, and thus it doesn't need to be rebuilt or 
redeployed when schema of `Response` changes.

    void ProcessResponse(const Upstream& upstream)
    {
        if (upstream.ranking > threshold)
        {
            m_aggregated.responses.push_back(upstream.response);
        }
    }

Polymorphism
------------

Bond support for polymorphism is built around the capability of `bonded<T>` to
contain serialized struct data not limited to just fields of struct `T`. In 
particular `bonded<Base>` can contain serialized data for some struct `Derived` 
which inherits from `Base`. Together with the ability to down-cast 
`bonded<Base>` to `bonded<Dervied>`, this enables use of Bond schemas 
supporting serialization of polymorphic objects.

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
        0: list<bonded<Shape>> shapes;
    }

For details on implementing polymorphism see the following examples:

- `examples/cpp/core/polymorphic_container`
- `examples/cpp/core/polymorphic_container_visitor`

Bonus explainer: parallels between `bonded<T>` and C++ pointers
---------------------------------------------------------------

The rules of casting and slicing that apply to `bonded<T>` are by design very 
similar to the standard C++ rules for pointers:

+---------------------------+---------------------------------+-----------------------------------+
|                           | bonded\<T\>                     | C++ pointer                       |
+===========================+=================================+===================================+
|                           | ```cpp                          | ```cpp                            |
| Slicing to base           | bonded<Derived> b;              | Derived* p;                       |
|                           | Base obj;                       | Base obj;                         |
|                           | b.Deserialize(obj);             | obj = *p;                         |
|                           | ```                             | ```                               |
+---------------------------+---------------------------------+-----------------------------------+
|                           | ```cpp                          | ```cpp                            |
| Assigning to base part    | bonded<Base> b;                 | Base* p;                          |
|                           | Derived obj;                    | Derived obj;                      |
|                           | b.Deserialize(obj);             | static_cast<Base&>(obj) = *p;     |
|                           | ```                             | ```                               |
+---------------------------+---------------------------------+-----------------------------------+
|                           | ```cpp                          | ```cpp                            |
| Implicit up-casting       | void foo(bonded<Base>);         | void foo(Base*)                   |
|                           | bonded<Derived> b;              | Derived* p;                       |
|                           | foo(b);                         | foo(p);                           |
|                           | ```                             | ```                               |
+---------------------------+---------------------------------+-----------------------------------+
|                           | ```cpp                          | ```cpp                            |
| Explicit down-casting     | bonded<Base> b;                 | Base* p;                          |
|                           | Derived obj;                    | Derived obj;                      |
|                           | bonded<Derived>(b)              | obj = *static_cast<Derived*>(p);  |
|                           |     .Deserialize(obj);          |                                   |
|                           | ```                             | ```                               |
+---------------------------+---------------------------------+-----------------------------------+
|                           | ```cpp                          | ```cpp                            |
| Implicit cast to void     | void foo(bonded<void>);         | void foo(void*);                  |
|                           | bonded<Bar> b;                  | Bar* p;                           |
|                           | foo(b);                         | foo(p);                           |
|                           | ```                             | ```                               |
+---------------------------+---------------------------------+-----------------------------------+
|                           | ```cpp                          | ```cpp                            |
| Explicit cast from void   | bonded<void> b(data, schema);   | void* p = &obj                    |
|                           | bonded<Bar> bb;                 | Bar* pp;                          |
|                           | bb = bonded<Bar>(b);            | pp = static_cast<Bar*>(p);        |
|                           | ```                             | ```                               |
+---------------------------+---------------------------------+-----------------------------------+


Merge
=====

An important feature of Bond is the ability to preserve unknown fields 
([Pass-through](#pass-through)), or even whole parts of the inheritance 
hierarchy ([Polymorphism](#polymorphism)), when transcoding or forwarding 
serialized Bond payloads. The merge feature builds on these capabilities by 
allowing modifications to the known part of a structure and merging them with 
any unknown parts present in the payload.

The `Merge` API takes as input an instance of a Bond object and a serialized 
payload and writes the merged result to the specified protocol writer:

    template <typename T, typename Reader, typename Writer>
    void Merge(const T& obj, Reader input, Writer& output);

The `Reader` and `Writer` can be for different protocols (in other words 
merging can transcode payload at the same time). The type `T` of the object is 
usually somehow related to the schema of the input payload (e.g. a different 
version of the schema or its base). In the typical usage scenario, the object 
is first deserialized from the payload, and then modified and merged with 
original payload.

The `Merge` API is a thin wrapper around the `Merger` transform which can be 
applied to a [`bonded<T>`](#understanding-bondedt) in order to merge its 
payload with an instance of T, e.g.:

    typedef bond::CompactBinaryWriter<bond::OutputBuffer> Writer;
    T obj;
    bond::bonded<T> payload;
    // ...
    Writer output(buffer);
    Apply(bond::Merger<T, Writer>(obj, output), payload);

For fields that are present in the object's schema, the value in the object is
serialized, otherwise the value for the unknown field in the payload is
preserved. Merge works recursively, merging bases and any nested structures,
including structures that are elements of a container. When merging containers
containing structures, lists and vectors must have the same number of elements
and maps must have the same set of keys in both the object and in the payload,
otherwise Merge throws an exception. Containers with non-struct types as
elements are treated like regular fields.

Fields and containers of type `bonded<T>` are not merged and instead the value 
from the object is written to the output. This allows applications to 
selectively merge individual `bonded<T>` fields or elements, enabling, for 
example, merging of containers when elements have been added or removed. 

The helper method Merge supports the common scenario where the result of merge 
is put back as payload of `bonded<T>`:

    template <typename T>
    template <typename X>
    void bonded<T>::Merge(const X& var);

See example: `examples/cpp/core/merge`.


Required fields
===============

By default, struct fields in Bond schemas are considered optional. This means 
that during deserialization, if the payload doesn't contain a field, Bond will 
just use the field's default value specified in the schema. Consequently, 
during serialization optional fields which are set to their default value _can_ 
be omitted, resulting in a more compact payload. This behavior is fundamental 
to enabling forward and backward compatibility between different schema 
versions. As long as fields are optional, they can be freely added and removed, 
without breaking the ability of the old code to deserialize the new data and 
vice versa. In distributed systems, where we usually can't depend on deployment 
order, this two-way compatibility is a critical feature. This is why optional 
fields are the implicit default in Bond, and why avoiding required fields is 
generally considered a good rule of thumb.

Required fields can be declared using the following syntax. 

    struct Example
    {
        0: required int32 field;
    }

Required fields _must_ be present in the payload during deserialization 
(otherwise an exception is thrown) and consequently they are always written to 
the payload during serialization. 

When should you use required fields? One way to think about this is that 
required fields are a way to _explicitly break_ schema compatibility. 
Essentially, a schema with required field(s) means: a consumer using this 
schema is incompatible with any version of the schema, past or future, that 
doesn't have the required field(s). In cases when the semantics of a field are 
such that the schema doesn't make sense without it, declaring the field as 
required might be the right thing to do.

Specifying a field as required doesn't mean that it can _never_ be removed from 
the schema, but it does mean that _all_ existing consumers using the schema 
will have to be updated first. In order to enable adding/removing required 
fields (or converting optional fields to required and vice versa), Bond 
supports an intermediary state for fields called `required_optional`. Fields 
that are marked as `required_optional` behave like required fields during 
serialization (i.e. their value is always included in the payload) and like 
optional fields during deserialization (i.e. if the field is not in the 
payload, Bond will use the default value). This allows non-breaking, 
deployment-order-independent schema changes that involve `required` fields. 

**required <----> required_optional <----> optional**

Changes involving `required` fields take two steps. First the schema is updated 
to use a `required_optional` field (i.e. a new `required_optional` field is 
added or an existing `required` or `optional` field is converted to 
`required_optional`). These changes are non-breaking and can be deployed in any 
order. Once all programs using the updated schema are deployed, in the second 
step the `required_optional` field can be removed or converted to `required` or 
`optional` as desired. Again these changes are non-breaking.

Tuples
======

Bond can de/serialize instances of std::tuple<T...> as if they were regular 
Bond-defined structs. For example the following tuple instance:

    std::tuple<std::string, double, std::vector<uint32_t>>

is equivalent to this Bond schema:

    struct tuple
    {
        0: string item0;
        1: double item1;
        2: vector<uint32> item2;
    }

Field ordinals used for Bond serialization are the same as tuple item 
identifiers used with std::get<N> function. Field names are `item`*N* where is 
*N* is the item identifier. The schema/struct name for a tuple instance is 
`tuple<`*parameters*`>`.

Since field ordinals are implicitly assigned based on the order in which items 
are defined, tuples don't offer the same versioning flexibility as explicitly 
defined Bond schemas. In particular, adding or removing any tuple item other 
than the last is a breaking change because it offsets ordinals of all 
subsequent items.

Tuple instances can be used with all Bond APIs that accept regular Bond defined 
structs, e.g:

    auto obj = std::tuple<string, double>;

    Serialize(obj, writer);
    Deserialize(reader, obj);
    auto schema = bond::GetRuntimeSchema(obj);

Bond provides helper functions `Pack` and `Unpack` which can be used
respectively to create/serialize a tuple from several values and deserialize 
struct fields into several variables.

    std::string str;
    Pack(writer, str, 10);

    int n;
    std::string str2;
    Unpack(reader, str2, n);

If the payload contains more fields than variables provided to `Unpack` the 
tail fields are ignored. The special object `std::ignore` can be passed as an 
argument to `Pack` and `Unpack` in order to ignore field(s) at particular 
position(s). For example:

    Pack(writer, std::ignore, 10);

will serialize a struct/tuple containing one field with ordinal 1 and type 
`int`. Similarly:

    Unpack(reader, std::ignore, n);

will ignore the field with ordinal 0, if any, and deserialize the field with 
ordinal 1 into the variable `n`.

Examples:

- `examples/native/core/trace`
- `examples/native/core/variadic`

Transforms
==========

Bond transform are a powerful mechanism which enables writing generic, 
schema-independent, type-safe and high performance code operating on instances 
of Bond generated classes and their serialized representation. As transforms 
are flexible and have good performance characteristics, the core APIs like 
`Serialize` and `Deserialize` are in fact implemented as applications of 
transforms.

    // Serialize and Deserialize APIs as defined in bond.h

    template <typename T, typename Writer>
    inline void Serialize(const T& obj, Writer& output)
    {
        Apply(Serializer<Writer>(output), obj);
    }

    template <typename T, typename Reader>
    inline void Deserialize(Reader input, T& obj)
    {
        Apply(To<T>(obj), bonded<T, Reader&>(input));
    }

Transforms are an instance of the visitor pattern. A transform class implements 
methods which are called by a Bond parser for the fields of a Bond type 
instance or its serialized representation. The name _transform_ comes from the 
fact that they usually perform transformation of Bond objects or payloads. For 
example the `Serializer` transform can be applied to an object and output its 
serialized representation, or it may be applied to a payload encoded in one 
protocol and transcode it into another protocol.

Transforms are applied using the `bond::Apply` API. The first argument to 
`Apply` is always a const reference to an instance of transform class, and the 
second argument is an object the transform should be applied to. There are 
essentially three overloads of `Apply` API [^apply_overloads]:

    template <typename Transform, typename T, typename Reader>
    bool Apply(const Transform& transform, const bonded<T, Reader>& bonded);

    template <typename Transform, typename T, typename Reader>
    void Apply(const Transform& transform, const value<T, Reader>& value);

    template <typename Transform, typename T>
    bool Apply(const Transform& transform, T& value);

[^apply_overloads]: Reviewing `apply.h` will reveal that in fact there are a 
few more overloads of `Apply`, and their signatures are more complex. However 
these can be considered implementation details, only defined to enable the 
compiler to select of the proper implementation in various scenarios. 
Conceptually, the `Apply` API can be called to apply a transform to one of 
three _things_: `bonded<T>`, `value<T>` or an instance of a Bond class.

A transform class must inherit from one of the following classes:

- [`bond::SerializingTransform`][serializing_transform_ref]
- [`bond::DeserializingTransform`][deserializing_transform_ref]
- [`bond::ModifyingTransform`][modifying_transform_ref]

The base indicates the type of transformation performed by the class, and what 
kind of object it can be applied to. A serializing transform can be applied to 
an object or a serialized payload and usually outputs serialized payload. A 
deserializing transform can only be applied to a serialized payload. A 
modifying transform can only be applied to a non-const object and usually 
modifies the object.

Transform concept
-----------------

A transform class has to implement the following concept:

    struct Transform
    {
        // All transforms
        void Begin(const bond::Metadata& metadata) const;

        void End() const;

        template <typename T>
        bool Base(const T& value) const;

        template <typename T>
        bool Field(uint16_t id, const bond::Metadata& metadata, T& value) const;

        // Only serializing and deserializing transforms
        void UnknownEnd() const;

        template <typename T>
        bool UnknownField(uint16_t id, T& value) const;

        bool OmittedField(uint16_t id, const bond::Metadata& metadata, bond::BondDataType type) const;

        // Only serializing transforms
        template <typename T>
        void Container(const T& element, uint32_t size) const;

        template <typename Key, typename T>
        void Container(const Key& key, const T& value, uint32_t size) const;
    };


The type `T` of the values visited by a transform depends on what the transform 
is applied to. If it is applied to an instance of a Bond type, the visited 
values will be references to the object's fields and/or its base object (if 
any). If a transform is applied to a serialized payload, the visited values 
will represent the serialized fields, elements of a container and/or base (if 
any). The serialized data is represented by one of two types: `bond::bonded<T, 
Reader>` or `bond::value<T, Reader>`. The former represents a serialized Bond 
object and the latter a serialized value of a basic type or a container. 

A transform can generally do one of two things with the serialized values:

1. Deserialize using the Deserialize method

<!-- -->

    template <typename T, typename Reader>
    typename boost::enable_if<bond::is_basic_type<T>>::type
    Field(uint16_t, const bond::Metadata&, const bond::value<T, Reader>& value) const
    {
        T x;
        value.Deserialize(x);
        return false;
    }

2. Recursively apply the transform

<!-- -->

    template <typename T>
    bool Base(const T& value) const
    {
        return Apply(MyTransform(), value);
    }

Recursive application of transforms is a key technique which enables 
transformations of arbitrary complex/nested schemas/containers. By creating and 
applying an new instance of its class, a transform class can easily implement 
management of per-hierarchy-level state. For example a transform creating a 
text output such as XML, could store the indentation level in its data member 
and create a new instance with increased indentation to be applied to nested 
fields.

Transforms are a deep topic and by necessity this article only touches on the 
most basic concepts. Users writing their own custom transforms are encouraged 
to study the implementation of built in transforms in 
[transfroms.h][transforms_h], in particular the `Serializer<Writer>` and 
`To<T>` transforms.

Code examples:

- `examples/cpp/core/transform`
- `examples/cpp/core/modifying_transform`
- `examples/cpp/core/access_control`

Reflection
==========

See [compile-time schema](#compile-time-schema) and [transforms](#transforms).

Protocols
=========

Bond protocols are pluggable, allowing application to choose the most
appropriate encoding format. Bond supports three kinds of protocols:

  - Tagged protocols

    Tagged protocols interleave schema metadata within the payload. This makes
    the payload self-describing, allowing consumers to interpret it even
    without knowing the schema used by the producer.

  - Untagged protocols

    Untagged protocols serialize only data and thus require that consumers know 
    the payload schema via some out-of-band mechanism. Untagged protocols are 
    often used in storage scenarios because they allow storing a 
    [schema](#runtime-schema) once (e.g. in a system table in a database) and 
    thus eliminating metadata overhead from many records using the same schema.

  - DOM-based protocols

    DOM-based protocol parse whole payload into an in-memory Data Object Model
    which then is queried during deserialization. Typically this kind of
    protocol is used to implement text based encoding such as JSON or XML.

Compact Binary
--------------

A binary, tagged protocol using variable integer encoding and compact field
header. A good choice, along with [Fast Binary](#fast-binary), for RPC
scenarios. 

Implemented in [`CompactBinaryReader`][compact_binary_reader_reference] and 
[`CompactBinaryWriter`][compact_binary_writer_reference] classes.

Version 2 of Compact Binary adds length prefix for structs. This enables
deserialization of [`bonded<T>`](#understanding-bondedt) and skipping of
unknown struct fields in constant time. The trade-off is double pass encoding,
resulting in up to 30% slower serialization performance. 

See also [Compact Binary encoding reference][compact_binary_format_reference].

Fast Binary
-----------

A binary, tagged protocol similar to [Compact Binary](#compact-binary) but 
optimized for deserialization speed rather than payload compactness.

Implemented in [`FastBinaryReader`][fast_binary_reader_reference] and 
[`FastBinaryWriter`][fast_binary_writer_reference] classes. 

See also [Fast Binary encoding reference][fast_binary_format_reference].

Simple Binary
-------------

A binary, untagged protocol which is a good choice for storage scenarios as it
offers potential for big saving on payload size. Because Simple is an untagged
protocol, it requires that the payload schema is available during
deserialization. In typical storage scenario application would store [runtime
schema](#runtime-schema) and use it during deserialization with
[`bonded<void>`][bonded_void_reference]. In some specific scenarios when it can
be assumed that producer and consumer have exactly the same schema, Simple
Protocol can be used with compile-time schema, providing unparalleled
deserialization performance. One example is marshaling objects between
processes or between native and managed components. 

Implemented in [`SimpleBinaryReader`][simple_binary_reader_reference] and 
[`SimpleBinaryWriter`][simple_binary_writer_reference] classes.

Version 2 of Simple Protocol uses variable integer encoding for string and 
container lengths, resulting in more compact payload without measurable 
performance impact.

See example: `examples/cpp/core/protocol_versions`.

Simple JSON
-----------

Simple JSON encoding implemented as a DOM protocol. The output is a
standard JSON and is a very good choice for interoperating with other systems
or generating human readable payload. Because payload doesn't include field
ordinals, there are two caveats when used as a Bond serialization protocol:

- Transcoding from Simple JSON to binary Bond protocols is not supported 
  (transcoding from a binary protocol to Simple JSON is supported).
- Field matching is done by field name rather than ordinal. The implication
  is that renaming a field (which is considered a bad practice anyways) is
  a breaking schema change for Simple JSON.

Simple JSON flattens inheritance hierarchy which may lead to name conflicts 
between fields of base and derived Bond struct. It is possible to resolve such 
conflicts w/o the need to actually rename the fields by annotating fields with 
JsonName attribute, e.g.:

    struct Base
    {
        0: string foo;
    }

    struct Derived : Base
    {
        [JsonName("DerivedFoo")]
        0: string foo;
    }

Implemented in [`SimpleJsonReader`][simple_json_reader_reference] and
[`SimpleJsonWriter`][simple_json_writer_reference] classes.

See examples:

- `examples/cpp/core/simple_json`


Custom type mappings
====================

Bond codegen provides a simple extensibility mechanism allowing use of custom 
C++ types to represent types in a Bond schema. One common scenario is replacing 
the default STL containers with a different implementation that is semantically 
identical, e.g. `std::unordered_map` instead of `std::map`. Custom type 
mappings can be also used to introduce completely new types which can be 
serialized to one of the built-in Bond schema types. For example time could be 
represented using the `boost::posix_time::ptime` class and serialized as 
`int64`.

Defining a custom type mapping involves three steps:

- Define a [type alias](compiler.html#type-aliases) in the schema.
- Specify during codegen a C++ type to represent the alias.
- Implement an appropriate concept for the custom C++ type.

Codegen parameters
------------------

When generating code for a schema that uses [type aliases](compiler.html#type-aliases), the 
user can specify a custom type to represent each alias in the generated code:

    gbc c++ --using="time=boost::posix_time::ptime" time.bond

The value of the `--using` parameter is a custom alias mapping in the following 
format:

    alias-name=generated-type-name

Generated code using custom types usually has to include a header file with 
appropriate declarations. The `gbc` compiler supports the `--header` parameter 
for that purpose:

    gbc c++ --header="<time_alias.h>" --using="time=boost::posix_time::ptime" time.bond

The above command will add the following statement at the top of the generated
header file `time_types.h`:

    #include <time_alias.h>

Container concept
-----------------

The custom container concept defined in `bond/core/container_interface.h` 
serves as an interface between a custom container type and Bond. The concept 
consists of compile-time meta-function (traits) that need to be specialized for 
the custom container type and overloaded free functions which implement the 
runtime interaction with the container instances. The concept forms 
a non-intrusive interface, which can be provided for any type without changes 
to its implementation.

The first step is to identify a type as an appropriate container by 
specializing one of the following traits:

    template <typename T> struct
    is_set_container
        : std::false_type {};

    template <typename T> struct
    is_map_container
        : std::false_type {};

    template <typename T> struct
    is_list_container
        : std::false_type {};

For example the following specialization would allow Bond to treat `std::array` 
as a list type:

    template <typename T, std::size_t N> struct
    is_list_container<std::array<T, N> >
        : std::true_type {};

The second trait called `element_type` specifies the type of the container 
elements:

    template <typename T> struct
    element_type
    {
        typedef typename T::value_type type;
    };

The default implementation assumes a commonly used STL convention of using 
a nested `value_type` typedef and will likely work for many container 
implementations from libraries like Boost. For other containers the trait can 
specialized, e.g.:

    template <typename T> struct
    element_type<MyList<T> >
    {
        typedef T type;
    };

The next part of the container concept consists of free functions exposing 
container size and operations to add and remove elements.

    template <typename T>
    uint32_t container_size(const T& container);

    template <typename T>
    void resize_list(T& list, uint32_t size);

    template <typename T>
    void clear_set(T& set);

    template <typename S, typename T>
    void set_insert(S& set, const T& item);

    template <typename T>
    void clear_map(T& map);

    template <typename M, typename K, typename T>
    T& mapped_at(M& map, const K& key);

Note that unlike the traits which need to be specialized in the `bond` 
namespace, these function can be overloaded in the namespace of the container 
type.

The final part of the container concept are enumerators:

    template <typename T>
    class enumerator
    {
        explicit enumerator(T& container);
        bool more() const;
        typename element_type<T>::type& next();
    };


    template <typename T>
    class const_enumerator
    {
        explicit const_enumerator(const T& container);
        bool more() const;
        const typename element_type<T>::type& next();
    };

The `const_enumerator` must be implemented for any custom container while the 
`enumerator` is used only for lists. As the name indicates, the enumerators 
abstract iteration over elements of the container. The default implementation 
of `const_enumerator` illustrates well the simple semantics of the interface 
[^enumerator]:


    template <typename T>
    class const_enumerator
    {
    public:
        explicit const_enumerator(const T& container)
            : it(container.begin()),
              end(container.end())
        {}

        bool more() const 
        {
            return it != end;
        }

        typename T::const_reference 
        next()
        {
            return *(it++);
        }

    private:
        typename T::const_iterator it, end;
    };

- `examples/cpp/core/container_of_pointers`
- `examples/cpp/core/multiprecision`
- `examples/cpp/core/static_array`

[^enumerator]: The default implementation assumes broadly used STL conventions 
and may work for many container implementations from libraries such as Boost. 
For such containers is not necessary to specialize the enumerators.

String concept
--------------

The custom string concept defined in `bond/core/container_interface.h` serves 
as an interface between a custom string type and Bond.

Custom string types are identified by specializing the appropriate trait:

    template <typename T> struct
    is_string
        : std::false_type {};

    template <typename T> struct
    is_wstring
        : std::false_type {};

For example the following specialization would allow Bond to treat 
`boost::string_ref` as a string type:

    template <> struct
    is_string<boost::string_ref>
        : std::true_type {};

The operations on custom strings are exposed by overloading the following free 
functions:

    template<typename C, typename T>
    const C* string_data(const T& str);

    template<typename C, typename T>
    C* string_data(T& str);

    template<typename T>
    uint32_t string_length(const T& str);

    template<typename T>
    void resize_string(T& str, uint32_t size);

- `examples/cpp/core/string_ref`

Scalar concept
--------------

The custom scalar type concept defined in `bond/core/scalar_interface.h` serves 
as an interface between Bond and a custom type aliasing a built-in scalar type.

The `aliased_type` trait is used to specify which built-in type is being 
aliased:

    template <typename T> struct
    aliased_type
    {
        typedef void type;
    };

For example the following specialization would tell Bond to treat
`boost::posix_time::ptime` as if it were an alias of `int64`.

    template <> struct
    aliased_type<boost::posix_time::ptime>
    {
        typedef int64_t type;
    };

Conversions to/from a custom type and its aliased type are implemented as 
a pair of free function:

    template <typename T>
    void set_aliased_value(T& var, typename aliased_type<T>::type value);

    template <typename T>
    typename aliased_type<T>::type get_aliased_value(const T& value);

- `examples/cpp/core/time_alias`

Custom allocators
=================

The Bond compiler flag `--allocator` can be used to generate schema structs 
such that all containers are declared to use a custom allocator type:

    gbc c++ --allocator=my::arena example.bond

If the allocator is stateful, the application can pass a pointer to an 
allocator instance to the struct constructor. The allocator will then be passed 
to constructors of all container fields and nested structs. During 
deserialization Bond will also make sure that any container elements are 
constructed using the same allocator. 

The generated structs can use any allocator which implements the C++ Standard 
Library allocator concept. 

Bond APIs which allocate memory also allow use of custom allocators. In 
particular `bond::OutputMemoryStream`, which can be used as output stream for 
Bond serialization, can allocate the memory blobs for serialized payload with 
a user specified allocator.

    typedef bond::OutputMemoryStream<my::arena> Output;

    my::arena arena;
    Output output(arena);
    bond::CompactBinaryWriter<Output> writer(output);

See example `examples/cpp/core/output_stream_allocator`.

Custom streams
==============

Applications can define custom buffers used for writing/reading data during 
[de/serialization](#serialization). 

An input stream class implements the following input stream concept [^concept]:

    class InputStream
    {
    public:
        // Read overload(s) for arithmetic types
        template <typename T>
        void Read(T& value);

        // Read into a memory buffer
        void Read(void *buffer, uint32_t size);

        // Read into a memory blob
        void Read(bond::blob& blob, uint32_t size);
    };

An output stream class implements the following output stream concept:

    class OutputStream
    {
    public:
        // Write overload(s) for arithmetic types
        template<typename T>
        void Write(const T& value);

        // Write a memory buffer
        void Write(const void* value, uint32_t size);

        // Write a memory blob
        void Write(const bond::blob& blob);
    };

[^concept]: Note that input/output streams are not _interface classes_ which 
can be derived from. They are conceptual interfaces, a set of method signatures 
that need to be implemented. Furthermore, the Read and Write templates don't 
have to be implemented as a single method template (or, for that matter, as 
a template at all). They merely mean that a Read/Write method overload must be 
defined for every arithmetic type in Bond meta-schema.

Scoped enumerations
===================

Bond provides a standard-compliant solution for scoped enumerations in C++ that 
overcomes the limitations of normal C++ enumeration types. Usually enumerations 
are part of a schema used with library Bond APIs. However applications can use 
the following technique to defined standalone enum types.

Define your enumerations in a .bond file. You can use the same identifiers for 
constants in different enumerations in the same namespace scope.

    namespace example

    enum Fruit
    {
        Orange = 1,
        Apple = 2
    }
    
    enum Color
    {
        Green = 1,
        Orange = 7
    }

Use the `--enum-header` gbc compiler option to generate a standalone 
_filename_`_enum.h` header file that you can include for scoped enumerations.

    #include "enumerations_enum.h"

    int main()
    {
        example::Fruit fruit;
        example::Color color;

        fruit = example::Fruit::Orange;
        fruit = example::Apple;

        color = example::Green;
        color = example::Color::Orange;

        return 0;
    }

This solution uses the Bond compiler to generate the code that you use but your 
code does not take a dependency on the Bond library.

See example: `examples/cpp/core/enumerations`.

Exceptions
==========

Bond uses C++ exceptions to communicate errors. All exceptions derive from 
`std::exception` and implement the `what()` method which returns a human 
readable description of the error. The following exceptions can be thrown 
during calls to Bond APIs:

`bond::Exception`
-----------------

Bond-specific errors are reported using exceptions derived from 
`bond::Exception` which itself derives from `std::exception`. The following 
exception types are defined:

- `bond::CoreException`

    Used to indicate errors such as a missing required field during 
    deserialization or an invalid protocol during unmarshaling.

- `bond::StreamException`

    Used to indicate errors related to reading from or writing to a data 
    stream, for example attempting to read past the end of data stream.

`std::bad_alloc`
----------------

Failure to allocate memory. For example, this exception can be thrown if the 
payload being deserialized contains a container with more elements than can be 
fit into available memory.

`std::length_error`
-------------------

The standard library throws this exception to report errors that are 
consequence of attempt to exceed implementation-defined lengths for objects 
such as `std::string` or `std::vector`.

`std::range_error`
------------------

The standard library `std::wstring_convert::from_bytes` and 
`std::wstring_convert::to_bytes` which are used by Bond JSON de/serializer 
throw this exception to indicate an invalid string encoding.

Optimizing build time
=====================
 
Extensive use of C++ templates in Bond may sometimes lead to long compilation 
times. In order to optimize build speed for projects using Bond, it is 
important to understand how the Bond implementation in particular, and C++ 
templates in general, affect compilation and linking time.

At the high level, the cost of compiling template heavy C++ code comes from 
template instantiation. In Bond the most expensive templates to instantiate are 
related to deserialization. Deserialization APIs are usually instantiated for 
all enabled [protocols](#protocols). This leads to the first obvious way to 
optimize build speed: enable only the protocols that are needed. 

The following built-in protocols are enabled by default:

- Compact Binary
- Fast Binary
- Simple Binary

Two sets of macros control which built-in protocol are enabled.

- Enable a specific protocol and disable all others

    - `BOND_COMPACT_BINARY_PROTOCOL`
    - `BOND_SIMPLE_BINARY_PROTOCOL`
    - `BOND_FAST_BINARY_PROTOCOL`
    - `BOND_SIMPLE_JSON_PROTOCOL`

It is critical that these macros are always defined the same way for all 
compilation units that will be linked into a particular executable. Failure to 
do so may lead to violation of the C++ One Definition rule [^one_definition]. 
To avoid this problem the recommended way to set these macros is via the C++ 
compiler command line flags in the makefile, e.g.:

    /DBOND_COMPACT_BINARY_PROTOCOL /DBOND_SIMPLE_BINARY_PROTOCOL

C++ templates are instantiated separately in every compilation unit. This means 
that building an application which has calls to Bond APIs deserializing a 
particular schema in multiple .cpp files will result in repeated instantiation 
of the same templates, unnecessarily increasing build time. In order to 
mitigate this problem, the Bond compiler generates two additional files for 
each schema file: _filename_`_apply.h` and _filename_`_apply.cpp`. Using these 
files is optional but for most non-trivial applications it will result in 
significant improvement of build time. The `_apply.cpp` can be built as part of 
the application project itself, but often it is better to compile it into a 
separate static library. This way the static library needs to be rebuilt only 
when the schemas change, and otherwise applications can be quickly built and 
linked with the schema library. The _filename_`_apply.h` can be thought of as a 
header file for the schema library; it must be included in every compilation 
unit where Bond APIs are called for any schema defined in _filename_`.bond`.

The Bond compiler command line switch `--apply` can be use to control which 
protocols are included in the generated `_apply` files. This can be used to 
reduce compilation time for `_apply.cpp`.

    gbc c++ --apply=fast example.bond

Compiling generated _filename_`_apply.cpp` results in instantiation of all the 
templates used by the most common APIs such a Serialize and Deserialize for all 
the schemas defined in _filename_`.bond`. For applications with very large 
schemas it is often beneficial to spread the schema definitions across multiple 
`.bond` files. This allows building in parallel multiple, smaller `_apply.cpp` 
files rather that one large file.

C++ templates for Bond internal schemas, such as those used to serialize 
runtime schema, are pre-instantiated and included in Bond static libraries. It 
is recommended that applications include the header file 
`bond/core/bond_apply.h` and link to the `bond_apply` static library in order 
to reuse the pre-instantiated code. The one exception are applications using 
custom protocols - by definition templates pre-instantiated at the time Bond 
library was built can't support custom protocols.

When using the Microsoft Visual Studio toolchain there are two important things 
to be aware related to build time. First, always use 64-bit tools. In 
particular the 32-bit version of link.exe is not capable of linking large 
template-based code. It will either fail, or if you are unlucky, it will run 
several orders of magnitude times longer than the 64-bit version. You can force 
64-bit tools using environment variables:

For Visual Studio 2012:

    set _IsNativeEnvironment=true

For Visual Studio 2013:

    set PreferredToolArchitecture=x64

Link-time code generation can also lead to egregiously long link times. It is 
strongly recommended to disable it for projects using Bond. Link-time code 
generation does not improve runtime performance for Bond APIs and in many cases 
it actually degrades it. If application code _must_ be built with LTCG enabled, 
we recommend using a separate static schema library as described above, and 
disabling LTCG when building the library.

See example: `examples/cpp/core/static_library`.

[^one_definition]: Since breaking the C++ One Definition rule may lead to very 
unpredictable runtime behaviour, the Bond implementation has a built-in 
assertion mechanism to detect it.

References
==========

[C++ API reference][API_reference]
------------------------------

[Bond compiler reference][compiler]
---------------------------

[C# User's Manual][bond_cs]
---------------------------

[Python User's Manual][bond_py]
----------------------------

[API_reference]: ../reference/cpp/index.html

[compiler]: compiler.html

[bond_py]: bond_py.html

[bond_cs]: bond_cs.html

[serializing_transform_ref]: 
../reference/cpp/structbond_1_1_serializing_transform.html

[deserializing_transform_ref]: 
../reference/cpp/structbond_1_1_deserializing_transform.html

[modifying_transform_ref]: 
../reference/cpp/structbond_1_1_modifying_transform.html

[transforms_h]: ../reference/cpp/transforms_8h_source.html

[field_template_reference]: 
../reference/cpp/structbond_1_1reflection_1_1_field_template.html

[maybe_reference]: ../reference/cpp/classbond_1_1maybe.html

[nullable_reference]: 
../reference/cpp/classbond_1_1nullable_3_01_t_00_01_allocator_00_01false_01_4.html

[bonded_reference]: ../reference/cpp/classbond_1_1bonded.html

[bonded_void_reference]: 
../reference/cpp/classbond_1_1bonded_3_01void_00_01_reader_01_4.html

[blob_reference]: ../reference/cpp/classbond_1_1blob.html

[compact_binary_reader_reference]: 
../reference/cpp/classbond_1_1_compact_binary_reader.html

[compact_binary_writer_reference]: 
../reference/cpp/classbond_1_1_compact_binary_writer.html

[compact_binary_format_reference]: 
../reference/cpp/compact__binary_8h_source.html

[fast_binary_reader_reference]: 
../reference/cpp/classbond_1_1_fast_binary_reader.html

[fast_binary_writer_reference]: 
../reference/cpp/classbond_1_1_fast_binary_writer.html

[fast_binary_format_reference]: 
../reference/cpp/fast__binary_8h_source.html

[simple_binary_reader_reference]: 
../reference/cpp/classbond_1_1_simple_binary_reader.html

[simple_binary_writer_reference]: 
../reference/cpp/classbond_1_1_simple_binary_writer.html
 
[simple_json_reader_reference]: 
../reference/cpp/classbond_1_1_simple_json_reader.html

[simple_json_writer_reference]: 
../reference/cpp/classbond_1_1_simple_json_writer.html
 
