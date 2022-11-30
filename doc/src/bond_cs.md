% A Thorough Guide to Bond for C#

About
=====

Bond is an extensible framework for working with schematized data. It is
suitable for scenarios ranging from service communications to Big Data storage
and processing.

Bond defines a rich type system and [schema evolution rules](#schema-evolution)
which allow forward and backward compatibility. The core Bond features include
high performance serialization/deserialization and a very powerful, generic
data transform mechanism. The framework is highly extensible via pluggable
serialization protocols, data streams, user defined type aliases and more.

By design Bond is language and platform independent and is currently supported
for C++, C#, Java, and Python on Linux, macOS, and Windows.

Bond is published on GitHub at
[https://github.com/microsoft/bond/](https://github.com/microsoft/bond/).

Basic example
=============

In Bond data schemas are defined using idl-like
[syntax](compiler.html#idl-syntax):

```
namespace Examples

struct Record
{
    0: string Name;
    1: vector<double> Constants;
}
```

In order to use the schema in a C# program, it needs to be compiled using the
Bond compiler. This step is sometimes also referred to as code generation (or
codegen) because the compilation generates C# code corresponding to the schema
definition.

```
gbc c# example.bond
```

Using the generated C# code, we can write a simple program that will
serialize and deserialize an instance of the Record schema using [Compact
Binary](bond_cpp.html#compact-binary) protocol:

```csharp
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
```

Code generation
===============

In order to use a Bond schema in a C# program, it needs to be compiled using the
Bond compiler [`gbc`](compiler.html). The compiler generates C# classes that
represent the schema. By default schema fields are represented by public
auto-properties initialized in the default constructor.

The mapping between Bond and C# type systems is mostly obvious, but it is worth
noting that, unlike C# reference types, Bond types are not nullable. This means
that `string` in Bond IDL will be mapped to C# `string`, which is a reference
type, but the value `null` will not be valid. In order to allow `null` values, a
type must be declared as [`nullable`](bond_cpp.html#nullable-types), e.g.:

```
struct Foo
{
    0: list<nullable<string>> listOfNullableStrings;
}
```

The value `null` is also legal for fields declared in Bond IDL to have a [default
of `nothing`](bond_cpp.html#default-value-of-nothing), e.g.:

```
struct Bar
{
    0: string str = nothing;
}
```

Code generation can be customized by passing one or more of the following
command line options to `gbc`:

`--fields`

Schema fields are represented by public fields with initializers and no
constructor is generated.

`--readonly-properties`

Schema fields are represented by properties with public getter and private
setter and initialized to the default values in the default constructor.
Classes with read-only properties are fully supported by all Bond APIs.

`--preview-constructor-parameters`

A constructor is generated with a parameter to initialize each of the schema
fields. This option is typically used in conjunction with
`--readonly-properties`.  This functionailty is in preview and may change.

`--collection-interfaces`

Collection types `vector<T>`, `map<K, V>`, `list<T>` and `set<T>` are
represented by respective generic collection interfaces: `IList<T>`,
`IDictionary<K, V>`, `ICollection<T>` and `ISet<T>`.

Serializer
==========

Bond serialization API is provided by the `Serializer` class. It is a generic
class parameterized with type of protocol writer used for serialization:

```csharp
Serializer<CompactBinaryWriter<OutputStream>>
```

The constructor of the `Serializer` class takes the type of a class or struct
representing Bond schema:

```csharp
new Serializer<CompactBinaryWriter<OutputStream>>(typeof(Record))
```

The constructor is non-trivial so application usually should create an instance
of `Serializer` outside of the inner loop and reuse it.

The `Serializer` class exposes one public method `Serialize` which takes two
arguments, an object to be serialized and an instance of the protocol writer to
be used for serialization.

```csharp
serializer.Serialize(obj, writer);
```

The object's type must be the same as the type passed to the `Serializer`
constructor, otherwise the behaviour is undefined.

Bond provides a helper static API for applications that use schema types known
at compile-time and don't need to manage lifetime of the `Serializer`:

```csharp
Serialize.To(writer, obj);
```

When the API is called for the first time, a static instance of appropriate
`Serializer` is created. Because of this the first call to the API for given
type and protocol may take relatively long time. Subsequent calls for the same
writer/object types reuse the static instance.

Deserializer
============

Bond serialization API is provided by the `Deserializer` class. It is a generic
class parameterized with type of the protocol reader to be used for
deserialization:

```csharp
Deserializer<CompactBinaryReader<InputStream>>
```

The constructor of the `Deserializer` class takes the type of a class or struct
representing Bond schema:

```csharp
new Deserializer<CompactBinaryReader<InputStream>>(typeof(Record))
```

The constructor is non-trivial so application usually should create an instance
of `Deserializer` outside of the inner loop and reuse it.

The `Deserializer` class exposes one generic public method `Deserialize` which
takes as an argument an instance of the protocol reader and returns
a deserialized object:

```csharp
var record = deserializer.Deserialize<Record>(reader);
```

The object created by `Deserialize` is always of the type specified during
`Deserializer` construction. The type parameter of the method is only used to
cast the result.

Deserializing from payload encoded in an untagged protocol like Simple usually
requires specifying schema of the payload. To address this scenario the
`Deserializer` class has an additional constructor taking
a [`RuntimeSchema`](#runtime-schema) as an argument:

```csharp
RuntimeSchema schema;
// ...
var d = new Deserializer<SimpleReader<InputStream>>(typeof(Record), schema);
var obj = d.Deserialize<Record>(reader);
```

An instance of `Deserializer` created this way is tied to the triplet of
protocol, object type and payload schema. In order to deserialize from payload
in another schema a new instance of `Deserializer` needs to be created.

Bond provides a helper static API for applications that use schema types known
at compile-time, don't need to specify payload schema and don't need to manage
lifetime of the `Deserializer`:

```csharp
var obj = Deserialize<Record>.From(reader);
```

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

```csharp
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
```

See also the following example:

- `examples/cs/core/marshaling`

Schema evolution
================

Bond does not use explicit versioning to deal with changes to schemas (and the
resulting types) over time. Instead, Bond supports certain schema evolution
operations which allow the producer and consumer of Bond types to evolve
without lockstep coordination.

The following changes to a schema will never break compatibility across the wire:

- Adding or removing an `optional` or `required_optional` field
- Changing a field's type between `int32` and `enum`
- Changing a field's type between `vector<T>` and `list<T>`
- Changing a field's type between `blob` and `vector<int8>` or `blob` and
  `list<int8>`
- Changing a field's type between `T` and `bonded<T>`
- Adding new enumeration constants that don't alter existing constants (beware
  of implicit reordering)

The following changes to a type are generally safe but require some
consideration about how the change is rolled out:

- Changing a field between `optional` and `required_optional` or between
  `required_optional` and `required`. The `required_optional` modifier
  facilitates a two-step process for changing between `optional` and
  `required`, but the first step must be completed on both the producer and the
  consumer sides before the second step can be started on either side.
- Promoting a field with a numeric type from a smaller size to a larger size
  (e.g.: `float` to `double`, `uint8` to `uint16`, `uint8` to `uint32`, `int8`
  to `int16`, etc.). The consumer must get the change before the producer.
  Note that changing from unsigned to signed or vice versa is *not* compatible
  (e.g.: `uint8` to `int16`).
- Promoting from `int8` or `int16` to `enum`. The consumer must get the change
  before the producer.

These following changes will break wire compatibility and are not recommended:

- Adding or removing `required` fields
- Incompatible change of field types (any type change *not* covered above);
  e.g.: `int32` to `string`, `string` to `wstring`, `float` to
  `nullable<float>`
- Changing of field ordinals/ids
- Changing of inheritance hierarchy (add/remove/substituting base struct)
- Changing between `required` and `optional` directly
- Changing the default value of a field
- Changing existing enumeration constants in any way (including implicit
  renumbering)

Some best practices and other considerations to keep in mind:

- When removing a field, comment it out rather than removing it altogether
  so that the neither the field ordinal nor name are reused in future edits
  of the schema
- When working with untagged protocols like
  [SimpleBinaryProtocol](#simple-binary), great care must be taken to ensure
  the same [schema](#runtime-schema) is used when deserializing the payload as
  was used to serialize it.
- Caution should be used when changing or reusing field names as this could
  break text-based protocols like [SimpleJsonProtocol](#simple-json)
- `required` should be used sparingly and only with careful consideration

Default values
==============

Fields of a Bond defined struct always have a default value, either
explicitly specified in the .bond file, or the implicit default.

The implicit default is

* `false` for `bool` fields
* 0 for arithmetic types
* empty for string/containers
* `null` for [nullable type](#nullable-types)
* for struct and `bonded` fields, an instance of a struct in which all of
  the fields are initialized to their default values, recursively

There is no implicit default for enum fields: they must have an explicit
default value in the .bond file.

Explicit default values (other than [`nothing`](#default-value-of-nothing))
may not be specified for `nullable` or container fields. Struct and `bonded`
fields may not have an explicit default value. They always use their
implicit default values.

The default values of fields matter because this is what an application will
see after deserialization for any optional field that wasn't present in the
payload (e.g. when the payload was created from an older version of the
schema).

Additionally, some protocols can omit
[`optional` non-struct fields](bond_cpp.html#required-fields) set to their
default values, reducing payload size.

Default value of `nothing`
==========================

Sometimes it is necessary to distinguish between any of the possible values
of a field and absence of a value. To support such scenarios Bond allows
non-struct fields' default values to be explicitly set to `nothing` [^1]:

```
struct AboutNothing
{
    0: uint16 n = nothing;
    1: string name = nothing;
    2: list<float> floats = nothing;
}
```

Setting a field's default to `nothing` doesn't affect the schema type of the
field, however it may affect what type the field is mapped to in the
generated code. The reason why is pretty obvious: some types such as
`ushort` just can't represent absence of a value. In C# reference types
already have a way to represent absence of value: `null`. For these types
specifying a default of `nothing` doesn't change the field type in the
generated code. For C# value types such as `UInt16`, the generated code will
use
[C# Nullable types](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/nullable-types/)
(e.g. `UInt16?`).

The fact that setting the default value of a field to `nothing` doesn't
affect the field's schema type has an important consequence: the default
value of the field doesn't have a serialized representation. What this means
in practice depends on whether the field is `optional` or `required`.
Optional fields set to `nothing` are usually omitted during serialization
[^2], just like for any other default values.
[Required fields](bond_cpp.html#required-fields), by definition, can never
be omitted. Since `nothing` has no serialized representation, an attempt to
serialize an object with required fields set to `nothing` will result in a
runtime exception. If a null value needs to be represented in the serialized
form, then a default of `nothing` is the wrong choice and a
[nullable type](#nullable-types) should be used instead.


[^1]: In Bond there is no concept of a default value for structs and thus a
default of `nothing` can't be set for fields of struct types or `bonded<T>`.

[^2]: Some protocols might not support omitting optional fields (e.g. Simple
Protocol). In such cases an attempt to serialize an object with field(s) set
to `nothing` will result in a runtime exception.


Nullable types
==============

For any type in the Bond meta-schema, `nullable<T>` defines a nullable type.
A nullable type can store all the same values as its base type plus one
additional value: `null`.

```
struct Nullables
{
    0: nullable<bool>         b; // can be true, false, or null
    1: list<nullable<string>> l; // can be a (possibly empty) list or null
}
```

The default value for a field of a nullable type is always implicitly set to
`null`. Explicit default values for nullable fields are not supported.

In C# reference types already have a way to represent `null`: `null`. For
these types `nullable<T>` and `T` will have the same type in the generated
code. For C# value types such as `UInt16`, the generated code will use
[C# Nullable types](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/nullable-types/)
(e.g. `bool?`).

Caveat: `blob`, `nullable<blob>` and `blob = nothing` are all represented as
an `ArraySegment<byte>` in the generated C# code. The C# default value for
all three is `default(ArraySegment<byte>)` (in which the `Array` field is
`null`). An empty `ArraySegment<byte>` (in which the `Array` field is not
`null` but the `Count` is 0) is treated as a non-default value, so it will
not be omitted for optional fields. This behavior will be changing in a
future release, to align it with how other types are handled and how
nullable/nothing fields are handled in other languages.

Since a nullable type must represent the additional value of `null`, its
serialized representation necessarily incurs some overhead compared to the
base type. Often it is more efficient to avoid using a nullable type and
instead to designate one of the normal values to handle the special case
that otherwise would be represented by `null`. For example _empty_ is
usually a good choice for string and container types and 0 for arithmetic
types. Another option that may sometimes be appropriate is setting the
default value of a non-struct field to
[`nothing`](#default-value-of-nothing). Struct fields can have neither an
explicit default value nor be set to `nothing`, so `nullable` needs to be
used if `null` semantics are needed for these fields.

The canonical scenario where a nullable type is the right choice is
recursive structures. For example here's how Bond `TypeDef` struct is
defined:

```
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
```

The `TypeDef` struct is used to represent the type of a field in a Bond
schema. If the type is a container such as a list or map, the type
definition becomes recursive. For example, a list type definition contains
the type of the list element which of course itself can be a container of
elements of some other type, and so on, until the recursion is terminated
with a `null` value for the `element` and `key` fields.

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

Implemented in `CompactBinaryReader` and `CompactBinaryWriter` classes.
Version 2 of Compact Binary adds length prefix for structs. This enables
deserialization of [`bonded<T>`](#understanding-bondedt) and skipping of
unknown fields in constant time. The trade-off is double pass encoding,
resulting in up to 30% slower serialization performance.

See also [Compact Binary encoding reference][compact_binary_format_reference].

Fast Binary
-----------

A binary, tagged protocol similar to [Compact Binary](#compact-binary) but
optimized for deserialization speed rather than payload compactness.

Implemented in `FastBinaryReader` and`FastBinaryWriter` classes.

See also [Fast Binary encoding reference][fast_binary_format_reference].

Simple Binary
-------------

A binary, untagged protocol which is a good choice for storage scenarios as it
offers potential for big saving on payload size. Because Simple is an untagged
protocol, it requires that the payload schema is available during
deserialization. In typical storage scenario application would store [runtime
schema](#runtime-schema) and use it during deserialization with `BondedVoid`.
In some specific scenarios when it can be assumed that producer and consumer
have exactly the same schema, SimpleProtocol can be used with compile-time
schema, providing unparalleled deserialization performance. One example is
marshaling objects between processes or between native and managed components.

Implemented in `SimpleBinaryReader` and `SimpleBinaryWriter` classes.

Version 2 of Simple Protocol uses variable integer encoding for string and
container lengths, resulting in more compact payload without measurable
performance impact.

See example: `examples/cs/core/untagged_protocols`.

Simple JSON
-----------

The Simple JSON protocol is a simple JSON encoding implemented as a DOM
protocol. The output is standard JSON and is a very good choice for
interoperating with other systems or generating human readable payload.

Because the payload doesn't include field ordinals, there are two caveats
when used as a Bond serialization protocol:

- Transcoding from Simple JSON to binary Bond protocols is not supported
  (transcoding from a binary protocol to Simple JSON is supported if you
  have the schema).
- Field matching is done by field name rather than ordinal. The implication
  is that renaming a field (which is considered a bad practice anyways) is a
  breaking schema change for Simple JSON.

Simple JSON also flattens the inheritance hierarchy which may lead to name
conflicts between fields of base and derived Bond structs. It is possible to
resolve such conflicts without the need to actually rename the fields by
annotating fields with `JsonName` attribute, e.g.:

```
struct Base
{
    0: string foo;
}

struct Derived : Base
{
    [JsonName("DerivedFoo")]
    0: string foo;
}
```

Note that Simple JSON is not designed to be able to read arbitrary JSON
objects. Simple JSON has its own way of encoding Bond objects in JSON that
differs from how other libraries would encode the same object. When
interoperating with other JSON libraries, be aware of these differences:

- maps are encoded as arrays of key/value pairs not as sub-objects
- the inheritance hierarchy is flattened
- nulls are expressed as empty arrays
- enums are encoded via their numeric value, not their symbolic names

Implemented in `SimpleJsonReader` and `SimpleJsonWriter` classes.

See examples:

- `examples/cs/core/simple_json`

Transcoder
==========

The `Transcoder` class provides API for converting payloads from one protocol
into another. Transcoder operates directly on serialized data and doesn't need
to know the C# type representing payload schema.

The `Transcoder` is a generic class parameterized with types of source protocol
reader and destination protocol writer, e.g.:

```csharp
Transcoder<CompactBinaryReader<InputStream>, SimpleWriter<OutputStream>>
```

The constructor of the `Transcoder` class takes as an optional argument the
[runtime schema](#runtime-schema) of the payload. The argument is optional when
transcoding between tagged protocols but must be specified when transcoding
from an untagged protocol or to a text protocol.

```csharp
RuntimeSchema schema;
// ...
var t = new Transcoder<<SimpleReader<InputStream>, CompactBinaryWriter<OutputStream>>(schema);
```

The `Transcoder` constructor is non-trivial so application usually should
create an instance of `Transcoder` outside of the inner loop and reuse it.

The `Transcoder` class exposes one public method `Transcode` which takes two
arguments, protocol reader representing payload to transcode from, and protocol
writer to be used to write the result.

```csharp
t.Transcode(reader, writer);
```

Bond provides a static helper API for application that don't need to explicitly
manage `Transcoder` lifetime and don't specify payload schema or use a schema
known at compile-time:

```csharp
// transcoding w/o schema
Transcode.FromTo(reader, writer);

// transcoding with compile-time schema Foo
Transcode<Foo>.FromTo(reader, writer);
```

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

The input and output for binary protocols is provided by the
[`IInputStream`](https://github.com/microsoft/bond/blob/master/cs/src/core/io/IInputStream.cs)
and
[`IOutputStream`](https://github.com/microsoft/bond/blob/master/cs/src/core/io/IOutputStream.cs)
interfaces. Bond comes with standard implementations of these interfaces for
memory buffers and `System.IO.Stream`. Applications can also provide their
own custom implementations.

The `OutputBuffer` class implements the `IOutputStream` interface on top of
a memory buffer. It comes in two variants.
[`Bond.IO.Safe.OutputBuffer`](https://github.com/microsoft/bond/blob/master/cs/src/core/io/safe/OutputBuffer.cs)
uses only safe managed code and is included in the `Bond.dll` assembly.
[`Bond.IO.Unsafe.OutputBuffer`](https://github.com/microsoft/bond/blob/master/cs/src/io/unsafe/OutputBuffer.cs)
uses unsafe code to optimize for performance. It is included in the
`Bond.IO.dll` assembly. Both implementations have identical class names and
APIs; the only difference is the namespace in which they are defined.

```csharp
// Create an output buffer with initial size of 16KB
var output = new OutputBuffer(16 * 1024);
var writer = new CompactBinaryWriter<OutputBuffer>(output);

Serialize.To(writer, obj);

// Get the serialized payload form the output buffer
ArraySegment<byte> data = output.Data;
```

The
[`InputBuffer`](https://github.com/microsoft/bond/blob/master/cs/src/core/io/safe/InputBuffer.cs)
class implements the `IInputStream` interface on top of a memory buffer.
Like `OutputBuffer` it comes in two flavors, the safe and portable
[`Bond.IO.Safe.InputBuffer`](https://github.com/microsoft/bond/blob/master/cs/src/core/io/safe/InputBuffer.cs),
and the performance optimized via use of unsafe code
[`Bond.IO.Unsafe.InputBuffer`](https://github.com/microsoft/bond/blob/master/cs/src/io/unsafe/InputBuffer.cs).

```csharp
// Create an input buffer on top of a byte[]
var input = new InputBuffer(byteArray);
var reader = new CompactBinaryReader<InputBuffer>(input);
```

The
[`InputStream`](https://github.com/microsoft/bond/blob/master/cs/src/io/unsafe/InputStream.cs)
and
[`OutputStream`](https://github.com/microsoft/bond/blob/master/cs/src/io/unsafe/OutputStream.cs)
classes provide implementations of `IInputStream` and `IOutputStream` on top
of `System.IO.Stream`. These classes are included in `Bond.IO.dll` and thus
are only available to applications using a full .NET runtime and allowing
unsafe code. `In/OutputStream` can be used with any `Stream`, including
`MemoryStream`. However `InputBuffer` and `OutputBuffer` provide
significantly better performance and are recommended when working with
in-memory payloads.

```csharp
using (var stream = new FileStream("example.bin", FileMode.Open))
{
    var input = new InputStream(stream);
    var reader = new CompactBinaryReader<InputStream>(input);
    var example = Deserialize<Example>.From(reader);
}
```

Cloner
======

The `Cloner` class provides API for deep cloning of objects representing Bond
schemas. Unlike typical cloning, Bond `Cloner` is not limited to making clones
that are of the same type as the source object. The type of the source and the
clone merely need to represent compatible Bond schemas.

The `Cloner` is a generic class parameterized with the source type and its
constructor takes one argument representing the type of clones to be created
e.g.:

```csharp
var cloner = new Cloner<Foo>(typeof(Bar));
```

The constructor is non-trivial so application usually should create an instance
of `Cloner` outside of the inner loop and reuse it.

The `Cloner` exposes one public, generic method `Clone` which takes as the
argument the source object and returns a clone:

```csharp
var clone = cloner.Clone<Bar>(foo);
```

The object created by `Clone` is always of the type specified during `Cloner`
construction. The type parameter of the method is only used to cast the result.

Bond provides a helper static API which creates and caches appropriate instance
of `Cloner` the first time it is used, e.g.:

```csharp
var foo = new Foo();
var clone = Clone<Bar>.From(foo);
```

See also the following example:

- `examples/cs/core/cloning`

Comparer
========

The `Comparer` class provides API for deep comparison for equality of objects
representing Bond schemas. The class exposes one public, static, generic method
`Equal` which takes two parameters representing objects to be compared.

```csharp
var left = new Foo();
var right = new Foo();

bool equal = Comparer.Equal(left, right);
```

Note that the `Comparer` doesn't compare arbitrary C# objects, it compares
instances of Bond schemas. Only fields/properties decorated with Bond
[attributes](#idattribute) and base classes/interface representing Bond
[schemas](#schemaattribute) are considered during comparison.

Performance
===========

Bond offers very fast serialization and deserialization. Here are some tips on
how to achieve the best performance.

1. Explicitly create instances of `Serializer`/`Deserializer`/`Transcoder`

    Instead of using simplified APIs like `Serialize.To` and
    `Deserialize<T>.From` it is usually better to explicitly instantiate and
    cache appropriate `Serializer`/`Deserializer`/`Transcoder` objects.
    Creation of these objects involves generation and JIT'ing of specific
    code to handle the particular operation for a given schema type and
    protocol(s). This may take a relatively long time, especially for large
    schemas, and usually it is best to do it during program initialization.
    Once the object is created it can be reused repeatedly and calling the
    Serialize/Deserialize/Transcode methods will be very fast.

    ```csharp
    var exampleSerializer = new Serializer<CompactBinaryWriter<OutputBuffer>>(typeof(Example));
    var exampleDeserializer = new Deserializer<CompactBinaryReader<InputBuffer>>(typeof(Example));

    var output = new OutputBuffer();
    var writer = new CompactBinaryWriter<OutputBuffer>(output);

    exampleSerializer.Serialize(src, writer);

    var input = new InputBuffer(output.Data);
    var reader = new CompactBinaryReader<InputBuffer>(input);

    var dst = exampleDeserializer.Deserialize<Example>(reader);
    ```

    Note that the type of `Serializer`/`Deserializer` doesn't depend on the
    schema type so it is easy to cache these objects for multiple schemas
    used in an application:

    ```csharp
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

    ```csharp
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

6. Experiment with `inlineNested` when creating
   `Serializer`/`Deserializer`/`Transcoder` instances

    When a `Serializer`/`Deserializer`/`Transcoder` refers to another Bond
    struct, by default, the instructions to serialize/deserialize/transcode
    that type are inlined in the method for the top-level type. This _often_
    results in better runtime performance. However, it can sometimes cause
    long JIT'ing times when creating
    `Serializer`/`Deserializer`/`Transcoder` instances or worse runtime
    performance. (For example, some optimizations are not performed when
    methods get large.) You will need to profile and experiment with
    instances created with `inlineNested` set to `true` (the default) and
    `false`) to see what is the best fit for your scenario.

Runtime schema
==============

Some generic applications may need to work with Bond schemas unknown at
compile-time. In order to address such scenarios Bond defines a type
`SchemaDef` to represent schemas at runtime. Applications can obtain an
instance of `SchemaDef` for a particular type using the `Schema` class:

```csharp
// from a type T
var schema = Schema<T>.RuntimeSchema;

// from type of an object
var schema = Schema.GetRuntimeSchema(typeof(obj));
```

The APIs return an object of type `RuntimeSchema`, which is a thin wrapper over
`SchemaDef`. Access to underlying schema is provided via public properties:

```csharp
var schemaDef = schema.SchemaDef
```

The `SchemaDef` object is always self contained, including the runtime schema
definitions for all nested types (if any). The `RuntimeSchema` class instance
can be constructed from a `SchemaDef`, and then it represents the whole schema,
or from any embedded `TypeDef`:

```csharp
// runtime schema of Foo
var schema = Schema<Foo>.RuntimeSchema;

// runtime schema of the first field of Foo
var fieldSchema = new RuntimeSchema(schema, schema.StructDef.fields[0].type);
```

`SchemaDef` is a Bond type, defined in `bond.bond`, and as such can be
de/serialized like any other Bond type:

```csharp
Serialize.To(writer, Schema<T>.RuntimeSchema.SchemaDef);
```

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

The standard implementations always use default implementations of `Serializer/Deserializer/Cloner/Transcoder`.
In order to customize this behavior, the user can pass custom `ObjectBondedFactory` or `PayloadBondedFactory` delegates
as parameters to `ObjectParser` or `ParserFactory<R>.Create()`:

```csharp
// create serializer for schema type T and protocol reader W
// using custom InstanceBondedFactory
new Serializer<W>(typeof(T), new ObjectParser(typeof(T), CustomObjectBondedFactory), ...);

// create deserializer for schema type T and protocol reader R
// using custom PayloadBondedFactory
new Deserializer<R>(typeof(T), ParserFactory<R>.Create(typeof(T), CustomPayloadBondedFactory), ...);
```

Lazy deserialization
--------------------

Because `bonded<T>` can store (or more accurately, refer to) data representing
a serialized data, it can be used to de facto delay deserialization of some
parts of payload:

```
struct Example
{
    0: Always always;
    1: bonded<Sometimes> sometimes;
}
```

The schema defined above contains two nested fields. When an object of type
`Example` is deserialized, the field `always` will be fully instantiated and
deserialized, but field `sometimes`, which is declared as `bonded<Sometimes>`,
will be merely initialized with a reference to its serialized representation.
Application can then deserialize the object only when needed:

```csharp
var example = Deserialize<Example>.From(reader);

// Deserialize sometimes only when needed
if (needSometimes)
{
    var sometimes = example.sometimes.Deserialize();
}
```

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

```
struct Upstream
{
    0: bonded<Response> response;
    1: float ranking;
}

struct Aggregated
{
    0: list<bonded<Response>> responses;
}
```

Using `bonded<Response>` allows the intermediary to aggregate responses,
preserving their full content, even if the aggregator doesn't use the same
version of the `Response` schema as the upstream.

```csharp
void ProcessResponse(Upstream upstream)
{
    if (upstream.ranking > threshold)
    {
        aggregated.responses.Add(upstream.response);
    }
}
```

Polymorphism
------------

The type parameter `T` in `IBonded<T>` interface is covariant which enables
polymorphism. A `IBonded<Base>` can be initialized with an instance of
`Bonded<Derived>`. For example, given the following schema:

```
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
```

The type of the `shapes` field in C# class `Example` will be
`List<IBonded<Shape>>` and an instance of the `Example` can be initialized as
following:

```csharp
var src = new Example
{
    shapes =
    {
        new Bonded<Circle>(new Circle {kind = Kind.circle, radius = 10),
        new Bonded<Rectangle>(new Rectangle {kind = Kind.rectangle, width = 4, height = 5)
    }
};
```

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

```
gbc c# --using="DateTime=System.DateTime" date_time.bond
```

The value of the `/using` parameter consists of one or more alias substitutions
separated by semicolons, each in the following format:

```
alias-name=generated-type-name
```

Custom containers
-----------------

Type aliases of container types can be mapped to user defined collection
classes as long as they implement the same interfaces as the default
collections: `IEnumerable<T>` as well as `ICollection<T>` for aliases of
`list<T>` and `vector<T>`, `ISet<T>` for aliases of `set<T>` and
`IDictionary<K, V>` for aliases of `map<K ,V>`. Custom mappings of container
type aliases don't require a user defined converter.

- `examples/cs/core/container_alias`

System.Collection.Immutable support
-----------------------------------

Bond provides special support for using the
[System.Collections.Immutable](https://learn.microsoft.com/dotnet/api/system.collections.immutable)
collections as container type aliases. The following aliases are supported:

| Underlying Bond type | Supported System.Collections.Immutable container                                        |
|----------------------|-----------------------------------------------------------------------------------------|
| `vector<T>`          | `ImmutableArray<T>`, `ImmutableList<T>`                                                 |
| `list<T>`            | `ImmutableArray<T>`, `ImmutableHashSet<T>`, `ImmutableList<T>`, `ImmutableSortedSet<T>` |
| `set<T>`             | `ImmutableHashSet<T>`, `ImmutableSortedSet<T>`                                          |
| `map<K, V>`          | `ImmutableDictionary<K, V>`, `ImmutableSortedDictionary<K, V>`                          |

During code generation, immutable collection fields are handled specially.
Since they do not have parameterless constructors, the Bond compiler will
instead use the static `Empty` field is used as the default value, e.g.
[ImmutableList\<T\>.Empty](https://learn.microsoft.com/dotnet/api/system.collections.immutable.immutablelist-1.empty).

When deserializing immutable collections, Bond will use the inner `Builder`
classes to efficiently reconstruct the collection, e.g.
[ImmutableList\<T\>.Builder](https://learn.microsoft.com/dotnet/api/system.collections.immutable.immutablelist-1.builder).

See the below project for examples on using immutable collections as
container aliases:

- `examples/cs/core/immutable_collections_alias`

Converter
---------

Applications using custom mappings for aliases of scalar types, string or blob
must provide converter between the custom type and the default type. The
converter is a public class named `BondTypeAliasConverter` defining a pair of
public static `Convert` methods for each type alias:

```csharp
public static CustomType Convert(AliasedType value, CustomType unused)
public static AliasedType Convert(CustomType value, AliasedType unused)
```

For example if `System.DateTime` was mapped to an alias of `int64` the
following class could be defined to provide conversions between `DateTime` and
`long` (the default type for `int64`):

```csharp
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
```

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

```csharp
var stream = new System.IO.MemoryStream();
var writer = new SimpleXmlWriter(stream);

Serialize.To(writer, src);

output.Flush();
stream.Position = 0;

var reader = new SimpleXmlReader(stream);

var dst = Deserialize<Record>.From(reader);
```

In the example above the Xml reader and writer are constructed directly from an
instance of `System.Stream`. Underneath however they use `System.Xml.XmlReader`
and `System.Xml.XmlWriter` which provide fast, non-cached, forward-only Xml
parsing and generation on top of many different data readers and writers. For
example to deserialize Xml payload from a string:

```csharp
var xmlString = "<Record><Name>test</Name><Numbers><Item>3.14</Item></Numbers></Record>";

var reader = new SimpleXmlReader(XmlReader.Create(new StringReader(xmlString)));
var record = Deserialize<Record>.From(reader);
```

The Simple Xml protocol flattens the inheritance hierarchy, putting fields from
base and derived classes together under a single element. In order to prevent
name conflicts, Simple Xml protocol provides support for optional use of fully
qualified schema names as field element namespaces, e.g.:

```xml
<Derived xmlns:b="urn:Examples.Base" xmlns:d="urn:Examples.Derived">
    <d:Field>10</d:Field>
    <b:Field>foo</b:Field>
</Derived>
```

Namespaces can be enabled when serializing to Xml via the `UseNamespaces` flag
in SimpleXmlWriter.Settings:

```csharp
var writer = new SimpleXmlWriter(stream, new SimpleXmlWriter.Settings
{
    UseNamespaces = true
});
```

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

```csharp
var stream = new System.IO.MemoryStream();
var writer = new SimpleJsonWriter(stream);

Serialize.To(writer, src);

output.Flush();
stream.Position = 0;

var reader = new SimpleJsonReader(stream);

var dst = Deserialize<Record>.From(reader);
```

In the example above the JSON reader and writer are constructed directly from
an instance of `System.Stream`. Alternatively they can be also constructed from
`System.IO.TextReader` and `System.IO.TextWriter`. For example to deserialize
JSON payload from a string:

```csharp
var jsonString = "{Name: test, Numbers: [3.14]}";

var reader = new SimpleJsonReader(new StringReader(jsonString));
var record = Deserialize<Record>.From(reader);
```

The Simple JSON protocol flattens the inheritance hierarchy, putting fields
from base and derived schemas together in the same JSON object. Name conflicts
in the JSON representation between fields of base and derived schema can be
resolved using `JsonName` schema field attribute:

```
struct Base
{
    0: string name;
}

struct Derived : Base
{
    [JsonName("DerivedName")]
    0: string name;
}
```

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

```csharp
[Bond.Schema]
public class Foo {}

[Bond.Schema]
public struct Bar {}

[Bond.Schema]
public interface IFoo {}
```

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

```csharp
[Bond.Schema]
public class Foo
{
    [Bond.Id(0)]
    public string message { get; set; }
}
```

A type representing a schema may have additional fields/properties that don't
represent schema fields and thus are not decorated with Bond attributes.

### RequiredAttribute ###

By default fields of Bond schemas are optional. [Required
fields](bond_cpp.html#required-fields) must be marked with the `Required`
attribute.

```csharp
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
```

### TypeAttribute ###

The `Type` attribute is used to provide additional type information about
schema fields. The attribute is optional because in most cases Bond can infer
type from the C# type of field/property. For example C# type `short` always
maps to Bond type `int16`. However not all C# types have an unambiguous mapping
to Bond type system. For example C# `string` can represent either Bond type
`string` or `wstring`. Similarly C# reference types which are always *nullable*
can represent both *nullable* and *non-nullable* type in Bond schema. One of
the uses for the `Type` is resolving such ambiguities.

```csharp
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
```

Bond defines the following tag types that can be used in a `Type` attribute:

- `nullable`: specifies that a reference or nullable C# type represents
  a *nullable* type in the Bond type system.
- `wstring`: specifies that a string is UTF16 (i.e. `wstring` in the Bond
  type system).
- `blob`: specifies that the type represents the schema type `blob`.

The `Type` attribute can also be used to specify type of object to be created
during deserialization when a field/property type is an interface.

```csharp
[Bond.Schema]
public class Foo
{
    [Bond.Id(0), Bond.Type(typeof(SortedSet<string>))]
    public ISet<string> strings1 { get; set; }

    [Bond.Id(1), Bond.Type(typeof(HashSet<string>))]
    public ISet<string> strings2 { get; set; }
}
```

### DefaultAttribute ###

Bond infers default field values for classes and structs representing schemas
from the field initializers or the class/struct constructor. For interfaces the
default field values must be specified explicitly by decorating the interface
properties with the `Default` attribute. The value specified in the attribute
must be compatible with the field type, otherwise the behaviour is undefined.

```csharp
[Bond.Schema]
public interface IFoo
{
    [Bond.Id(0), Default("default value")]
    public string { get; set; }

    [Bond.Id(0), Default(3.14f)]
    public float { get; set; }
}
```

The `Default` attribute is optional for properties that are decorated with
`nullable` tag (the default is implicitly `null`). For non-nullable collections
the `Default` attribute can either specify `null`, which means default of
`nothing` in the schema, or it can be omitted, which means the default is an
empty collection. Non-nullable properties of a type representing a schema have
no concept of a default value thus the `Default` attribute is not applicable.

### AttributeAttribute ###

The `Attribute` attribute can be used to specify user defined attribute(s) for
schemas, fields and enums.

```csharp
[Bond.Schema]
[Bond.Attribute("name", "value")]
public class Foo
{
    [Bond.Id(0)]
    [Bond.Attribute("custom1", "value1")]
    [Bond.Attribute("custom2", "value2")]
    public string foo;
}
```

Schema attributes are usually used by transforms to customize code generation
but they can also be accessed by applications via reflection.

Protocol decoration
-------------------

Bond defines several attributes that are used to decorate implementation of
custom protocols with extra information.

### ReaderAttribute ###

The `Reader` attribute is used on a protocol writer implementation and
specifies the type that implements the reader for that protocol.

```csharp
[Bond.Reader(typeof(SimpleXmlReader))]
public struct SimpleXmlWriter : IProtocolWriter
{
    // ...
}
```

### ParserAttribute ###

The `Parser` attribute can be used on a protocol reader implementation and
specifies the type of parser to be used for the protocol. It is optional for
protocols that implement `IUntaggedReader` or `ITaggedReader` because they
implicitly default to use `UntaggedParser` and `TaggedParser` respectively.
When specified, the `Parser` attribute value must be a generic type definition
with one type parameter, it must implement `IParser` interface and have two
public constructors, one accepting `RuntimeSchema` argument and one accepting
`Type` argument (compile-time schema).

```csharp
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
```

### SerializerAttribute ###

The `Serializer` attribute can be used on a protocol writer implementation to
specify custom serializer for the writer. If the attribute is not specified
then the default serializer implementation is used. The value of `Serializer`
attribute must be a generic type definition with two type parameters `R` and
`W`, it must implement `ISerializerGenerator<R, W>` interface and define two
public constructors:

```csharp
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
```

NuGet packages
==============

Pre-compiled versions of Bond are distributed via NuGet packages from NuGet.org.

[![Bond.CSharp NuGet package](https://img.shields.io/nuget/v/Bond.CSharp.svg?style=flat)](https://www.nuget.org/packages/Bond.CSharp/)
**Bond.CSharp** - An omnibus package that pulls in everything required to
use Bond in a C# project. If you're not sure which packages to use, use this
one. (It will pull in all the other packages you need.)

[![Bond.Core.CSharp NuGet package](https://img.shields.io/nuget/v/Bond.Core.CSharp.svg?style=flat)](https://www.nuget.org/packages/Bond.Core.CSharp/)
**Bond.Core.CSharp** - The assemblies required to use Bond at runtime.
Useful if some other assembly already contains the compiled types. If your
project contains .bond files, you will need to use either Bond.CSharp or
Bond.Compiler.CSharp to perform code generation at build time.

[![Bond.Runtime.CSharp NuGet package](https://img.shields.io/nuget/v/Bond.Runtime.CSharp.svg?style=flat)](https://www.nuget.org/packages/Bond.Runtime.CSharp/)
**Bond.Runtime.CSharp** - Additional assemblies that may be needed at
runtime depending on which Bond [protocols](bond_cpp.html#protocols) are
being used. Needed for Simple JSON.

[![Bond.Compiler.CSharp NuGet package](https://img.shields.io/nuget/v/Bond.Compiler.CSharp.svg?style=flat)](https://www.nuget.org/packages/Bond.Compiler.CSharp/)
**Bond.Compiler.CSharp** - A package with the
[Bond compiler (gbc)](compiler.html) and MSBuild targets for C# code
generation. Bond.CSharp includes similar functionality, but pulls in lots of
dependencies. Bond.Complier.CSharp has no dependencies.

[![Bond.Compiler NuGet package](https://img.shields.io/nuget/v/Bond.Compiler.svg?style=flat)](https://www.nuget.org/packages/Bond.Compiler/)
**Bond.Compiler** - A tools-only package that contains the
[Bond compiler (gbc)](compiler.html). This is useful if you want to
integrate gbc into a build process that isn't using C# or MSBuild.

For example, if you want to use Bond's Compact Binary protocol but want to
avoid a dependency on Newtonsoft's JSON.NET, you can use the
Bond.Compiler.CSharp and Bond.Core.CSharp packages together.

## Platform limitations ##

The pre-compiled gbc that is included in these packages is Windows-only. See
the [README](https://github.com/microsoft/bond/blob/master/README.md) for
instructions to build gbc for other platforms.

Bond.IO.dll (which provides the types in the Bond.IO.Unsafe namespace) is
currently Windows-only, as it relies on some Win32 APIs. To stay
cross-platform, only use the types from Bond.dll (in the Bond.IO.Safe
namespace).

## Frameworks targeted ##

This table lists which frameworks are targeted by the Bond assemblies.

This table is accurate for Bond NuGet packages 8.2.0 and later.

| Assembly                 | .NET 4.0 | .NET 4.5 | .NET 4.6 | .NET Standard 1.0 | .NET Standard 1.3 | .NET Standard 1.6 |
|--------------------------|----------|----------|----------|-------------------|-------------------|-------------------|
| Bond.Attributes.dll      | No       | Yes      | ←        | Yes               | ←                 | Yes               |
| Bond.Reflection.dll      | No       | Yes      | ←        | Yes               | ←                 | Yes               |
| Bond.dll                 | No       | Yes      | ←        | Yes               | ←                 | Yes               |
| Bond.JSON.dll            | No       | Yes      | ←        | Yes               | ←                 | Yes               |
| Bond.IO.dll              | No       | Win only | Win only | No                | Win only          | Win only          |

A left arrow (←) indicates that support for that framework is provided by
the version of the assembly that targets a lower version of the framework.

References
==========

[Bond compiler reference][compiler]
---------------------------

[C++ User's Manual][bond_cpp]
---------------------------

[Python User's Manual][bond_py]
----------------------------

[Bond-over-gRPC overview][bond_over_grpc]
----------------------------

[bond_over_grpc]: bond_over_grpc.html
[bond_cpp]: bond_cpp.html
[bond_java]: bond_java.html
[bond_py]: bond_py.html
[compiler]: compiler.html

[compact_binary_format_reference]:
../reference/cpp/compact__binary_8h_source.html

[fast_binary_format_reference]:
../reference/cpp/fast__binary_8h_source.html
