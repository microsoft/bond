% A Thorough Guide to Bond for Java

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

Bond is published on GitHub at [https://github.com/microsoft/bond/](https://github.com/microsoft/bond/).

Basic example
=============

In Bond data schemas are defined using idl-like
[syntax](compiler.html#idl-syntax):

```
namespace examples

struct Record
{
    0: string name;
    1: vector<double> constants;
}
```

In order to use the schema in a Java program, it needs to be compiled using the
Bond compiler. This step is sometimes also referred to as code generation (or
codegen) because the compilation generates Java code corresponding to the schema
definition.

```
gbc java example.bond
```

Using the generated Java code, we can write a simple program that will
serialize and deserialize an instance of the Record schema using the [Compact
Binary](bond_cpp.html#compact-binary) protocol:

```java
package examples;

import org.bondlib.*;

import java.io.*;

public class Example {
    public static void main(String[] args) throws IOException {
        final Record src = new Record();
        src.name = "FooBar";
        src.constants.add(3.14);
        src.constants.add(6.28);

        final ByteArrayOutputStream output = new ByteArrayOutputStream();
        final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

        final Serializer<Record> serializer = new Serializer<>();
        serializer.serialize(src, writer);

        final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
        final CompactBinaryReader reader = new CompactBinaryReader(input, 1);

        final Deserializer<Record> deserializer = new Deserializer<>(Record.BOND_TYPE);
        final Record dst = deserializer.deserialize(reader);
    }
}
```

Code generation
===============

In order to use a Bond schema in a Java program, it needs to be compiled using
the Bond compiler [`gbc`](compiler.html). The compiler generates Java classes
that represent the schema. Schema fields are represented by public fields, and
collection fields will be automatically initialized to an empty instance.

The mapping between the Bond and Java type systems is mostly obvious, but it is
worth noting that, unlike Java reference types, Bond types are not nullable.
This means that `string` in Bond IDL will be mapped to `java.lang.String`, which
is a reference type, but the value `null` will not be valid. In order to allow
`null` values, a type must be declared as
[`nullable`](bond_cpp.html#nullable-types),
e.g.:

```
struct Foo
{
    0: list<nullable<string>> listOfNullableStrings;
}
```

The value `null` is also legal for fields declared in Bond IDL to have a
[default of `nothing`](bond_cpp.html#default-value-of-nothing), e.g.:

```
struct Bar
{
    0: string str = nothing;
}
```

Serializer
==========

The Bond serialization API is provided by the `Serializer` class. It is a
generic class parameterized with a single Bond-generated type:

```java
Serializer<Record>
```

The constructor of the `Serializer` class takes a `BondType`, which contains the
information necessary to serialize the generated type:

```java
new Serializer<Record>(Record.BOND_TYPE)
// or
new Serializer<Record>(someRecord.getBondType())

serializer.serialize(obj, writer);
```

Deserializer
============

The Bond deserialization API is provided by the `Deserializer` class. It is a
generic class parameterized with a single Bond-generated type:

```java
Deserializer<Record>
```

The constructor of the `Deserializer` class takes a `BondType`, which contains
the information necessary to deserialize the generated type:

```java
deserializer = new Deserializer<Record>(Record.BOND_TYPE)
// or
deserializer = new Deserializer<Record>(someRecord.getBondType())

record = deserializer.deserialize(reader);
```

Deserializing from a payload encoded in an untagged protocol like [Simple
Binary](#simple-binary) requires specifying the schema of the payload. To
address this scenario, `Deserializer.deserialize()` has an overload that takes a
[`RuntimeSchema`](#runtime-schema) as an argument:

```java
RuntimeSchema schema;
// ...
deserializer = new Deserializer<Record>(Record.BOND_TYPE);
record = deserializer.deserialize(reader, schema);
```

See also the following example:

- `examples/java/core/untagged_protocols`

Marshaling
==========

Since Bond supports multiple serialization [protocols](bond_cpp.html#protocols),
application endpoints either have to agree on a particular protocol, or include
protocol metadata in the payload. Marshaling APIs provide the standard way to do
the latter by automatically adding a payload header with the protocol identifier
and version.

The `Marshal` and `Unmarshal` APIs are similar to `Serializer` and
`Deserializer`, except that when calling `Unmarshal` the application simply
provides an input stream with payload data, rather than an instance of a
particular protocol reader:

```java
final Record src = new Record();
src.foo = "foo";
src.constants.add(3.14);
src.constants.add(6.28);

final ByteArrayOutputStream output = new ByteArrayOutputStream();
final CompactBinaryWriter writer = new CompactBinaryWriter(output, 1);

Marshal.marshal(src, writer);

final ByteArrayInputStream input = new ByteArrayInputStream(output.toByteArray());
final Record dst = Unmarshal.unmarshal(input, Record.BOND_TYPE);
```

See also the following example:

- `examples/java/core/marshaling`

Generics
========
Java type erasure makes it necessary to express the parameters of generic Bond
types separately from normal Java declarations. This information is carried in
`BondType` instances, which are  required for all serialization and
deserialization calls. Concrete Bond-generated types provide their `BondType`
instances in the static member `BOND_TYPE`, but generic Bond-generated types
require their parameters to be explicitly passed to their constructors and then
expose their `BondType` instances via the instance method `.getBondType()`.

See the following example:

- `examples/java/core/generics`

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
- Changing of field ordinals
- Changing of inheritance hierarchy (add/remove/substituting base struct)
- Changing between `required` and `optional` directly
- Changing the default value of a field
- Changing existing enumeration constants in any way (including implicit
  renumbering)

Some best practices and other considerations to keep in mind:

- When removing a field, comment it out rather than removing it altogether so
  that the field ordinal is not reused in future edits of the schema
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
field, but does change the generated Java type. Types that would be references
in Java - structs, collections, bondeds, and anything nullable - become
`Something<T>`, while primitives become one of several specializations (e.g.,
`SomethingInteger`). A field with a default of `nothing` that is not present in
a payload will result in a field of the appropriate `Something` type with a null
value.

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

Java reference types are always nullable, so a `nullable<T>` where `T` maps to a
Java reference type will not change the output of code generation. Attemping to
serialize a field whose Bond type is not nullable that has a null value will
result in a runtime exception. Where `T` maps to a Java value type, such as
Bond's `int32` and Java's `int`, code generation will produce a field of the
appropriate boxed reference type (in this case, `Integer`).

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
    (These protocols are not yet supported in Java.)

Compact Binary
--------------

A binary, tagged protocol using variable integer encoding and compact field
header. A good choice, along with [Fast Binary](#fast-binary), for RPC
scenarios.

Implemented in `CompactBinaryReader` and `CompactBinaryWriter` classes.
Version 2 of Compact Binary adds length prefix for structs. This enables
deserialization of [`bonded<T>`](#understanding-bondedt) and skipping of
unknown fields in constant time.

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
offers potential for big saving on payload size. Because Simple Binary is an
untagged protocol, it requires that the payload schema is available during
deserialization. In typical storage scenario application would store a [runtime
schema](#runtime-schema) and pass it to any deserialization calls made later.

Implemented in `SimpleBinaryReader` and `SimpleBinaryWriter` classes.

Version 2 of Simple Protocol uses variable integer encoding for string and
container lengths, resulting in more compact payloads.

See example: `examples/java/core/untagged_protocols`.

Runtime schema
==============

Some generic applications may need to work with Bond schemas unknown at
compile-time. In order to address such scenarios, Bond defines a type
`SchemaDef` to represent schemas at runtime. Applications can obtain an
instance of `SchemaDef` for a particular type through its `BondType`:

```java
// from a concrete class:
final SchemaDef schema = Record.BOND_TYPE.buildSchemaDef();

// from an instance of a concrete or generic type:
final SchemaDef schema = myInstance.getBondType().buildSchemaDef();
```

The `SchemaDef` object is always self contained, including the runtime schema
definitions for all nested types (if any). `SchemaDef` is a Bond type, defined
in `bond.bond`, and as such can be de/serialized like any other Bond type:

```java
serializer.serialize(Record.BOND_TYPE.buildSchemaDef(), writer);
```

A serialized representation of `SchemaDef` can be also obtained directly from
a schema definition IDL file using the [bond
compiler](compiler.html#runtime-schema).

See also the following example:

- `examples/java/core/runtime_schema`

Understanding `bonded<T>`
=========================

The generic type `bonded<T>` is a simple yet powerful abstraction which is a
fundamental part of Bond APIs and enables such usage scenarios as lazy
deserialization, pass-through, and polymorphism.

In Java, `bonded<T>` maps to the `Bonded<T>` abstract class, which supports four
operations: `serialize`, `deserialize`, `convert`, and `cast`. Bond provides
several implementations that represent delayed serialization backed by an
instance or a stream. `Bonded<T>` exposes several static methods that allow the
creation of bonded objects and streams.

Lazy deserialization
--------------------

Because `bonded<T>` can store (or, more accurately, refer to) data representing
a serialized object, it can be used to delay deserialization of some parts of a
payload:

```
struct Example
{
    0: Always always;
    1: bonded<Sometimes> sometimes;
}
```

The schema defined above contains two nested fields. When an object of type
`Example` is deserialized, the field `always` will be fully instantiated and
deserialized, but the field `sometimes`, which is declared as `bonded<Sometimes>`,
will be merely initialized with a reference to its serialized representation.
Applications can then deserialize the object only when needed:

```java
final Example ex = deserializer.deserialize(reader);

// Deserialize sometimes only when needed
if (needSometimes) {
    final Sometimes sometimes = ex.sometimes.deserialize();
}
```

Polymorphism
------------

The type parameter `T` in a `Bonded<T>` is invariant. The instance method
`.cast(BondType)` can be used to upcast and `.convert(BondType)` can be used to
downcast. In both cases, the data backing the `Bonded` is unmodified.

**Warning**: instantiating a `Bonded<Base>` from an instance of `Derived` and then
serializing it will serialize _only_ the `Base` fields.

- `examples/java/core/polymorphic_container`

Codegen parameters
------------------

`--namespace`: Allows mapping Bond namespaces into Java packages. If you have a
Bond file containing a `namespace examples` declaration and want your classes
generated into `org.bondlib.examples`, you can invoke gbc like this:

```
gbc java --namespace="examples=org.bondlib.examples" example.bond
```

Multiple aliases may be given to a single `--namespace` option by separating
them with semicolons.

Platform limitations
====================

Bond for Java currently targets JDK 1.6.

Build instructions
==================

Bond for Java is currently only available in source form. You will need to clone
the Bond repository, install the appropriate dependencies for your platform, and
build and install the Bond compiler. Follow the instructions in the top-level
[README.md](https://github.com/microsoft/bond/blob/master/README.md) to do all
of this.

Java has two additional requirements:

* The Bond compiler, `gbc`, must be in your `PATH`.

  * Linux, macOS: The `make install` step in the `README` will take care of
    this. If you don't want to install Bond into system directories, you can add
    `export PATH=<bond repo>/build/compiler/build/gbc` to your `.bashrc` or
    other shell config file, where `<bond repo>` is the directory you cloned
    Bond into.

  * Windows: You must add the directory containing `gbc.exe` to your `PATH`
    variable. You can do this from a `cmd` window with `setx PATH
    "%PATH%;<bond repo>\build\compiler\build\gbc"`, where `<bond repo>` is the
    directory you cloned Bond into.

* You need the `gradle` build tool, and should get it from the package manager
  for your system. Accordingly:

  * Ubuntu: `sudo apt install gradle`

  * macOS: `brew install gradle`

  * Windows: `choco install gradle`

With all of this done, you're ready to build the Bond library and the Bond
gradle plugin (optional, but strongly recommended). To build the plugin and
install it to your local maven repository:

```
cd java/gradle-plugin; gradle build install
```

To build the library and install it to your local maven repository:

```
cd java/core; gradle build install
```

To consume either component from your local maven repository, see the
[`build.gradle`](https://github.com/microsoft/bond/blob/master/examples/java/core/serialization/build.gradle)
in any of our Java example projects.

Build tooling
=============

We provide a plugin for the `gradle` build tool. Once you've completed all of
the build and installation steps in the [build
instructions](#build-instructions) section, the `gradle` plugin will recursively
discover and compile all Bond files in `src/main/bond` and `src/test/bond` and
add the generated code as production and test sources, respectively. You can
specify Bond files outside those directories and pass options to `gbc` by adding
an explicit `compileBond` or `compileTestBond` container to your build file. You
can see an example of this in the
[`build.gradle`](https://github.com/microsoft/bond/blob/master/java/core/build.gradle)
of the Bond Core library itself.

References
==========

[Bond compiler reference][compiler]
---------------------------

[C++ User's Manual][bond_cpp]
---------------------------

[C# User's Manual][bond_cs]
---------------------------

[Python User's Manual][bond_py]
----------------------------

[bond_cpp]: bond_cpp.html
[bond_cs]: bond_cs.html
[bond_py]: bond_py.html
[compiler]: compiler.html

[compact_binary_format_reference]:
../reference/cpp/compact__binary_8h_source.html

[fast_binary_format_reference]:
../reference/cpp/fast__binary_8h_source.html
