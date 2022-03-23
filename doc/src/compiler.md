% Bond compiler

Command line options
====================

<div class="sourceCode"><object width="100%" height="600" data="gbc.html"/></div>

IDL syntax
==========

A Bond schema definition file can contain the following elements:

- Import statements
- Namespace definition
- Declarations
    - enum
    - forward declaration
    - struct
    - generic structs
    - type aliases
    - services
    - generic services
    - struct view
- Custom attributes
- Comments


Import statements
------------------

In order to use types defined in another schema definition file, the other file
needs to be explicitly imported. Schema file can contain zero or more import
statements and they must appear at the top of the file:

```
import "file.bond"
```

The file being imported can be specified using a partial path which is resolved
by Bond compiler relative to the directory containing the schema file being
compiled and any import path(s) specified using the --import-dir option(s)
passed to gbc.

See examples:

- `examples/cpp/core/import`
- `examples/cs/core/import`

Namespace definition
--------------------

All schema types are always defined within a namespace. Namespace scope starts
with namespace definition statement and ends at the end of the file.

```
namespace some.unique.name
```

Enum definition
---------------

Bond enums are very similar to C++ enums, both in semantics and syntax used to
define them:

```
enum Protocols
{
    TCP,
    UDP = 10
}
```

On the wire values of enums types are equivalent to 32-bit signed integers.


Forward declaration
-------------------

In order to define recursive schemas, such as trees, it may be necessary to
declare a struct before it is defined. A forward declaration statement serves
this purpose:

```
struct Node;
```

Forward declared structs can be used in field declarations as the base type for
nullable\<T\> and bonded\<T\> or the element type of a container.

```
struct Node;

struct Node
{
    0: nullable<Node> left;
    1: nullable<Node> right;
}
```

Struct definition
-----------------

Struct definition consists of a struct name, an optional base struct, and zero
or more fields.

```
struct Example : Base
{
    0: uint32 fieldName = 10;
}
```

Field definition consists of an ordinal/id, type, name, and a default value.
The fields type's can be:

- Basic type: `bool`, `uint8`, `uint16`, `uint32`, `uint64`, `int8`, `int16`,
`int32`, `int64`, `float`, `double`, `string`, `wstring`.

- Container: `blob`, `list<T>`, `vector<T>`, `set<T>`, `map<K, T>`,
`nullable<T>`.

- User-defined type: enum, struct or `bonded<T>` where T is a struct.

An optional default value can be specified for fields of basic types. For
integers the default can be specified as either a decimal number or a
hexadecimal number prefixed with `0x`. The only explicit default value allowed
for containers is [`nothing`](#default-value-of-nothing). Enum fields _must_ have
an explicit default value which must be one of the enum named constants or
[`nothing`](#default-value-of-nothing).

Names of structs and enums defined in another namespace must be qualified with
the namespace name:

```
import "bond/core/bond.bond"

namespace example

struct Example
{
    0: bond.GUID id;
    1: bond.BondDataType type = BT_UNAVAILABLE;
}
```

Generic struct
--------------

Generic structs are parameterized with one or more type parameters which can be
used within the struct definition in any place where a concrete type could be
used (e.g. base struct, field type, container element type, parameter of a
generic struct).

```
struct Example<T1, T2> : T1
{
    0: T1 field;
    1: list<T2> list;
    2: Generic<T2> generic;
}
```

The usage of a type parameter within a generic struct definition may implicitly
constrain what type(s) can be used to instantiate the generic struct:

```
struct Example<T>
{
    // The default value of 10 implicitly constrains T to numeric types
    0: T x = 10;
}
```

Using a type parameter in a [`nullable`](#nullable-types) or as the type of
a field with default value of [`nothing`](#default-value-of-nothing) constrains
the type parameter to be non-scalar type. If this is undesired then explicit
constraint to value type can be specified in the generic schema definition:

```
struct Example<T : value>
{
    0: nullable<T> x;
    1: T y = nothing;
}
```

When instantiating a generic struct all type parameters must be concrete types.
Bond IDL doesn't support the equivalent of C++ template template parameters.

See examples:

- `examples/cpp/core/generics`
- `examples/cs/core/generics`
- `examples/cpp/core/generic_tree`


Type aliases
------------

The syntax to define type aliases is very similar to C++:

```
using time = int64;
using array<T> = vector<T>;
```

An alias can be used in any context where the aliased type could be used,
including a definition of another alias:

```
using times = array<time>;
```

Type aliases can optionally be mapped to custom types in the generated code
for both [C++](bond_cpp.html#custom-type-mappings) and
[C#](bond_cs.html#custom-type-mappings).

See examples:

- `examples/cpp/core/time_alias`
- `examples/cpp/core/multiprecision`
- `examples/cpp/core/string_ref`
- `examples/cpp/core/container_of_pointers`
- `examples/cpp/core/static_array`
- `examples/cs/core/date_time`
- `examples/cs/core/decimal`
- `examples/cs/core/guid`
- `examples/cs/core/container_alias`

Struct views
------------

A view definition is syntactic sugar to define a struct that has a subset of
the fields of another struct:

```
struct Example
{
    0: int16 x;
    1: string y;
    2: list<bool> z;
}

struct View view_of Example
{
    x,z;
}
```

In the above example, the definition of `View` is equivalent to:

```
struct View
{
    0: int16 x;
    2: list<bool> z;
}
```

A view of a generic struct is also a generic struct with the same number of
type parameters.

See example: `examples/cpp/core/schema_view`

Service definition
------------------

**Bond-over-gRPC is deprecated and will be completely removed from Bond in
May 2022.** See GitHub issue [\#1131, Bond-over-gRPC will be deprecated
February 2022](https://github.com/microsoft/bond/issues/1131), for full
details. `gbc` will still be able to parse service definitions and emit them
in JSON AST, but C++ and C# codegen will be removed in May 2022.

A service definition consists of a service name and methods:

```
service Calculator
{
    Result Calculate(Operation);
    void Configure(Settings);
    Stats GetStats();

    Result RunningSum(stream Operation);
    stream Result ComputePrimes(Limit);
    stream Result StartRPNSession(stream Operation);
}
```

Methods take as input and return as results Bond
[structs](#struct-definition).

There are five different kinds of service methods:

1. unary: take one parameter and return one result
1. client streaming: take a sequence of parameters and return one result
1. server streaming: take one parameter and return a sequence of results
1. duplex streaming: take a sequence of parameters and return a sequence of
   results
1. one-way events: take one parameter and do not return anything

For both parameters and results, `void` can be used to indicate that there
is no meaningful data to be passed/returned. An empty payload is sent for a
`void` parameter or result. A method that omits its parameter has an
implicit `void` parameter. (While both forms are legal, `GetStats()` is the
preferred syntax over `GetStats(void)`.)

Streaming parameters or results are indicate by using the `stream` keyword
before the type name in either the parameter or result position: `stream
Operation`. Note that `stream void` is not legal. If you need to send/return
a stream of empty payloads, use a stream of some empty struct: e.g., `stream
bond.Void`, which uses the pre-defined `bond.Void` struct in bond.bond.

For duplex streaming methods, the parameters and results do not have to have
a 1:1 correspondence. The server is free to return results at any time
during the lifetime of the method invocation.

NB: Streaming methods are currently only supported when generating C# and
using Bond-over-gRPC.

Methods with a result of `nothing` are one-way, fire and forget methods: the
service doesn't send any response regardless of whether the service method
execution resulted in success or failure. Furthmore, the client will not
report transport errors to the application. There is no way to tell whether
an event ever left the application process, let alone was received and
processed by the serivce. This is different from methods returning `void`
which send back a response with an empty payload and may indicate failure
using errors.

```
service Watchdog
{
    nothing Heartbeat(Identifier);
}
```

Streaming parameters cannot be used with a `nothing` result.


Generic service
---------------

Generic services are parameterized with one or more type parameters which can be
used within the service definition in any place where a concrete type could be
used.

```
service Gateway<Token>
{
    Result Authenticate(Token);
}
```

The usage of a type parameter within a generic service definition may implicitly
constrain what type(s) can be used to instantiate the generic service. For
example in the above definition the `Token` type parameter must be
a [struct](#struct-definition).

Custom attributes
-----------------

Struct, service, enum as well as struct field and service method definitions
can be annotated with custom attributes which are in effect name, string-values
pairs:

```
[Validate("True")]
struct Example
{
    [Max("100")]
    0: uint32 value;
}

[service_id("MyExample")]
service Example
{
    [Tracing("enabled")]
    Result Method(Param);
}
```

Attributes are available in code generation templates and thus can be used to
drive custom code generation. They are also available to applications via
[compile-time](#compile-time-schema) and [runtime](#runtime-schema) schema, and
as `Metadata` argument in [transforms](#transforms) and [protocols](#protocols).

See example: `examples/cpp/core/attributes`


Comments
--------

Bond IDL supports C++ style comments:

```
/*
    Multi-line
    comment
*/
struct Example
{
    // One line comment
}
```

Schema AST
==========

The compiler exposes a JSON representation of the schema Abstract Syntax Tree.
The AST is intended for tools that need to access to the schema information
contained in Bond IDL files with the full fidelity. The compiler can also take
the JSON representation of the AST as an input, enabling tools which
programmatically construct/modify Bond schemas.

Example
-------

Given the following schema definition:

```
namespace example.some

struct SomeStruct
{
    0: int32 someField = 123;
}
```

Below is the JSON representation of the schema's Abstract Syntax Tree generated
using `gbc schema example.bond` command:

```javascript
{
    "imports": [],
    "namespaces": [
    {
        "name": [
            "example",
            "some"
        ]
    }
    ],
    "declarations": [
    {
        "tag": "Struct",
        "declNamespaces": [
        {
            "name": [
                "example",
                "some"
            ]
        }
        ],
        "declAttributes": [],
        "declParams": [],
        "declName": "SomeStruct",
        "structFields": [
        {
            "fieldOrdinal": 0,
            "fieldType": "int32",
            "fieldName": "someField",
            "fieldDefault": {
            "value": 123,
            "type": "integer"
            }
        }
        ]
    }
    ]
}
```

Bond
----

The top level JSON object represents the parsed Bond IDL file and has the
following structure:

```javascript
{
    "imports": [
    ],
    "namespaces": [
    ],
    "declarations": [
    ]
}
```

where:

- `imports` is an array of [imports](#import).
- `namespaces` is an array of [namespaces](#namespace). Each Bond file should
have one namespace declaration, although the AST and IDL syntax have support
for legacy schema files with multiple, language-specific namespaces.
- `declarations` is an array of [declarations](#declaration).

Import
------

Imports are represented by JSON strings. For example the following IDL:

```
import "bond/core/bond.bond"
```

is represented in the AST as:

```
"bond/core/bond.bond"
```

Namespace
---------

The namespace declaration is represented by a JSON object:

```javascript
{
    "name": [
    ]
}
```

where:

- `name` is [qualified name](#qualified-name) of the namespace.

For example:

```
namespace foo.bar
```

is represented as:

```javascript
{
    "name": [
    "foo",
    "bar"
    ]
}
```

Declaration
-----------

A declaration is represented by a JSON object with the following common properties:

```javascript
{
    "tag": "Tag",
    "declNamespaces": [
    ],
    "declName": "Name",
    "declParams": [
    ],
    "declAttributes": [
    ]
}
```

where:

- `tag` is a string indicating the type of the declaration. It can have one of
the following values: `"Struct"`, `"Enum"`, `"Alias"`, `"Forward"`, `"Service"`,
`"Function"`, `"Event"`.
- `declNamespaces` is an array of one or more [namespaces](#namespace).
- `declName` is a string.
- `declParams` is an array of zero or more [type parameters](#type-parameter).
The property doesn't apply to [`Enum`](#enum) declarations.
- `declAttributes` is an array of zero or more [attributes](#attribute). The
property doesn't apply to [`Forward`](#forward-declaration) declarations.

### Struct

A JSON object representing a `Struct` declaration has the following properties:

```javascript
{
    "tag": "Struct",
    "declNamespaces": [
    ],
    "declName": "StructName",
    "declParams": [
    ],
    "declAttributes": [
    ],
    "structBase": null,
    "structFields": [
    ]
}
```

where:

- `structBase` is `null` or a [type](#type) representing the struct base. The
property is optional and may be omitted.
- `structFields` is an array of zero or more [fields](#struct-field).

### Service

A JSON object representing a `Service` declaration has the following properties:

```javascript
{
    "tag": "Service",
    "declNamespaces": [
    ],
    "declName": "ServiceName",
    "declParams": [
    ],
    "declAttributes": [
    ],
    "serviceMethods": [
    ]
}
```

where:

- `serviceMethods` is an array of zero or more [methods](#methods).

#### Methods

A JSON object representing a `Function` declaration has the following properties:

```javascript
{
    "tag": "Function",
    "methodName": "methodName",
    "methodAttributes": [
    ],
    "methodResult": {
    },
    "methodInput": {
    }
}
```

where:

- `methodResult` is a user defined type struct, alias to struct, parameter or null.
- `methodInput` is a user defined type struct, alias to struct, parameter or null.
- `methodAttributes` is an array of zero or more [attributes](#attribute)

A JSON object representing an `Event` declaration has the following properties:

```javascript
{
    "tag": "Event",
    "methodName": "methodName",
    "methodAttributes": [
    ],
    "methodInput": {
    }
}
```

where:

- `methodInput` is a user defined type struct, alias to struct, parameter or null.
- `methodAttributes` is an array of zero or more [attributes](#attribute)

### Enum

A JSON object representing an `Enum` declaration has the following properties:

```javascript
{
    "tag": "Enum",
    "declNamespaces": [
    ],
    "declName": "EnumName",
    "declAttributes": [
    ],
    "enumConstants": [
    ]
}
```

where:

- `enumConstants` is an array of one or more [constants](#constant).

#### Constant

An enum constant is represented by the following JSON object:

```javascript
{
    "constantName": "ConstantName",
    "constantValue": null
}
```

where:

- `constantName` is a string.
- `constantValue` is an integer or `null` if the value is not explicitly
defined. The property is optional and may be omitted.

### Type alias

A JSON object representing a type alias declaration has the following properties:

```javascript
{
    "tag": "Alias",
    "declNamespaces": [
    ],
    "declName": "AliasName",
    "declParams": [
    ],
    "aliasType": {
    }
}
```

where:

- `aliasType` is the aliased [`type`](#type).

### Forward declaration

A JSON object representing a forward declaration has the following properties:

```javascript
{
    "tag": "Forward",
    "declNamespaces": [
    ],
    "declName": "StructName",
    "declParams": [
    ]
}
```

Qualified name
--------------

Qualified names are represented in JSON by an array of strings. For example:

```
foo.bar
```

is represented by:

```javascript
[ "foo",
    "bar"
]
```

Type parameter
--------------

Type parameters are represented by JSON objects with the following properties:

```javascript
{
    "paramName": "T",
    "paramConstraint": null
}
```

where:

- `paramName` is a string.
- `paramConstraint` is `null` or the string `"value"`. The property is optional
and may be omitted.

Attribute
---------

Attributes are represented by JSON objects with the following properties:

```javascript
{
    "attrName": [
    ],
    "attrValue": "Value"
}
```

where:

- `attrName` is a [qualified name](#qualified-name).
- `attrValue` is a string.

Struct field
------------

A struct field is represented by a JSON object with the following properties:

```javascript
{
    "fieldModifier": "Optional",
    "fieldDefault": null,
    "fieldType": {
    },
    "fieldName": "name",
    "fieldAttributes": [
    ],
    "fieldOrdinal": 0
}
```

where:

- `fieldModifier` is one of the following strings: `"Optional"`, `"Required"`,
`"RequiredOptional"`. The property is optional and `fieldModifier` defaults to
`Optional` if omitted.
- `fieldDefault` is `null` or a [default value](#field-default-value). The
property is optional and may be omitted.
- `fieldType` is a [type](#type).
- `fieldName` is a string.
- `fieldAttributes` is an array of zero or more [attributes](#attribute). The
property is optional an may be omitted.
- `fieldOrdinal` is an integer.

### Field default value

A field default value is represented by a JSON object with the following properties:

```javascript
{
    "type": "enum",
    "value": "Value2"
}
```

where:

- `type` is one of the following strings: `"enum"`, `"bool"`, `"integer"`,
`"float"`, `"string"`, `"nothing"`.
- `value` is a value appropriate for the type. The `value` property is not used
when `type` is `"nothing"`.

Type
----

### Basic types

Basic types are represented by JSON strings:

```javascript
"int8"
"int16"
"int32"
"int64"
"uint8"
"uint16"
"uint32"
"uint64"
"float"
"double"
"bool"
"string"
"wstring"
"blob"
```

### Complex types

Complex types are represented by JSON objects with a `type` property indicating
the complex type. If the `type` property is one of the following: `"vector"`,
`"list"`, `"set"`, `"nullable"`, `"maybe"`, `"bonded"` then the object has the
following structure:

```javascript
{
    "type": "vector",
    "element": {
    }
}
```

where `element` is the [type](#type) of the element (or the nested type).

Other complex types are:

- map

    ```javascript
    {
      "type": "map",
      "key": {
      },
      "element": {
      }
    }
    ```

    where:

    - `key` is the [type](#type) of the map key.
    - `element` is the [type](#type) of map value.

- type parameter

    ```javascript
    {
      "type": "parameter",
      "value": {
      }
    }
    ```

    where `value` is a [type parameter](#type-parameter).

- numeric type argument

    ```javascript
    {
      "type": "constant",
      "value": 0
    }
    ```

    where `value` is an integer.

- user defined type

    ```javascript
    {
      "type": "user",
      "declaration": {
      },
      "arguments": [
      ]
    }
    ```

    where:

    - `declaration` is a [declaration](#declaration) of a user defined type.
    - `arguments` is an array of zero or more [types](#type) representing type
    arguments for a generic user defined type. The property is optional and
    may be omitted for non-generic types.

Service method
--------------

A JSON object representing a `Method` has the following properties:

    {
      "tag": "Tag",
      "methodName": "MethodName",
      "methodAttributes": [
      ],
      "methodResult": {
      },
      "methodInput": {
      }
    }

where

- `tag` is one of the following string values:
    - `"Sink"` to represent a one-way method that doesn't return any result.
    - `"Function"` to represent a method that returns a result.
- `methodName` is a string.
- `methodAttributes` is an array of zero or more [attributes](#attribute).
- `methodResult` is an object representing [message](#message) returned from
the method as the result.
- `methodInput` is an object representing [message](#message) accepted by the
method as the input.

### Message

A JSON object representing a `Message` has the following properties:

    {
      "messagePayload": {
      },
      "messageService": {
      }
    }

where

- `messagePayload` is `null` or an object representing [type](#type) of data
payload carried by the message. The type must be a user defined struct.
- `messageService` is `null` or an object representing [type of a service](#service-type)
that can be passed in the message, e.g. as a callback.

### Service type

Service type can be specified using one of the following JSON objects:

    {
      "typeParam": {
      }
    }

or

    {
      "declaration": {
      },
      "arguments": {
      }
    }

where

- `typeParam` is an object representing [type parameter](#type-parameter).
- `declaration` is a [service declaration](#service).
- `arguments` is an array of zero or more [types](#type) representing type
arguments for a generic service. The property is optional and may be omitted
for non-generic services.

Runtime Schema
==============

Bond defines `SchemaDef` structure to represent Bond schemas at runtime.
`SchemaDef` is accepted as an argument by various Bond APIs. For example when
transcoding binary payload into a text protocol like JSON, the `SchemaDef` of
the payload is used to provide the necessary meta-data such as names of the
fields.

Usually `SchemaDef` is produced at runtime using Bond APIs. However in some
scenarios it may be desirable to be able to obtain `SchemaDef` object(s)
directly from a schema definition IDL file. The compiler can generate
`SchemaDef` serialized in Simple JSON protocol which can be deserialized using
standard Bond APIs.

Example
-------

Given the following schema definition contained a file `example.bond`:

```
namespace example.some

struct SomeStruct
{
    0: int32 someField = 123;
}
```

The command `gbc schema --runtime-schema example.bond` would produce a file
named `example.SomeStruct.json` with the following content:

```javascript
{
    "structs": [
    {
        "metadata": {
        "qualified_name": "example.some.SomeStruct",
        "name": "SomeStruct"
        },
        "fields": [
        {
            "metadata": {
            "default_value": {
                "int_value": 123
            },
            "name": "someField"
            },
            "id": 0,
            "type": {
            "id": 16
            }
        }
        ]
    }
    ]
}
```

Library
=======

The Bond IDL compiler and codegen functionality is accessible programatically
via the [bond](https://hackage.haskell.org/package/bond) Haskell package. The
library can be used to implement custom codegen tools. See examples/codegen in
the repository.
