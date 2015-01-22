% Bond F# Type Provider

Introduction
============

The Bond type provider enables F# users to reference serialized Bond runtime 
schemas (SchemaDef) at compile/design time to get a strongly-typed view of 
their data without running Bond code generation. The schema can be read from 
a URL or a local file. The provided types are idiomatic F# immutable types 
using F# collections. 

The type provider integrates with Visual Studio IntelliSense and can be used in 
F# interactive mode, enabling powerful ad hoc data exploration scenarios.

This is an Alpha release of the Bond type provider and is subject to change. 
Some Bond features are [not fully supported](#limitations) yet. Contributions 
are very welcome.

Basic example
=============

The type provider works off a serialized runtime schema and thus doesn't need 
schema source file in Bond IDL format. In typical scenarios the schema is 
stored with the data. The C++, C# and Python Bond implementations provide APIs 
to obtain and serialize/marshal runtime schema ([C#](https://microsoft.github.io/bond/manual/bond_cs.html#runtime-schema), [C++](https://microsoft.github.io/bond/manual/bond_cpp.html#runtime-schema), [Python](https://microsoft.github.io/bond/manual/bond_py.html#exposed-apis)). 

Assuming a file named `example.Record` containing a marshalled `SchemaDef` for 
the following schema: 

    namespace example.schema.serialize;

    struct Const
    {
        0: string name;  
        1: double value;
    }

    struct Example
    { 
        0: string name;
        1: list<Const> consts;
    }

The type provider can be used to operate on the data:

    open System
    open Bond
    open Bond.IO
    open Bond.Protocols
    open Bond.IO.Unsafe
    open Bond.TypeProvider

    // Use type provider to create types from a file with marshaled runtime schema
    type Ty = SchemaTypeProvider< @"example.Record">

    let serialize (obj : Ty.Example) =
        let output = new OutputBuffer()
        let writer = new CompactBinaryWriter<OutputBuffer>(output) 
        obj.SerializeTo(writer)
        output.Data

    let deserialize (data : ArraySegment<byte>) =
        let reader = new CompactBinaryReader<InputBuffer>(new InputBuffer(data))
        Ty.Example.DeserializeFrom(reader)

    [<EntryPoint>]
    let main argv = 
        let src = Ty.Example("math", [Ty.Const("Pi", 3.14); Ty.Const("Tau", 6.28)])
        let dst = serialize src |> deserialize
        printfn "%A" dst
        0 // return an integer exit code

Provided types
==============

A Bond schema with fields f~1~, f~2~, f~3~, ... is represented as the type 
`Tuple<f1',f2',f3',...>`, where `fi'` is derived from f~i~ as follows:


| Bond type        | F# type                                                                      |
|------------------|------------------------------------------------------------------------------|
| Bond structs     | `Tuple<...>`                                                                 |
| Primitive type   | Equivalent CLR primitive type                                                |
| list<T>          | `FSharp.Collections.List<(F# representation of T)>`                          |
| set<T>           | `FSharp.Collections.Set<(F# representation of T)>`                           |
| map<K, V>        | `FSharp.Collections.Map<(F# representation of K), (F# representation of V)>` |

The provided types have a default constructor, which initializes all fields to 
their defaults defined in the schema, as well as constructor taking an argument 
for each fields.

Provided methods
----------------

An instance method `SerializeTo` serializes instance of provided type using 
specified protocol:

    let writer = new CompactBinaryWriter<OutputBuffer>(output) 
    obj.SerializeTo(writer)

A class method `DeserializeFrom` constructs a new object initialized from the 
serialized data and returns it to the caller.

    let reader = new CompactBinaryReader<InputBuffer>(data)
    let obj = Prov.Record.DeserializeFrom(reader) 


Limitations
===========

The current version of the type provider has some limitations.

1. Schema inheritance is not supported yet
2. Schemas without any fields are not supported

    The provided types use Tuple for their representation and there is no 
    0-Tuple. We may change the type provider to use () as 0-tuple.

3. Nullable types are represented as lists

    When serialized, nullable types are represented as lists with zero or one 
    element, and the `SchemaDef` representation of a nullable type is list<T>. 
    In the future when we extend `SchemaDef` with information about nullable 
    fields and value, type provider will be able to map them to nullable type 
    in the CLR types system.

4. Default value of `nothing` is not supported

    Fields with default value of 'nothing' should be represented by a nullable 
    type in order to be able to represent `nothing`. In the current version 
    provided types will map `nothing` to the implicit default for the field 
    type.

