Introduction                         {#mainpage}
============

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

Basic example
=============

In Bond data schemas are defined using idl-like 
[syntax](../../manual/compiler.html#idl-syntax):

    namespace example

    struct Record
    {
        0: string          name;
        1: vector<double>  items;
    }

In order to use the schema in a C++ program, it needs to be compiled using the
Bond compiler [`gbc`](gbc.html). This step is sometimes also referred to as 
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

User's Manual
-------------

For more information see the [User's Manual](../../manual/bond_cpp.html).

