% Python bindings for Bond

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
for C++, C#, Java, and Python on Linux, macOS, and Windows.

Bond is published on GitHub at [https://github.com/microsoft/bond/](https://github.com/microsoft/bond/).

Introduction
============

The Python support is implemented as a library for building C++ extensions 
exposing Bond generated types and associated APIs to Python.

Basic example
=============

We start with defining a data schema using Bond idl language:

```
namespace example

struct Record
{
    0: string          name;
    1: vector<double>  items;
};
```

In order to use the schema in a Python program we need to implement a Python 
extension that exposes types representing the schema. The first step is to 
generate Bond C++ bindings for the schema using the Bond compiler:

```
gbc c++ example.bond
```

With the generated C++ code (`example_reflection.h` in this case) implementing 
the Python extension is as simple as specifying which structs we want to expose 
to Python:

```cpp
#include "example_reflection.h"
#include <bond/python/struct.h>

BOOST_PYTHON_MODULE(example)
{
    bond::python::struct_<example::Record>()
        .def();
}
```

Finally, we can import the extension as a module that can be used in a Python 
program:

```python
import example

src = example.Record()

src.name = "test"
src.items = [3.14, 6.28]

data = example.Serialize(src)

dst = example.Record()
example.Deserialize(data, dst)
```

Building extensions
===================

Python extensions are simply shared libraries with the same file name as the 
module name. On Windows the extension should have .pyd file extension. Bond 
extensions depend on [Boost Python][boost_python] library, and thus need to be 
linked with both the Boost Python library and the Python library. Refer to 
[Boost documentation][boost_python_build] for details.

Exposing types
==============

Structs
-------

Structs generated from Bond schemas can be exposed by calling `def` method of 
the `bond::python::struct_` class template within a `BOOST_PYTHON_MODULE` 
block. The template takes one parameter: the C++ struct to be exposed. The 
`def` method has two overloads. When `def` if called without any arguments the 
struct is exposed using unqualified name. To expose a struct using the fully 
qualified name, the `def` method can be called with `bond::qualified_name` as 
argument.

If a struct is exposed, all of its dependencies are implicitly exposed as well 
and often don't need to be exposed explicitly. Note however that 
[APIs](#exposed-apis) are defined only for explicitly exposed types. Implicitly 
exposed types always use unqualified name. If it is necessary to use the 
qualified name for a struct, it should be explicitly exposed *before* its 
dependent structs.

```cpp
BOOST_PYTHON_MODULE(example)
{
    using namespace bond::python;

    struct_<example::Nested>()
        .def(bond::qualified_name);

    struct_<example::Example>()
        .def();
}
```

Enums
-----

Since enums are almost always used in exposed structs, usually it is not 
necessary to expose them explicitly. One common exception, as was the case with 
structs, is when we want to explicitly expose an enum using the qualified name. 

Enumerator types generated from Bond schema can be exposed by calling `def` 
method of the `bond::python::enum_` class template from a `BOOST_PYTHON_MODULE` 
block. The template takes one parameter: the C++ enum to be exposed. The `def` 
method has two overloads. When `def` is called without any arguments the enum 
is exposed using unqualified name. To expose an enum using the fully qualified 
name, the `def` method can be called with `bond::qualified_name` as argument.

```cpp
BOOST_PYTHON_MODULE(example)
{
    using namespace bond::python;

    enum_<example::Colors>()
        .def(bond::qualified_name);

    struct_<example::Example>()
        .def();
}
```

Containers
----------

Any container classes used in exposed schema structs are automatically exposed 
to Python. The exposed types behave just like Python built-in equivalent 
containers, supporting Python style indexing, including slices and negative 
indices, list/set/dictionary comprehension as well as many of the built-in 
functions and operations like `len()`, `del` and `in`. Built-in container 
objects also are implicitly convertible to containers exposed by Bond. 

There is a noteworthy difference between Bond `map` and Python `dictionary`: 
iterating over elements of a `map` doesn't yield tuples `(key, value)` but 
instead objects with two methods: `key()` and `data()`.

Blob
----

Bond `blob` is represented as either a string object in Python2 or a bytes
object in Python3. Initializing a `blob` from a Python object does not involve
a memory copy; instead, the reference count on the underlying Python object is
increased.

Nullable and `nothing`
----------------------

The `null` value for nullable types and default value of `nothing` are both 
mapped to the Python `None` object. For example, given the schema:

```
struct Record
{
    0: int32 x = nothing;
    1: nullable<string> s;
}
```

We can write the following Python program:

```python
obj = example.Record()

assert(obj.x is None)
assert(obj.s is None)

x = 100
s = "test"

assert(obj.x is not None)
assert(obj.s is not None)
```

Generics
--------

While generic Bond schemas can not be instantiated from within a Python 
program, specific instances of generic schemas can be used like any other 
concrete structs. In fact any instances of a generic schema used within exposed 
structs are also implicitly exposed, just like any other nested struct.

```
namespace generic

struct Generic<T>
{
    0: T field;
}

struct Example
{
    0: Generic<string> field;
}
```

For example, an extension exposing the struct `Example` will also automatically 
expose the instance `Generic<string>`. Additionally, we can explicitly expose 
other instances of `Generic<T>`:

```cpp
BOOST_PYTHON_MODULE(example)
{
    // Expose Example and implicitly Generic<string>
    bond::python::struct_<generic.Example>()
        .def();

    // Explicitly expose Generic<bond.GUID>
    bond::python::struct_<generic.Generic<bond::GUID> >()
        .def();
}
```

The name of a generic schema instance is converted to a valid Python identifier 
by replacing all non-alphanumeric characters with an underscore. The names of 
type parameters that are Bond-defined structs are used in their fully qualified 
form. For example, using the extension defined above we can use the following 
types:

```python
import example

# The instance Generic<string>
obj1 = example.Generic_string_()

# The instance Generic<bond.GUID>
obj2 = example.Generic_bond_GUID_()
```

Exposed APIs
============

For every exposed schema struct Bond exposes appropriate overloads of the 
following API functions:

  - `Serialize` and `Marshal`

    Take as an argument instance of Bond struct and return a Python string with 
    serialized/marshaled data. 

  - `Deserialize` and `Unmarshal`

    Take two arguments, a Python string object containing serialized/marshaled 
    data and a reference to a Bond struct to which the data should be 
    deserialized.
 
  - `GetRuntimeSchema`

    Takes as an argument a Bond struct and returns an instance of SchemaDef for 
    that struct.

The `Serialize`, `Deserialize` and `Marshal` APIs use the Compact Binary 
protocol by default but they also take an optional argument of type 
`bond::ProtocolType` to specify a different protocol. 

The `Deserialize` and `Unmarshal` APIs take an optional argument of type 
`SchemaDef` to specify the schema of the serialized data.

```python
import example

obj = example.Record()

# serialize to Compact Binary
data = example.Serialize(obj)

# serialize to JSON
json = example.Serialize(obj, example.ProtocolType.SIMPLE_JSON_PROTOCOL)

# marshal schema to Compact Binary
data = example.Marshal(example.GetRuntimeSchema(obj))

# unmarshal SchemaDef
schema = example.SchemaDef()
example.Unmarshal(data, schema)

# deserialize from Simple Protocol with runtime schema
example.Deserialize(data, obj, schema, example.ProtocolType.SIMPLE_PROTOCOL)
```

References
==========

[Bond compiler reference][compiler]
---------------------------

[C++ User's Manual][bond_cpp]
-----------------------------

[C# User's Manual][bond_cs]
---------------------------

[Boost Python][boost_python]
----------------------------

[compiler]: compiler.html

[bond_cpp]: bond_cpp.html

[bond_cs]: bond_cs.html

[boost_python]: http://www.boost.org/doc/libs/1_54_0/libs/python/doc/index.html

[boost_python_build]: http://www.boost.org/doc/libs/1_54_0/libs/python/doc/building.html
