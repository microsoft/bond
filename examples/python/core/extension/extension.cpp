#include "extension_reflection.h"
#include <bond/python/struct.h>

BOOST_PYTHON_MODULE(python_extension)
{
    using namespace example::python;

    // Expose the Example struct to Python.
    // Any other structs or enums referenced by Example are automatically
    // exposed as well.
    bond::python::struct_<Example>()
        .def();
}

