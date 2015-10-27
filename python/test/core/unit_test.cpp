#include "unit_test_reflection.h"
#include <bond/python/struct.h>

BOOST_PYTHON_MODULE(python_unit_test)
{
    using namespace unittest;
    using namespace bond::python;

    bonded_<SimpleStruct>()
        .def();

    struct_<SimpleContainers>()
        .def();

    struct_<NestedContainers>()
        .def();

    struct_<Nullable>()
        .def();

    struct_<Nothing>()
        .def(bond::qualified_name);

    struct_<Generic<SimpleStruct> >()
        .def();

    struct_<Bonded>()
        .def();

    struct_<NestedWithBase>()
        .def();

    bonded_<SimpleWithBase>()
        .def();
}
