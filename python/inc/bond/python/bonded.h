// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "converters.h"
#include <bond/core/bond.h>

namespace bond
{
namespace python
{


template <typename T, bool API = true>
class struct_;


// Visitor defining bonded<T> casts for base schema
template <typename T>
class schema_base_visitor
    : public boost::python::def_visitor<schema_base_visitor<T> >
{
    friend class boost::python::def_visitor_access;

    template <typename classT>
    void visit(classT& c) const
    {
        def_init(c, base_class<typename T::Schema>());
    }

    template <typename classT, typename Base>
    void def_init(classT& c, const Base*) const
    {
        c.def(boost::python::init<const bonded<Base>&>());
        def_init(c, base_class<typename Base::Schema>());
    }

    template <typename classT>
    void def_init(classT&, const no_base*) const
    {
    }
};


// Helper class used to expose a bonded<T> to Python
template <typename T>
class bonded_
{
public:
    void def()
    {
        def_bonded(make_pythonic_name<bonded<T> >());
    }

private:
    void def_bonded(const pythonic_name& name)
    {
        // Expose bonded<T> type
        boost::python::class_<bonded<T> >(name)
            .def(boost::python::init<const T&>())
            .def(schema_base_visitor<T>())
            .def("Deserialize",
                static_cast<void (bonded<T>::*)(T&) const>(&bonded<T>::template Deserialize<BuiltInProtocols, T>));

        // Expose struct T to Python
        struct_<T>()
            .def();
    }
};

} // python
} // bond
