// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "converters.h"
#include <bond/core/bond.h>

namespace bond
{
namespace python
{


// Helper class used to expose a Bond generated enum to Python
template <typename T>
class enum_
{
public:
    void def()
    {
        def_enum(GetTypeName(T()));
    }

    void def(const qualified_name_tag&)
    {
        def_enum(GetTypeName(T(), qualified_name));
    }

private:
    void def_enum(const pythonic_name& name)
    {
        const auto* reg = boost::python::converter::registry::query(typeid(T));

        if (reg && reg->m_class_object)
            return;

        boost::python::enum_<T> e(name);

        auto values = bond::GetEnumNames<T>();

        for (auto it = values.begin(); it != values.end(); ++it)
            e.value(it->first.c_str(), it->second);
    }
};

} // python
} // bond
