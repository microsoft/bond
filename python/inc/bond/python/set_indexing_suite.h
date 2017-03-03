// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "list_indexing_suite.h"

namespace bond
{
namespace python
{

// Indexing suite for std::set

template <typename T, bool NoProxy, typename DerivedPolicies>
class set_indexing_suite;

namespace detail
{
    template <typename T, bool NoProxy>
    class final_set_derived_policies
        : public bond::python::set_indexing_suite<
            T,
            NoProxy,
            final_set_derived_policies<T, NoProxy> >
    {};
}

template <
    typename T,
    bool NoProxy = false,
    typename DerivedPolicies = detail::final_set_derived_policies<T, NoProxy>
>
class set_indexing_suite
    : public list_indexing_suite<T, NoProxy, DerivedPolicies>
{
    typedef list_indexing_suite<T, NoProxy, DerivedPolicies> base;
public:
    typedef typename base::data_type data_type;
    typedef typename base::index_type index_type;
    typedef typename base::key_type key_type;

    template <class Class>
    static void
    extension_def(Class& class_)
    {
        class_
            .def("add", &function<DerivedPolicies::add>)
            .def("remove", &function<DerivedPolicies::remove>)
            .def("discard", &function<DerivedPolicies::discard>)
            .def("clear", &DerivedPolicies::clear)
        ;
    }

    static bool
    contains(T& set, key_type const& key)
    {
        return set.find(key) != set.end();
    }

    static void
    add(T& set, data_type const& v)
    {
        set.insert(v);
    }

    static void
    discard(T& set, data_type const& v)
    {
        set.erase(v);
    }

    static void
    remove(T& set, data_type const& v)
    {
        if (!set.erase(v))
        {
            PyErr_SetString(PyExc_KeyError, "Element doesn't exist");
            boost::python::throw_error_already_set();
        }
    }

    static void
    clear(T& set)
    {
        set.clear();
    }

    static data_type
    get_item(T&, index_type)
    {
        not_supported();
        return data_type();
    }

    static void
    set_item(T&, index_type, data_type const&)
    {
        not_supported();
    }

    static void
    set_slice(T&, index_type, index_type, data_type const&)
    {
        not_supported();
    }

    template <typename Iter>
    static void
    set_slice(T&, index_type, index_type, Iter, Iter)
    {
        not_supported();
    }

private:
    template <void (*fn)(T&, data_type const&)>
    static void
    function(T& set, const boost::python::object v)
    {
        using namespace boost::python;

        extract<data_type&> elemRef(v);

        if (elemRef.check())
        {
            fn(set, elemRef());
        }
        else
        {
            extract<data_type> elem(v);

            if (elem.check())
            {
                fn(set, elem());
            }
            else
            {
                PyErr_SetString(PyExc_TypeError, "Invalid type");
                throw_error_already_set();
            }
        }
    }

    static void
    not_supported()
    {
        PyErr_SetString(PyExc_TypeError, "__getitem__ and __setitem__ not supported for set object");
        boost::python::throw_error_already_set();
    }
};

} // namespace python

} // namespace bond
