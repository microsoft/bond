// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/python/suite/indexing/vector_indexing_suite.hpp>

namespace bond
{
namespace python
{
            
// Indexing suite equivalent to boost::python::vector_indexing_suite but for
// containers that don't support random access iterator.

template <typename T, bool NoProxy, typename DerivedPolicies>
class list_indexing_suite;

namespace detail
{
    template <typename T, bool NoProxy>
    class final_list_derived_policies 
        : public bond::python::list_indexing_suite<
            T, 
            NoProxy, 
            final_list_derived_policies<T, NoProxy> > 
    {};
}

template <
    typename T, 
    bool NoProxy = false,
    typename DerivedPolicies = detail::final_list_derived_policies<T, NoProxy>
>
class list_indexing_suite 
    : public boost::python::vector_indexing_suite<T, NoProxy, DerivedPolicies>
{
    typedef boost::python::vector_indexing_suite<T, NoProxy, DerivedPolicies> base;
public:
    typedef typename base::data_type data_type;
    typedef typename base::index_type index_type;
    typedef typename base::key_type key_type;

    static 
    typename std::conditional<
        std::is_class<data_type>::value,
        data_type&,
        data_type
    >::type
    get_item(T& list, index_type i)
    { 
        return *advance(list.begin(), i);
    }

    static void 
    set_item(T& list, index_type i, data_type const& v)
    { 
        *advance(list.begin(), i) = v;
    }

    static void 
    delete_item(T& list, index_type i)
    { 
        list.erase(advance(list.begin(), i));
    }
        
    static boost::python::object 
    get_slice(T& list, index_type from, index_type to)
    { 
        if (from > to)
            return boost::python::object(T());

        auto s = slice(list, from, to);
        return boost::python::object(T(s.first, s.second));
    }

    static void 
    set_slice(T& list, index_type from, index_type to, data_type const& v)
    { 
        if (to >= from)
        {
            auto s = slice(list, from, to);
            list.erase(s.first, s.second);
            list.insert(advance(list.begin(), from), v);
        }
    }

    template <typename Iter>
    static void 
    set_slice(T& list, index_type from, 
        index_type to, Iter first, Iter last)
    { 
        if (to >= from)
        {
            auto s = slice(list, from, to);
            list.erase(s.first, s.second);
            list.insert(advance(list.begin(), from), first, last);
        }
    }

    static void 
    delete_slice(T& list, index_type from, index_type to)
    { 
        if (to >= from)
        {
            auto s = slice(list, from, to);
            list.erase(s.first, s.second);
        }
    }

private:
    static 
    typename T::iterator 
    advance(typename T::iterator it, typename T::difference_type i)
    {
        return std::advance(it, i), it;
    }

    static
    std::pair<typename T::iterator, typename T::iterator>
    slice(T& list, index_type from, index_type to)
    {
        BOOST_ASSERT(to >= from);

        std::pair<typename T::iterator, typename T::iterator> s;

        s.first = list.begin();
        std::advance(s.first, from);

        s.second = s.first;
        std::advance(s.second, to - from);

        return s;
    }
};
       
} // namespace python

} // namespace bond
