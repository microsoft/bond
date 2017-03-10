// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#if defined(_MSC_VER)
// Disable warnings in boost::python
#   pragma warning (push)
#   pragma warning (disable : 4100 4121 4127 4244 4267 4456 4459 4512)

#   if _MSC_VER < 1800
#       define HAVE_ROUND
#   endif
#endif

#include <boost/python/module.hpp>
#include <boost/python.hpp>
#include <boost/python/def.hpp>
#include <boost/python/suite/indexing/vector_indexing_suite.hpp>
#include <boost/python/suite/indexing/map_indexing_suite.hpp>

#if defined(_MSC_VER)
#   pragma warning (pop)
#endif

#include "list_indexing_suite.h"
#include "set_indexing_suite.h"

#include <bond/core/blob.h>
#include <bond/core/nullable.h>

namespace bond
{
namespace python
{


template <typename T> struct
has_custom_converter
    : std::false_type
{};

template <typename T> struct
has_custom_converter<bond::nullable<T>>
    : std::true_type
{};

template <> struct
has_custom_converter<bond::blob>
    : std::true_type
{};

// Convert Bond type name to a valid python identifier
class pythonic_name
{
public:
    template <typename T>
    pythonic_name()
        : _name(bond::detail::type<T>::name())
    {
        pythonize();
    }

    pythonic_name(const std::string& name)
        : _name(name)
    {
        pythonize();
    }

    pythonic_name(const char* name)
        : _name(name)
    {
        pythonize();
    }

    operator const char*() const
    {
        return _name.c_str();
    }

private:
    void pythonize()
    {
        // TODO: this may result in name conflict, e.g. list<string> and list_string_
        std::replace_if(_name.begin(), _name.end(), std::not1(std::ptr_fun(isalnum)), '_');
    }

    std::string _name;
};

template <typename T>
inline pythonic_name make_pythonic_name()
{
    return pythonic_name(bond::detail::type<T>::name());
}


// Register a converter from Python type to an rvalue of type T
template <class T, class Policy>
struct rvalue_from_python
{
public:
    rvalue_from_python()
    {
        boost::python::converter::registry::insert(
            &convertible,
            &construct,
            typeid(T),
            &Policy::get_pytype
        );
    }

private:
    static void* convertible(PyObject* obj)
    {
        unaryfunc* converter = Policy::convertible(obj);
        return converter && *converter ? converter : 0;
    }

    static void construct(PyObject* obj, boost::python::converter::rvalue_from_python_stage1_data* data)
    {
        // Get the (intermediate) source object
        unaryfunc converter = *static_cast<unaryfunc*>(data->convertible);
        boost::python::object intermediate(boost::python::handle<>(converter(obj)));

        // Construct the C++ object extracting value from the Python object
        void* storage = ((boost::python::converter::rvalue_from_python_storage<T>*)data)->storage.bytes;
        Policy::extract(*new (storage) T(), intermediate);
        data->convertible = storage;
    }
};


static PyObject* identity_unaryfunc(PyObject* x)
{
    Py_INCREF(x);
    return x;
}


static unaryfunc py_object_identity = identity_unaryfunc;


// Conversion policy from Python list to a list container
template <typename T>
struct list_container_from_python_list
    : boost::python::converter::wrap_pytype<&PyList_Type>
{
    static unaryfunc* convertible(PyObject* obj)
    {
        return (PyList_Check(obj)) ? &py_object_identity : 0;
    }

    static void extract(T& dst, const boost::python::object& src)
    {
        boost::python::container_utils::extend_container(dst, src);
    }
};


template <class T>
struct rvalue_list_container_from_python
    : rvalue_from_python<T, list_container_from_python_list<T> >
{};


// Insert items from a Python dictionary into a map
template <typename Map, typename Key>
bool map_insert(Map& map, const Key& key, const boost::python::object& obj)
{
    using namespace boost::python;
    typedef typename Map::value_type value_type;

    extract<const typename value_type::second_type&> valueRef(obj);

    if (valueRef.check())
    {
        map.insert(std::make_pair(key(), valueRef()));
        return true;
    }
    else
    {
        extract<typename value_type::second_type> value(obj);

        if (value.check())
        {
            map.insert(std::make_pair(key(), value()));
            return true;
        }
    }

    return false;
}


template <typename Map>
void
extend_map(Map& map, const boost::python::object& obj)
{
    using namespace boost::python;
    typedef typename Map::value_type value_type;

    BOOST_ASSERT(PyDict_Check(obj.ptr()));

    dict dict(obj);

    const auto keys =
#if PY_VERSION_HEX >= 0x03000000
        dict.keys();
#else
        dict.iterkeys();
#endif

    BOOST_FOREACH(object k,
        std::make_pair(
            stl_input_iterator<object>(keys),
            stl_input_iterator<object>()
            ))
    {
        extract<const typename value_type::first_type&> keyRef(k);

        if (keyRef.check())
        {
            if (map_insert(map, keyRef, dict.get(k)))
            {
                continue;
            }
        }
        else
        {
            extract<typename value_type::first_type> key(k);

            if (key.check())
            {
                if (map_insert(map, key, dict.get(k)))
                {
                    continue;
                }
            }
        }

        PyErr_SetString(PyExc_TypeError, "Incompatible Data Type");
        throw_error_already_set();
    }
}


// Conversion policy from Python dictionary to a map container
template <typename T>
struct map_container_from_python_dict
    : boost::python::converter::wrap_pytype<&PyDict_Type>
{
    static unaryfunc* convertible(PyObject* obj)
    {
        return (PyDict_Check(obj)) ? &py_object_identity : 0;
    }

    static void extract(T& dst, const boost::python::object& src)
    {
        extend_map(dst, src);
    }
};


template <class T>
struct rvalue_map_container_from_python
    : rvalue_from_python<T, map_container_from_python_dict<T> >
{};


template <typename T>
void
extend_set(T& set, const boost::python::object& obj)
{
    using namespace boost::python;
    typedef typename T::value_type data_type;

    BOOST_FOREACH(object elem,
        std::make_pair(
            stl_input_iterator<object>(obj),
            stl_input_iterator<object>()
            ))
    {
        extract<data_type const&> xRef(elem);

        if (xRef.check())
        {
            set.insert(xRef());
        }
        else
        {
            extract<data_type> x(elem);

            if (x.check())
            {
                set.insert(x());
            }
            else
            {
                PyErr_SetString(PyExc_TypeError, "Incompatible Data Type");
                throw_error_already_set();
            }
        }
    }
}


// Conversion policy from Python set to a set container
template <typename T>
struct set_container_from_python_set
    : boost::python::converter::wrap_pytype<&PySet_Type>
{
    static unaryfunc* convertible(PyObject* obj)
    {
        return (PySet_Check(obj)) ? &py_object_identity : 0;
    }

    static void extract(T& dst, const boost::python::object& src)
    {
        extend_set(dst, src);
    }
};


template <class T>
struct rvalue_set_container_from_python
    : rvalue_from_python<T, set_container_from_python_set<T> >
{};


// Conversion policy for bond::blob
struct blob_converter
#if PY_VERSION_HEX >= 0x03000000
    : boost::python::converter::wrap_pytype<&PyBytes_Type>
#else
    : boost::python::converter::wrap_pytype<&PyString_Type>
#endif
{
    // Conversion from Python2 string or Python3 bytes to bond::blob
    static unaryfunc* convertible(PyObject* obj)
    {
#if PY_VERSION_HEX >= 0x03000000
        return (PyBytes_Check(obj)) ? &py_object_identity : 0;
#else
        return (PyString_Check(obj)) ? &obj->ob_type->tp_str : 0;
#endif
    }

    static void extract(bond::blob& dst, const boost::python::object& src)
    {
#if PY_VERSION_HEX >= 0x03000000
        boost::shared_ptr<void> hold_convertible_ref_count(
            (void*)0,
            boost::python::converter::shared_ptr_deleter(
                boost::python::handle<>(boost::python::borrowed(src.ptr()))));
        boost::shared_ptr<char[]> bytes(
            hold_convertible_ref_count,
            PyBytes_AsString(src.ptr()));
        dst.assign(
            bytes,
            static_cast<uint32_t>(PyBytes_Size(src.ptr())));
#else
        dst.assign(
            boost::python::extract<boost::shared_ptr<char> >(src)(),
            static_cast<uint32_t>(PyString_Size(src.ptr())));
#endif
    }

    // Conversion from bond::blob to Python2 string or Python3 bytes
    static PyObject* convert(const bond::blob& blob)
    {
#if PY_VERSION_HEX >= 0x03000000
        return PyBytes_FromStringAndSize(blob.content(), blob.length());
#else
        return boost::python::incref(
            boost::python::str(blob.content(), blob.length()).ptr());
#endif
    }
};


// Conversion policy for nullable<T> and maybe<T>
template <typename T>
struct nullable_maybe_converter
    : boost::python::converter::to_python_target_type<typename T::value_type>
{
    // Conversion from Python to nullable<T>/maybe<T>
    // Python None object converts to null/nothing
    static unaryfunc* convertible(PyObject* obj)
    {
        if (obj == Py_None)
            return &py_object_identity;

        const auto* reg = boost::python::converter::registry::query
            (typeid(typename T::value_type));

        if (reg && reg->m_class_object == Py_TYPE(obj))
            return &py_object_identity;

        if (reg && reg->rvalue_chain)
            return static_cast<unaryfunc*>(reg->rvalue_chain->convertible(obj));

        return 0;
    }

    static void extract(T& dst, const boost::python::object& src)
    {
        boost::python::extract<const typename T::value_type&> valueRef(src);

        if (valueRef.check())
        {
            dst = T(static_cast<const typename T::value_type&>(valueRef));
            return;
        }
        else
        {
            boost::python::extract<typename T::value_type> value(src);

            if (value.check())
            {
                dst = T(static_cast<typename T::value_type>(value));
                return;
            }
        }

        dst = T();
    }

    // Convertion from nullable<T>/maybe<T> to Python
    // Null/nothing converts to Python None object
    static PyObject* convert(const T& obj)
    {
        if (obj == T())
            Py_RETURN_NONE;
        else
            return boost::python::incref(
                boost::python::object(obj.value()).ptr());
    }
};


inline void register_builtin_converters()
{
    static bool registered;

    if (!registered)
    {
        using namespace boost::python;

        rvalue_from_python<
            bond::blob,
            blob_converter
            >();

        to_python_converter<
            bond::blob,
            blob_converter,
            true
            >();

        // Allows extraction of share_ptr<char> from python string object
        converter::shared_ptr_from_python<char>();

        registered = true;
    }
}

} // python
} // bond
