// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "converters.h"
#include "enum.h"
#include "bonded.h"
#include <bond/core/bond.h>
#include <bond/stream/output_buffer.h>
#include <bond/protocol/simple_json_writer.h>


namespace bond
{
namespace python
{


// Function object to define read-write properties for a class
template <typename classT>
class def_readwrite_property
{
public:
    def_readwrite_property(classT& c)
        : c(c)
    {}

    template <typename Field>
    void operator()(const Field&) const
    {
        // Define read-write property
        add_property(Field::metadata.name, Field::field);

        // Define field type
        def_type<typename Field::value_type>();
    }

private:
    template <typename T>
    void add_property(const std::string& name, T ptr) const
    {
        c.def_readwrite(name.c_str(), ptr);
    }

    template <typename S, typename T>
    void add_property(const std::string& name, bond::nullable<T> S::* ptr) const
    {
        using namespace boost::python;

        c.add_property(name.c_str(),
             make_getter(ptr, return_value_policy<return_by_value>()),
             make_setter(ptr, return_value_policy<return_by_value>()));
    }

    template <typename S, typename T>
    void add_property(const std::string& name, bond::maybe<T> S::* ptr) const
    {
        using namespace boost::python;

        c.add_property(name.c_str(),
             make_getter(ptr, return_value_policy<return_by_value>()),
             make_setter(ptr, return_value_policy<return_by_value>()));
    }

    template <typename S>
    void add_property(const std::string& name, bond::blob S::* ptr) const
    {
        using namespace boost::python;

        c.add_property(name.c_str(),
             make_getter(ptr, return_value_policy<return_by_value>()),
             make_setter(ptr, return_value_policy<return_by_value>()));
    }

    template <typename T>
    void def_type() const
    {
        const auto* reg = boost::python::converter::registry::query(typeid(T));

        if (reg && reg->m_class_object)
            return;

        def_type(static_cast<T*>(nullptr));
    }


    template <typename T>
    typename boost::enable_if<is_list_container<T> >::type
    def_type(T*) const
    {
        static const bool NoProxy
            = has_custom_converter<typename element_type<T>::type>::value;

        typedef typename boost::mpl::if_<
                std::is_same<
                    typename std::iterator_traits<typename T::iterator>::iterator_category,
                    std::random_access_iterator_tag>,
                boost::python::vector_indexing_suite<T, NoProxy>,
                list_indexing_suite<T, NoProxy>
            >::type indexing_suite;

        // Expose container class to Python
        boost::python::class_<T>(bond::python::make_pythonic_name<T>())
            .def(indexing_suite());

        // Define rvalue conversion from Python container
        rvalue_list_container_from_python<T>();

        // Define element type
        def_type<typename element_type<T>::type>();
    }


    template <typename T>
    typename boost::enable_if<is_map_container<T> >::type
    def_type(T*) const
    {
        static const bool NoProxy
            = bond::is_string_type<typename element_type<T>::type::second_type>::value
            | has_custom_converter<typename element_type<T>::type::second_type>::value;

        // Expose container class to Python
        boost::python::class_<T>(bond::python::make_pythonic_name<T>())
            .def(boost::python::map_indexing_suite<T, NoProxy>());

        // Define rvalue conversion from Python container
        rvalue_map_container_from_python<T>();

        // Define key and value type
        def_type<typename element_type<T>::type::first_type>();
        def_type<typename element_type<T>::type::second_type>();
    }


    template <typename T>
    typename boost::enable_if<is_set_container<T> >::type
    def_type(T*) const
    {
        // Expose container class to Python
        boost::python::class_<T>(bond::python::make_pythonic_name<T>())
            .def(set_indexing_suite<T>());

        // Define rvalue conversion from Python container
        rvalue_set_container_from_python<T>();

        // Define element type
        def_type<typename element_type<T>::type>();
    }


    template <typename T>
    void def_type(bonded<T>*) const
    {
        // Expose bonded<T> type
        bonded_<T>()
            .def();
    }


    template <typename T>
    typename boost::enable_if<has_schema<T> >::type
    def_type(T*) const
    {
        // Expose a Bond struct to Python
        struct_<T, false>()
            .def();
    }


    template <typename T>
    typename boost::enable_if<std::is_enum<T> >::type
    def_type(T*) const
    {
        // Expose a Bond enum to Python
        enum_<T>()
            .def();
    }


    template <typename T>
    void def_type(bond::nullable<T>*) const
    {
        def_nullable_maybe_type<bond::nullable<T> >();
    }


    template <typename T>
    void def_type(bond::maybe<T>*) const
    {
        def_nullable_maybe_type<bond::maybe<T> >();
    }


    template <typename T>
    void def_nullable_maybe_type() const
    {
        using namespace boost::python;

        const auto* reg = converter::registry::query(typeid(T));

        if (reg && reg->rvalue_chain)
            return;

        rvalue_from_python<T, nullable_maybe_converter<T> >();
        to_python_converter<T, nullable_maybe_converter<T>, true>();

        def_type<typename T::value_type>();
    }


    void
    def_type(bond::blob*) const
    {}


    template <typename T>
    typename boost::enable_if_c<is_basic_type<T>::value
                            && !std::is_enum<T>::value>::type
    def_type(T*) const
    {}

    def_readwrite_property& operator=(const def_readwrite_property&);

    classT& c;
};


// Visitor defining all fields of a Bond schema T as read-write properties
template <typename T>
class schema_fields_visitor
    : public boost::python::def_visitor<schema_fields_visitor<T> >
{
    friend class boost::python::def_visitor_access;

    template <class classT>
    void visit(classT& c) const
    {
        boost::mpl::for_each<typename T::Schema::fields>(def_readwrite_property<classT>(c));
    }
};


// Helper class used to expose a Bond generated struct to Python
template <typename T, bool API>
class struct_
{
public:
    void def()
    {
        def_struct(T::Schema::metadata.name);
    }

    void def(const qualified_name_tag&)
    {
        def_struct(T::Schema::metadata.qualified_name);
    }

private:
    void def_struct(const pythonic_name& name)
    {
        const auto* reg = boost::python::converter::registry::query(typeid(T));

        if (!(reg && reg->m_class_object))
            def_struct(name, static_cast<T*>(nullptr));

        api<T, API>()
            .def();
    }


    template <typename U>
    typename boost::enable_if<has_base<U> >::type
    def_struct(const pythonic_name& name, U*)
    {
        using namespace boost::python;

        // Recurse into the base Bond struct
        struct_<typename U::Schema::base, false>()
            .def();

        // Expose Bond generated struct as an extension class
        class_<U, bases<typename U::Schema::base> >(name)
            .def(schema_fields_visitor<U>())
            .def(self == self);
    }


    template <typename U>
    typename boost::disable_if<has_base<U> >::type
    def_struct(const pythonic_name& name, U*)
    {
        using namespace boost::python;

        // Expose Bond generated struct as an extension class
        class_<U>(name)
            .def(schema_fields_visitor<U>())
            .def(self == self);
    }

    template <typename U, bool Enable = true>
    class api
    {
    public:
        void def()
        {
            using namespace boost::python;

            if (!defined)
            {
                defined = true;

                // Expose serialization APIs overloads for U
                boost::python::def("Serialize", &serialize, serialize_overloads());
                boost::python::def("Serialize", &serialize_bonded, serialize_bonded_overloads());
                boost::python::def("Deserialize", &deserialize, deserialize_overloads());
                boost::python::def("Deserialize", &deserialize_schema, deserialize_schema_overloads());
                boost::python::def("Marshal", &marshal, marshal_overloads());
                boost::python::def("Unmarshal", &unmarshal);
                boost::python::def("Unmarshal", &unmarshal_bonded);
                boost::python::def("Unmarshal", &unmarshal_schema);
                boost::python::def("Unmarshal", &unmarshal_bonded_schema);
                boost::python::def("GetRuntimeSchema", &schema, return_value_policy<reference_existing_object>());

                // Expose protocol enum used to sepecify protocol for de/serialization
                enum_<bond::ProtocolType>()
                    .def();

                // Expose SchemaDef
                struct_<bond::SchemaDef>()
                    .def();

                register_builtin_converters();
            }
        }

    private:
        BOOST_PYTHON_FUNCTION_OVERLOADS(marshal_overloads, marshal, 1, 2)
        BOOST_PYTHON_FUNCTION_OVERLOADS(serialize_overloads, serialize, 1, 2)
        BOOST_PYTHON_FUNCTION_OVERLOADS(serialize_bonded_overloads, serialize_bonded, 1, 2)
        BOOST_PYTHON_FUNCTION_OVERLOADS(deserialize_overloads, deserialize, 2, 3)
        BOOST_PYTHON_FUNCTION_OVERLOADS(deserialize_schema_overloads, deserialize_schema, 3, 4)

        static const bond::SchemaDef& schema(const U&)
        {
            return GetRuntimeSchema<U>().GetSchema();
        }

        static bond::blob serialize(const U& obj, uint16_t protocol = COMPACT_PROTOCOL)
        {
            OutputBuffer output;

            Apply<Serializer>(obj, output, protocol);
            return output.GetBuffer();
        }

        static bond::blob serialize_bonded(const bonded<U>& obj, uint16_t protocol = COMPACT_PROTOCOL)
        {
            OutputBuffer output;

            Apply<Serializer>(obj, output, protocol);
            return output.GetBuffer();
        }

        static void deserialize(const bond::blob& data, U& obj, uint16_t protocol = COMPACT_PROTOCOL)
        {
            InputBuffer input(data);
            // A workaround for GCC 4.8 which doesn't resolve the Apply overload below properly.
            // Apply<U>(To<U>(obj), input, protocol);
            bond::detail::NextProtocol<U, bond::BuiltInProtocols>(input, To<U>(obj), protocol);
        }

        static void deserialize_schema(const bond::blob& data, U& obj, const bond::SchemaDef& schema, uint16_t protocol = COMPACT_PROTOCOL)
        {
            InputBuffer input(data);
            Apply(To<U>(obj), bond::RuntimeSchema(schema), input, protocol);
        }

        static bond::blob marshal(const U& obj, uint16_t protocol = COMPACT_PROTOCOL)
        {
            OutputBuffer output;

            Apply<Marshaler>(obj, output, protocol);
            return output.GetBuffer();
        }

        static void unmarshal(const bond::blob& data, U& obj)
        {
            InputBuffer input(data);
            Unmarshal(input, obj);
        }

        static void unmarshal_bonded(const bond::blob& data, bonded<U>& obj)
        {
            InputBuffer input(data);
            Unmarshal(input, obj);
        }

        static void unmarshal_schema(const bond::blob& data, U& obj, const bond::SchemaDef& schema)
        {
            InputBuffer input(data);
            bonded<U> bonded_obj;

            Unmarshal(input, bonded_obj, bond::RuntimeSchema(schema));
            bonded_obj.Deserialize(obj);
        }

        static void unmarshal_bonded_schema(const bond::blob& data, bonded<U>& obj, const bond::SchemaDef& schema)
        {
            InputBuffer input(data);
            Unmarshal(input, obj, bond::RuntimeSchema(schema));
        }

        static bool defined;
    };

    template <typename U>
    class api<U, false>
    {
    public:
        void def()
        {}
    };
};

template <typename T, bool API>
template <typename U, bool Enable>
bool struct_<T, API>::api<U, Enable>::defined = false;


} // python
} // bond
