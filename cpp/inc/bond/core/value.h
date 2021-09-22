// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bonded.h"
#include "protocol.h"
#include "schema.h"
#include "detail/typeid_value.h"

#include <boost/static_assert.hpp>

namespace bond
{

namespace detail
{

template <typename X, typename T>
typename boost::disable_if<is_type_alias<X> >::type
inline set(X& var, const T& value)
{
    BOOST_STATIC_ASSERT((is_matching_basic<T, X>::value));
    var = static_cast<X>(value);
}


template <typename X, typename T>
typename boost::enable_if<is_type_alias<X> >::type
inline set(X& var, const T& value)
{
    BOOST_STATIC_ASSERT((is_matching_basic<T, X>::value));
    set_aliased_value(var, value);
}


} // namespace detail


template <typename X, typename T>
inline X cast(const T& value)
{
    X x;
    detail::set(x, value);
    return x;
}


// Skip basic type
template <typename T, typename Reader>
typename boost::enable_if<is_basic_type<T> >::type
inline Skip(Reader& input)
{
    input.template Skip<T>();
}


// Skip Bond struct
template <typename T, typename Reader>
typename boost::enable_if<is_bond_type<T> >::type
inline Skip(Reader& input)
{
    bonded<T, Reader&>(input).Skip();
}


// Skip container for static parser
template <typename T, typename Reader>
typename boost::enable_if_c<is_container<T>::value &&
         uses_static_parser<Reader>::value>::type
inline Skip(Reader& input)
{
    SkipContainer(value<typename element_type<T>::type, Reader&>(input, false), input);
}


// Skip by type id for static parser
template <typename Reader>
typename boost::enable_if<uses_static_parser<Reader> >::type
inline Skip(Reader& input, const RuntimeSchema& schema)
{
    switch (schema.GetTypeId())
    {
        case bond::BT_SET:
        case bond::BT_LIST:
        {
            return SkipContainer(
                value<void, Reader&>(input, element_schema(schema), false), input);
        }
        case bond::BT_MAP:
        {
            return SkipMap(key_schema(schema).GetTypeId(),
                value<void, Reader&>(input, element_schema(schema), false), input);
        }
        case bond::BT_STRUCT:
        {
            return bonded<void, Reader&>(input, schema).Skip();
        }
        default:
        {
            return input.Skip(schema.GetTypeId());
        }
    }
}

template <typename Reader>
typename boost::enable_if<uses_static_parser<Reader> >::type
inline Skip(Reader&, BondDataType)
{
    BOOST_ASSERT(false);
}


// Skip list for dynamic parser
template <typename T, typename Reader>
typename boost::enable_if_c<is_container<T>::value &&
         !uses_static_parser<Reader>::value>::type
inline Skip(Reader& input)
{
    // Call protocol to skip containers; this allows protocols to implement
    // more efficient skipping than element-by-element
    input.template Skip<T>();
}


// Skip by type id for dynamic parser
template <typename Reader>
typename boost::disable_if<uses_static_parser<Reader> >::type
inline Skip(Reader& input, const RuntimeSchema& schema)
{
    input.Skip(schema.GetTypeId());
}

template <typename Reader>
typename boost::disable_if<uses_static_parser<Reader> >::type
inline Skip(Reader& input, BondDataType type)
{
    input.Skip(type);
}


template <typename T, typename Reader>
BOND_NO_INLINE void Skip(Reader& input, const std::nothrow_t&) BOND_NOEXCEPT
{
    try
    {
        Skip<T>(input);
    }
    catch(...)
    {}
}

template <typename T, typename Reader = SchemaReader, typename boost::enable_if<std::is_same<Reader, SchemaReader> >::type* = nullptr>
void Skip(SchemaReader&, const std::nothrow_t&) BOND_NOEXCEPT
{}

template <typename Reader>
BOND_NO_INLINE void Skip(Reader& input, const RuntimeSchema& schema, const std::nothrow_t&) BOND_NOEXCEPT
{
    try
    {
        Skip(input, schema);
    }
    catch(...)
    {}
}

inline void Skip(SchemaReader&, const RuntimeSchema&, const std::nothrow_t&) BOND_NOEXCEPT
{}

template <typename Reader>
BOND_NO_INLINE void Skip(Reader& input, BondDataType type, const std::nothrow_t&) BOND_NOEXCEPT
{
    try
    {
        Skip(input, type);
    }
    catch(...)
    {}
}

inline void Skip(SchemaReader&, BondDataType, const std::nothrow_t&) BOND_NOEXCEPT
{}


// value_common implements common functionality related to skipping unread values
template <typename T, typename Reader>
class value_common
    : detail::nonassignable
{
public:
    value_common(Reader input, bool skip)
        : _input(input),
          _skip(skip)
    {}

    value_common(value_common&& rhs) BOND_NOEXCEPT_IF(
        BOND_NOEXCEPT_EXPR(detail::move_data<Reader>(rhs._input)))
        : _input(detail::move_data<Reader>(rhs._input)),
          _skip(std::move(rhs._skip))
    {
        rhs._skip = false;
    }

    value_common(const value_common& that) = default;
    value_common& operator=(const value_common& that) = default;

    ~value_common()
    {
        // skip the value if it has not been read
        if (_skip)
            bond::Skip<T, typename std::remove_reference<Reader>::type>(_input, std::nothrow);
    }

    void Skip() const
    {
        _skip = false;
        bond::Skip<T, typename std::remove_reference<Reader>::type>(_input);
    }

    // skip value of non-matching type
    template <typename Protocols = BuiltInProtocols, typename X>
    void Deserialize(X& /*var*/, typename boost::disable_if<is_matching<T, X> >::type* = nullptr) const
    {
        Skip();
    }


protected:
    Reader          _input;
    mutable bool    _skip;
};


// Specialization of value for basic types
/// @brief Represents value of type T serialized using protocol Reader
template <typename T, typename Reader>
class value<T, Reader, typename boost::enable_if_c<is_basic_type<T>::value && !is_type_alias<T>::value>::type>
    : public value_common<T, Reader>
{
public:
    value(Reader input, bool skip = true)
        : value_common<T, Reader>(input, skip)
    {}

    value(value&& rhs) BOND_NOEXCEPT_IF((
        std::is_nothrow_move_constructible<value_common<T, Reader> >::value))
        : value_common<T, Reader>(std::move(rhs))
    {}

    value(const value& that) = default;
    value& operator=(const value& that) = default;

    /// @brief Deserialize the value
    template <typename Protocols = BuiltInProtocols>
    void Deserialize(T& var) const
    {
        _skip = false;
        _input.Read(var);
    }


    // deserialize series of matching values to blob
    template <typename Protocols = BuiltInProtocols>
    void Deserialize(blob& var, uint32_t size) const
    {
        BOOST_STATIC_ASSERT((std::is_same<T, blob::value_type>::value));
        _skip = false;
        _input.Read(var, size);
    }


    // deserialize the value and cast it to a variable of a matching non-string type
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::enable_if_c<is_matching_basic<T, X>::value && !is_string_type<T>::value>::type
    Deserialize(X& var) const
    {
        typename std::conditional<std::is_enum<X>::value && sizeof(X) == sizeof(T), X, T>::type data;

        _skip = false;
        _input.Read(data);
        detail::set(var, data);
    }


    // deserialize the value to a variable of a matching string type
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::enable_if_c<is_matching_basic<T, X>::value && is_string_type<T>::value>::type
    Deserialize(X& var) const
    {
        _skip = false;
        _input.Read(var);
    }


    using value_common<T, Reader>::Deserialize;

protected:
    using value_common<T, Reader>::_input;
    using value_common<T, Reader>::_skip;
};


// Specialization of value for type alias
template <typename T, typename Reader>
class value<T, Reader, typename boost::enable_if<is_type_alias<T> >::type>
    : public value_common<T, Reader>
{
public:
    value(Reader input, bool skip = true)
        : value_common<T, Reader>(input, skip)
    {}

    value(value&& rhs) BOND_NOEXCEPT_IF((
        std::is_nothrow_move_constructible<value_common<T, Reader> >::value))
        : value_common<T, Reader>(std::move(rhs))
    {}

    value(const value& that) = default;
    value& operator=(const value& that) = default;

    template <typename Protocols = BuiltInProtocols>
    void Deserialize(T& var) const
    {
        typename aliased_type<T>::type value;
        _skip = false;
        _input.Read(value);
        set_aliased_value(var, value);
    }

protected:
    using value_common<T, Reader>::_skip;
    using value_common<T, Reader>::_input;
};


// Specialization of value for Bond structs with compile-time schema
template <typename T, typename Reader>
class value<T, Reader, typename boost::enable_if<is_bond_type<T> >::type>
    : public value_common<T, Reader>
{
public:
    value(Reader input, bool skip = true)
        : value_common<T, Reader>(input, skip)
    {}

    value(value&& rhs) BOND_NOEXCEPT_IF((
        std::is_nothrow_move_constructible<value_common<T, Reader> >::value))
        : value_common<T, Reader>(std::move(rhs))
    {}

    value(const value& that) = default;
    value& operator=(const value& that) = default;

    // deserialize Bond struct into matching variable
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::enable_if<is_bond_type<X> >::type
    Deserialize(X& var) const
    {
        _skip = false;
        bonded<T, Reader>(_input).template Deserialize<Protocols>(var);
    }


    template <typename Protocols = BuiltInProtocols, typename Transform>
    void _Apply(const Transform& transform) const
    {
        _skip = false;
        Apply<Protocols>(transform, bonded<T, Reader>(_input));
    }


protected:
    using value_common<T, Reader>::_skip;
    using value_common<T, Reader>::_input;
};


// Specialization of value for 2-tuples with compile-time schema
template <typename T1, typename T2, typename Reader>
class value<std::pair<T1, T2>, Reader>
{
public:
    value(Reader /*input*/, bool skip = true)
    {
        BOOST_VERIFY(!skip);
    }
};


template <typename Protocols, typename X, typename T1, typename T2, typename Reader>
inline void DeserializeContainer(X& var, const value<std::pair<T1, T2>, Reader&>&, Reader& input);

template <typename Protocols, typename X, typename T, typename Reader>
typename boost::disable_if<is_container<X> >::type
inline DeserializeContainer(X& var, const T& element, Reader& input);

template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_nested_container<X> >::type
inline DeserializeContainer(X& var, const T& element, Reader& input);

template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_basic_container<X> >::type
inline DeserializeContainer(X& var, const T& element, Reader& input);

template <typename Protocols, typename Transform, typename T>
void DeserializeContainer(const Transform& transform, const value<T, SchemaReader&>& element, SchemaReader&)
{
    transform.Container(element, 0);
}

template <typename Protocols, typename Transform, typename T1, typename T2>
void DeserializeContainer(const Transform& transform, const value<std::pair<T1, T2>, SchemaReader&>&, SchemaReader& input)
{
    transform.Container(value<T1, SchemaReader&>{ input, false }, value<T2, SchemaReader&>{ input, false }, 0);
}

template <typename Protocols, typename Transform>
void DeserializeContainer(const Transform& transform, const value<void, SchemaReader&>& element, SchemaReader& input);


// Specialization of value for containers with compile-time schema
template <typename T, typename Reader>
class value<T, Reader, typename boost::enable_if<is_container<T> >::type>
    : public value_common<T, Reader>
{
public:
    value(Reader input, bool skip = true)
        : value_common<T, Reader>(input, skip)
    {}

    value(value&& rhs) BOND_NOEXCEPT_IF((
        std::is_nothrow_move_constructible<value_common<T, Reader> >::value))
        : value_common<T, Reader>(std::move(rhs))
    {}

    value(const value& that) = default;
    value& operator=(const value& that) = default;

    // Deserialize container
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::enable_if_c<is_matching_container<T, X>::value>::type
    Deserialize(X& var) const
    {
        _skip = false;
        DeserializeContainer<Protocols>(var, value<typename element_type<T>::type, Reader>(_input, false), _input);
    }


    template <typename Protocols = BuiltInProtocols, typename Transform>
    void _Apply(const Transform& transform) const
    {
        _skip = false;
        DeserializeContainer<Protocols>(transform, value<typename element_type<T>::type, Reader>(_input, false), _input);
    }

    using value_common<T, Reader>::Deserialize;


protected:
    using value_common<T, Reader>::_input;
    using value_common<T, Reader>::_skip;
};


template <typename Protocols, typename X, typename T, typename Reader>
typename boost::disable_if<is_container<X> >::type
inline DeserializeMap(X& var, BondDataType keyType, const T& element, Reader& input);

template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_nested_container<X> >::type
inline DeserializeMap(X& var, BondDataType keyType, const T& element, Reader& input);

template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_basic_container<X> >::type
inline DeserializeMap(X& var, BondDataType keyType, const T& element, Reader& input);

template <typename Protocols, typename Transform>
void DeserializeMap(const Transform& transform, BondDataType keyType, const value<void, SchemaReader&>& element, SchemaReader& input);


// Specialization of value for data described by runtime schema
template <typename Reader>
class value<void, Reader>
    : detail::nonassignable
{
public:
    value(Reader input, const RuntimeSchema& schema, bool skip = true)
        : _input(input),
          _schema(schema),
          _skip(skip)
    {}

    value(Reader input, BondDataType type, bool skip = true)
        : _input(input),
          _schema(_schemaDef),
          _skip(skip)
    {
        _schemaDef.root.id = type;
    }

    value(value&& rhs) BOND_NOEXCEPT_IF(
        BOND_NOEXCEPT_EXPR(detail::move_data<Reader>(rhs._input))
        && std::is_nothrow_move_constructible<SchemaDef>::value
        && std::is_nothrow_move_constructible<RuntimeSchema>::value)
        : _input(detail::move_data<Reader>(rhs._input)),
          _schemaDef(std::move(rhs._schemaDef)),
          _schema(std::move(rhs._schema)),
          _skip(std::move(rhs._skip))
    {
        rhs._skip = false;
    }

    value(const value& that) = default;
    value& operator=(const value& that) = default;

    ~value()
    {
        // skip the value if it has not been read
        if (_skip)
            bond::Skip(_input, _schema, std::nothrow);
    }

    void Skip() const
    {
        _skip = false;
        bond::Skip(_input, _schema);
    }


    // Deserialize container
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::enable_if_c<is_container<X>::value && !is_map_container<X>::value>::type
    Deserialize(X& var) const
    {
        if (_schema.GetTypeId() == get_type_id<X>::value)
        {
            _skip = false;
            DeserializeContainer<Protocols>(var,
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
        else
        {
            Skip();
        }
    }


    // Deserialize map
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::enable_if<is_map_container<X> >::type
    Deserialize(X& var) const
    {
        if (_schema.GetTypeId() == get_type_id<X>::value)
        {
            _skip = false;
            DeserializeMap<Protocols>(var, key_schema(_schema).GetTypeId(),
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
        else
        {
            Skip();
        }
    }


    // Deserialize Bond struct
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::enable_if<is_bond_type<X> >::type
    Deserialize(X& var) const
    {
        if (_schema.GetTypeId() == get_type_id<X>::value)
        {
            _skip = false;
            bonded<void, Reader>(_input, _schema).template Deserialize<Protocols>(var);
        }
        else
        {
            Skip();
        }
    }


    template <typename Protocols = BuiltInProtocols, typename Transform>
    void _Apply(const Transform& transform) const
    {
        _skip = false;

        if (_schema.GetTypeId() == bond::BT_STRUCT)
        {
            Apply<Protocols>(transform, bonded<void, Reader>(_input, _schema));
        }
        else if(_schema.GetTypeId() == bond::BT_MAP)
        {
            DeserializeMap<Protocols>(transform, key_schema(_schema).GetTypeId(),
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
        else
        {
            BOOST_ASSERT(_schema.GetTypeId() == bond::BT_LIST || _schema.GetTypeId() == bond::BT_SET);

            DeserializeContainer<Protocols>(transform,
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
    }


    // skip value of non-matching type
    template <typename Protocols = BuiltInProtocols, typename X>
    typename boost::disable_if_c<is_container<X>::value || is_bond_type<X>::value>::type
    Deserialize(X& /*var*/) const
    {
        Skip();
    }


    BondDataType GetTypeId() const
    {
        return _schema.GetTypeId();
    }


    RuntimeSchema GetRuntimeSchema() const
    {
        return _schema;
    }

private:
    Reader _input;
    SchemaDef _schemaDef;
    RuntimeSchema _schema;
    mutable bool _skip;
};


template <typename Protocols, typename Transform>
void DeserializeContainer(const Transform& transform, const value<void, SchemaReader&>& element, SchemaReader& input)
{
    switch (element.GetTypeId())
    {
    case bond::BT_SET:
    case bond::BT_MAP:
    case bond::BT_LIST:
    case bond::BT_STRUCT:
        transform.Container(element, 0);
        break;
    default:
        detail::BasicTypeContainer<Protocols>(transform, element.GetTypeId(), input, 0);
        break;
    }
}


template <typename Protocols, typename X, typename I, typename T>
typename boost::enable_if<require_modify_element<X> >::type
inline DeserializeElement(X& var, const I& item, const T& element)
{
    struct DeserializeImpl
    {
        DeserializeImpl(const T& element)
            : element(element)
        {}

        void operator()(typename element_type<X>::type& e)
        {
            this->element.template Deserialize<Protocols>(e);
        }

        const T& element;
    };

    modify_element(var, item, DeserializeImpl(element));
}


template <typename Protocols, typename X, typename I, typename T>
typename boost::disable_if<require_modify_element<X> >::type
inline DeserializeElement(X&, I& item, const T& element)
{
    element.template Deserialize<Protocols>(item);
}


// Read elements of a list
template <typename Protocols, typename X, typename T>
typename boost::enable_if_c<is_list_container<X>::value
                         && is_element_matching<T, X>::value>::type
inline DeserializeElements(X& var, const T& element, uint32_t size)
{
    resize_list(var, size);

    for (enumerator<X> items(var); items.more();)
        DeserializeElement<Protocols>(var, items.next(), element);
}


template <typename Protocols, typename X, typename T>
typename boost::enable_if<is_matching<T, X> >::type
inline DeserializeElements(nullable<X>& var, const T& element, uint32_t size)
{
    resize_list(var, size);

    for (enumerator<nullable<X> > items(var); items.more(); --size)
        element.template Deserialize<Protocols>(items.next());

    // Wire representation and interface for nullable is the same as for list.
    // However nullable can "contain" at most one element. If there are more
    // elements in the payload we skip them.
    detail::SkipElements(element, size);
}


template <typename Protocols, typename Reader>
inline void DeserializeElements(blob& var, const value<blob::value_type, Reader&>& element, uint32_t size)
{
    element.template Deserialize<Protocols>(var, size);
}


template <typename Protocols, typename X, typename T>
typename boost::enable_if_c<is_set_container<X>::value
                         && is_element_matching<T, X>::value>::type
inline DeserializeElements(X& var, const T& element, uint32_t size)
{
    clear_set(var);

    typename element_type<X>::type e(make_element(var));

    while (size--)
    {
        element.template Deserialize<Protocols>(e);
        set_insert(var, e);
    }
}


template <typename Protocols, typename X, typename T>
typename boost::disable_if<is_element_matching<T, X> >::type
inline DeserializeElements(X&, const T& element, uint32_t size)
{
    detail::SkipElements(element, size);
}


template <typename Protocols, typename Transform, typename T>
inline void DeserializeElements(const Transform& transform, const T& element, uint32_t size)
{
    transform.Container(element, size);
}


template <typename T1, typename T2, typename Reader>
inline void SkipContainer(const value<std::pair<T1, T2>, Reader&>&, Reader& input)
{
    BOOST_STATIC_ASSERT(uses_static_parser<Reader>::value);

    SkipMap(get_type_id<T1>::value, value<T2, Reader&>(input, false), input);
}


template <typename T, typename Reader>
inline void SkipContainer(const T& element, Reader& input)
{
    BOOST_STATIC_ASSERT(uses_static_parser<Reader>::value);

    uint32_t size;

    {
        BondDataType type;
        input.ReadContainerBegin(size, type);
    }

    detail::SkipElements(element, size);

    input.ReadContainerEnd();
}


template <typename Protocols, typename X, typename T1, typename T2, typename Reader>
inline void DeserializeContainer(X& var, const value<std::pair<T1, T2>, Reader&>&, Reader& input)
{
    return DeserializeMap<Protocols>(var, get_type_id<T1>::value, value<T2, Reader&>(input, false), input);
}


template <typename Protocols, typename X, typename T, typename Reader>
typename boost::disable_if<is_container<X> >::type
inline DeserializeContainer(X& var, const T& element, Reader& input)
{
    BondDataType type = GetTypeId(element);
    uint32_t     size = 0;

    input.ReadContainerBegin(size, type);

    switch (type)
    {
        case bond::BT_SET:
        case bond::BT_MAP:
        case bond::BT_LIST:
        case bond::BT_STRUCT:
        {
            if (type == GetTypeId(element))
            {
                DeserializeElements<Protocols>(var, element, size);
            }
            else
            {
                DeserializeElements<Protocols>(var, value<void, Reader&>(input, type, false), size);
            }
            break;
        }
        default:
        {
            detail::BasicTypeContainer<Protocols>(var, type, input, size);
            break;
        }
    }

    input.ReadContainerEnd();
}


template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_nested_container<X> >::type
inline DeserializeContainer(X& var, const T& element, Reader& input)
{
    BondDataType type = GetTypeId(element);
    uint32_t     size;

    input.ReadContainerBegin(size, type);

    if (type == GetTypeId(element))
    {
        DeserializeElements<Protocols>(var, element, size);
    }
    else
    {
        detail::SkipElements(type, input, size);
    }

    input.ReadContainerEnd();
}


template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_basic_container<X> >::type
inline DeserializeContainer(X& var, const T& element, Reader& input)
{
    BondDataType type = GetTypeId(element);
    uint32_t     size;

    input.ReadContainerBegin(size, type);

    switch (type)
    {
        case bond::BT_SET:
        case bond::BT_MAP:
        case bond::BT_LIST:
        case bond::BT_STRUCT:
        {
            if (type == GetTypeId(element))
            {
                detail::SkipElements(element, size);
            }
            else
            {
                while (size--)
                    Skip(input, type);
            }
            break;
        }
        default:
        {
            detail::MatchingTypeContainer<Protocols>(var, type, input, size);
            break;
        }
    }

    input.ReadContainerEnd();
}


template <typename Protocols, typename Transform>
void DeserializeMap(const Transform& transform, BondDataType keyType, const value<void, SchemaReader&>& element, SchemaReader& input)
{
    switch (element.GetTypeId())
    {
    case bond::BT_SET:
    case bond::BT_MAP:
    case bond::BT_LIST:
    case bond::BT_STRUCT:
        detail::MapByKey<Protocols>(transform, keyType, element, input, 0);
        break;
    default:
        detail::MapByElement<Protocols>(transform, keyType, element.GetTypeId(), input, 0);
        break;
    }
}


template <typename Protocols, typename X, typename Key, typename T>
typename boost::enable_if<is_map_key_matching<Key, X> >::type
inline DeserializeMapElements(X& var, const Key& key, const T& element, uint32_t size)
{
    BOOST_STATIC_ASSERT((is_map_element_matching<T, X>::value));

    clear_map(var);

    typename element_type<X>::type::first_type k(make_key(var));

    while (size--)
    {
        key.template Deserialize<Protocols>(k);

#ifndef NDEBUG
        // In debug build To<T> asserts that optional fields are set to default
        // values before deserialization; if invalid map payload contains duplicate
        // keys the second time we deserialize a value it will trigger the assert.
        element.template Deserialize<Protocols>(mapped_at(var, k) = make_value(var));
#else
        element.template Deserialize<Protocols>(mapped_at(var, k));
#endif
    }
}


template <typename Protocols, typename X, typename Key, typename T>
typename boost::disable_if<is_map_key_matching<Key, X> >::type
inline DeserializeMapElements(X&, const Key& key, const T& element, uint32_t size)
{
    while (size--)
    {
        key.Skip();
        element.Skip();
    }
}


template <typename Protocols, typename Transform, typename Key, typename T>
inline void DeserializeMapElements(const Transform& transform, const Key& key, const T& element, uint32_t size)
{
    transform.Container(key, element, size);
}


template <typename T, typename Reader>
inline void SkipMap(BondDataType keyType, const T& element, Reader& input)
{
    BOOST_STATIC_ASSERT(uses_static_parser<Reader>::value);

    uint32_t size;

    {
        std::pair<BondDataType, BondDataType> type;

        input.ReadContainerBegin(size, type);
    }

    detail::SkipElements(keyType, element, input, size);

    input.ReadContainerEnd();
}


template <typename Protocols, typename X, typename T, typename Reader>
typename boost::disable_if<is_container<X> >::type
inline DeserializeMap(X& var, BondDataType keyType, const T& element, Reader& input)
{
    std::pair<BondDataType, BondDataType> type(keyType, GetTypeId(element));
    uint32_t                              size = 0;

    input.ReadContainerBegin(size, type);

    switch (type.second)
    {
        case bond::BT_SET:
        case bond::BT_MAP:
        case bond::BT_LIST:
        case bond::BT_STRUCT:
        {
            if (type.second == GetTypeId(element))
            {
                detail::MapByKey<Protocols>(var, type.first, element, input, size);
            }
            else
            {
                detail::MapByKey<Protocols>(var, type.first, value<void, Reader&>(input, type.second, false), input, size);
            }
            break;
        }
        default:
        {
            detail::MapByElement<Protocols>(var, type.first, type.second, input, size);
            break;
        }
    }

    input.ReadContainerEnd();
}


template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_nested_container<X> >::type
inline DeserializeMap(X& var, BondDataType keyType, const T& element, Reader& input)
{
    std::pair<BondDataType, BondDataType> type(keyType, GetTypeId(element));
    uint32_t                              size;

    input.ReadContainerBegin(size, type);

    if (type.second == GetTypeId(element))
    {
        detail::MapByKey<Protocols>(var, type.first, element, input, size);
    }
    else
    {
        detail::SkipElements(type.first, type.second, input, size);
    }

    input.ReadContainerEnd();
}


template <typename Protocols, typename X, typename T, typename Reader>
typename boost::enable_if<is_basic_container<X> >::type
inline DeserializeMap(X& var, BondDataType keyType, const T& element, Reader& input)
{
    std::pair<BondDataType, BondDataType> type(keyType, GetTypeId(element));
    uint32_t                              size;

    input.ReadContainerBegin(size, type);

    switch (type.second)
    {
        case bond::BT_SET:
        case bond::BT_MAP:
        case bond::BT_LIST:
        case bond::BT_STRUCT:
        {
            if (type.second == GetTypeId(element))
            {
                detail::SkipElements(type.first, element, input, size);
            }
            else
            {
                while (size--)
                {
                    input.Skip(type.first);
                    Skip(input, type.second);
                }
            }
            break;
        }
        default:
        {
            detail::MatchingMapByElement<Protocols>(var, type.first, type.second, input, size);
            break;
        }
    }

    input.ReadContainerEnd();
}


} // namespace bond


#ifdef BOND_LIB_TYPE
#if BOND_LIB_TYPE != BOND_LIB_TYPE_HEADER
#include "detail/value_extern.h"
#endif
#else
#error BOND_LIB_TYPE is undefined
#endif
