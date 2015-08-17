// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/static_assert.hpp>

#include "config.h"
#include "schema.h"

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

}

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
BOND_NO_INLINE void Skip(Reader& input, const std::nothrow_t&)
{
    try
    {
        Skip<T>(input);
    }
    catch(...)
    {}
}

template <typename Reader>
BOND_NO_INLINE void Skip(Reader& input, const RuntimeSchema& schema, const std::nothrow_t&)
{
    try
    {
        Skip(input, schema);
    }
    catch(...)
    {}
}

template <typename Reader>
BOND_NO_INLINE void Skip(Reader& input, BondDataType type, const std::nothrow_t&)
{
    try
    {
        Skip(input, type);
    }
    catch(...)
    {}
}

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

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    value_common(value_common&& rhs)
        : _input(rhs._input),
          _skip(std::move(rhs._skip))
    {
        rhs._skip = false;
    }
#endif

#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
    value_common(const value_common& that) = default;
    value_common& operator=(const value_common& that) = default;
#endif

    ~value_common()
    {
        // skip the value if it has not been read
        if (_skip)
            bond::Skip<T, typename remove_reference<Reader>::type>(_input, std::nothrow);
    }

    void Skip() const
    {
        _skip = false;
        bond::Skip<T, typename remove_reference<Reader>::type>(_input);
    }

    // skip value of non-matching type
    template <typename X>
    typename boost::disable_if_c<is_matching<T, X>::value>::type
    Deserialize(X& /*var*/) const
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

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    value(value&& rhs)
        : value_common<T, Reader>(std::move(rhs))
    {}
#endif

#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
    value(const value& that) = default;
    value& operator=(const value& that) = default;
#endif

    /// @brief Deserialize the value
    void Deserialize(T& var) const
    {
        _skip = false;
        _input.Read(var);
    }


    // deserialize series of matching values to blob
    void Deserialize(blob& var, uint32_t size) const
    {
        BOOST_STATIC_ASSERT((is_same<T, blob::value_type>::value));
        _skip = false;
        _input.Read(var, size);
    }
    
    
    // deserialize the value and cast it to a variable of a matching non-string type
    template <typename X>
    typename boost::enable_if_c<is_matching_basic<T, X>::value && !is_string_type<T>::value>::type
    Deserialize(X& var) const
    {
        typename boost::mpl::if_c<is_enum<X>::value && sizeof(X) == sizeof(T), X, T>::type data;
        
        _skip = false;
        _input.Read(data);
        detail::set(var, data);
    }

    
    // deserialize the value to a variable of a matching string type
    template <typename X>
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

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    value(value&& rhs)
        : value_common<T, Reader>(std::move(rhs))
    {}
#endif

#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
    value(const value& that) = default;
    value& operator=(const value& that) = default;
#endif

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

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    value(value&& rhs)
        : value_common<T, Reader>(std::move(rhs))
    {}
#endif

#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
    value(const value& that) = default;
    value& operator=(const value& that) = default;
#endif

    // deserialize Bond struct into matching variable
    template <typename X>
    typename boost::enable_if<is_bond_type<X> >::type
    Deserialize(X& var) const
    {
        _skip = false;
        bonded<T, Reader>(_input).Deserialize(var);
    }


    template <typename Transform>
    void _Apply(const Transform& transform) const
    {
        _skip = false;
        Apply(transform, bonded<T, Reader>(_input));
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


// Specialization of value for containers with compile-time schema
template <typename T, typename Reader>
class value<T, Reader, typename boost::enable_if<is_container<T> >::type>
    : public value_common<T, Reader>
{
public:
    value(Reader input, bool skip = true)
        : value_common<T, Reader>(input, skip)
    {}

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    value(value&& rhs)
        : value_common<T, Reader>(std::move(rhs))
    {}
#endif

#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
    value(const value& that) = default;
    value& operator=(const value& that) = default;
#endif
    
    // Deserialize container
    template <typename X>
    typename boost::enable_if_c<is_matching_container<T, X>::value>::type
    Deserialize(X& var) const
    {
        _skip = false;
        DeserializeContainer(var, value<typename element_type<T>::type, Reader>(_input, false), _input);
    }


    template <typename Transform>
    void _Apply(const Transform& transform) const
    {
        _skip = false;
        DeserializeContainer(transform, value<typename element_type<T>::type, Reader>(_input, false), _input);
    }

    using value_common<T, Reader>::Deserialize;


protected:
    using value_common<T, Reader>::_input;
    using value_common<T, Reader>::_skip;
};



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

#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    value(value&& rhs)
        : _input(rhs._input),
          _schemaDef(std::move(rhs._schemaDef)),
          _schema(std::move(rhs._schema)),
          _skip(std::move(rhs._skip))
    {
        rhs._skip = false;
    }
#endif

#ifndef BOND_NO_CXX11_DEFAULTED_FUNCTIONS
    value(const value& that) = default;
    value& operator=(const value& that) = default;
#endif

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
    template <typename X>
    typename boost::enable_if_c<is_container<X>::value && !is_map_container<X>::value>::type
    Deserialize(X& var) const
    {
        if (_schema.GetTypeId() == get_type_id<X>::value)
        {
            _skip = false;
            DeserializeContainer(var, 
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
        else
        {
            Skip();
        }
    }

    
    // Deserialize map
    template <typename X>
    typename boost::enable_if<is_map_container<X> >::type
    Deserialize(X& var) const
    {
        if (_schema.GetTypeId() == get_type_id<X>::value)
        {
            _skip = false;
            DeserializeMap(var, key_schema(_schema).GetTypeId(),
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
        else
        {
            Skip();
        }
    }


    // Deserialize Bond struct
    template <typename X>
    typename boost::enable_if<is_bond_type<X> >::type
    Deserialize(X& var) const
    {
        if (_schema.GetTypeId() == get_type_id<X>::value)
        {
            _skip = false;
            bonded<void, Reader>(_input, _schema).Deserialize(var);
        }
        else
        {
            Skip();
        }
    }


    template <typename Transform>
    void _Apply(const Transform& transform) const
    {
        _skip = false;

        if (_schema.GetTypeId() == bond::BT_STRUCT)
        {
            Apply(transform, bonded<void, Reader>(_input, _schema));
        }
        else if(_schema.GetTypeId() == bond::BT_MAP)
        {
            DeserializeMap(transform, key_schema(_schema).GetTypeId(),
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
        else
        {
            BOOST_ASSERT(_schema.GetTypeId() == bond::BT_LIST || _schema.GetTypeId() == bond::BT_SET);

            DeserializeContainer(transform, 
                value<void, Reader>(_input, element_schema(_schema), false), _input);
        }
    }

    
    // skip value of non-matching type
    template <typename X>
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


template <typename X, typename I, typename T>
typename boost::enable_if<require_modify_element<X> >::type 
inline DeserializeElement(X& var, const I& item, const T& element)
{
    struct Deserialize
    {
        Deserialize(const T& element)
            : element(element)
        {}

        void operator()(typename element_type<X>::type& e)
        {
            this->element.Deserialize(e);
        }

        const T& element;
    };

    modify_element(var, item, Deserialize(element));
}


template <typename X, typename I, typename T>
typename boost::disable_if<require_modify_element<X> >::type 
inline DeserializeElement(X&, I& item, const T& element)
{
    element.Deserialize(item);
}


// Read elements of a list
template <typename X, typename T>
typename boost::enable_if_c<is_list_container<X>::value
                         && is_element_matching<T, X>::value>::type
inline DeserializeElements(X& var, const T& element, uint32_t size)
{    
    resize_list(var, size);

    for (enumerator<X> items(var); items.more();)
        DeserializeElement(var, items.next(), element);
}


template <typename X, typename Allocator, bool useValue, typename T>
typename boost::enable_if<is_matching<T, X> >::type
inline DeserializeElements(nullable<X, Allocator, useValue>& var, const T& element, uint32_t size)
{    
    resize_list(var, size);

    for (enumerator<nullable<X, Allocator, useValue> > items(var); items.more(); --size)
        element.Deserialize(items.next());

    // Wire representation and interface for nullable is the same as for list.
    // However nullable can "contain" at most one element. If there are more
    // elements in the payload we skip them. 
    while (size--)
        element.Skip();        
}


template <typename Reader>
inline void DeserializeElements(blob& var, const value<blob::value_type, Reader&>& element, uint32_t size)
{    
    element.Deserialize(var, size);
}


template <typename X, typename T>
typename boost::enable_if_c<is_set_container<X>::value
                         && is_element_matching<T, X>::value>::type
inline DeserializeElements(X& var, const T& element, uint32_t size)
{    
    clear_set(var);

    typename element_type<X>::type e(make_element(var));

    while (size--)
    {
        element.Deserialize(e);
        set_insert(var, e);
    }
}


template <typename X, typename T>
typename boost::disable_if<is_element_matching<T, X> >::type
inline DeserializeElements(X&, const T& element, uint32_t size)
{    
    while (size--)
        element.Skip();
}


template <typename Transform, typename T>
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

    while (size--)
        element.Skip();

    input.ReadContainerEnd();
}


template <typename X, typename T1, typename T2, typename Reader>
inline void DeserializeContainer(X& var, const value<std::pair<T1, T2>, Reader&>&, Reader& input)
{
    return DeserializeMap(var, get_type_id<T1>::value, value<T2, Reader&>(input, false), input);
}


template <typename X, typename T, typename Reader>
typename boost::disable_if<is_container<X> >::type
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
                DeserializeElements(var, element, size);
            }
            else
            {
                DeserializeElements(var, value<void, Reader&>(input, type, false), size);
            }
            break;
        }
        default:
        {
            detail::BasicTypeContainer(var, type, input, size);
            break;
        }
    }
               
    input.ReadContainerEnd();
}


template <typename X, typename T, typename Reader>
typename boost::enable_if<is_nested_container<X> >::type
inline DeserializeContainer(X& var, const T& element, Reader& input)
{
    BondDataType type = GetTypeId(element);
    uint32_t     size;

    input.ReadContainerBegin(size, type);

    if (type == GetTypeId(element))
    {
        DeserializeElements(var, element, size);
    }
    else
    {
        while (size--)
            input.Skip(type);
    }
               
    input.ReadContainerEnd();
}


template <typename X, typename T, typename Reader>
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
                while (size--)
                    element.Skip();
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
            detail::MatchingTypeContainer(var, type, input, size);
            break;
        }
    }
               
    input.ReadContainerEnd();
}


template <typename X, typename Key, typename T>
typename boost::enable_if<is_map_key_matching<Key, X> >::type
inline DeserializeMapElements(X& var, const Key& key, const T& element, uint32_t size)
{    
    BOOST_STATIC_ASSERT((is_map_element_matching<T, X>::value));

    clear_map(var);

    typename element_type<X>::type::first_type k(make_key(var));

    while (size--)
    {
        key.Deserialize(k);

#ifndef NDEBUG
        // In debug build To<T> asserts that optional fields are set to default 
        // values before deserialization; if invalid map payload contains duplicate 
        // keys the second time we deserialize a value it will trigger the assert. 
        element.Deserialize(mapped_at(var, k) = make_value(var));
#else
        element.Deserialize(mapped_at(var, k));
#endif
    }
}


template <typename X, typename Key, typename T>
typename boost::disable_if<is_map_key_matching<Key, X> >::type  
inline DeserializeMapElements(X&, const Key& key, const T& element, uint32_t size)
{    
    while (size--)
    {
        key.Skip();
        element.Skip();
    }
}


template <typename Transform, typename Key, typename T>
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

    while (size--)
    {
        input.Skip(keyType);
        element.Skip();
    }

    input.ReadContainerEnd();
}


template <typename X, typename T, typename Reader>
typename boost::disable_if<is_container<X> >::type
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
                detail::MapByKey(var, type.first, element, input, size);
            }
            else
            {
                detail::MapByKey(var, type.first, value<void, Reader&>(input, type.second, false), input, size);
            }
            break;
        }
        default:
        {
            detail::MapByElement(var, type.first, type.second, input, size);
            break;
        }
    }
               
    input.ReadContainerEnd();
}


template <typename X, typename T, typename Reader>
typename boost::enable_if<is_nested_container<X> >::type
inline DeserializeMap(X& var, BondDataType keyType, const T& element, Reader& input)
{
    std::pair<BondDataType, BondDataType> type(keyType, GetTypeId(element));
    uint32_t                              size;

    input.ReadContainerBegin(size, type); 

    if (type.second == GetTypeId(element))
    {
        detail::MapByKey(var, type.first, element, input, size);
    }
    else
    {
        while (size--)
        {
            input.Skip(type.first);
            input.Skip(type.second);
        }
    }
               
    input.ReadContainerEnd();
}


template <typename X, typename T, typename Reader>
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
                while (size--)
                {
                    input.Skip(type.first);
                    element.Skip();
                }
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
            detail::MatchingMapByElement(var, type.first, type.second, input, size);
            break;
        }
    }
               
    input.ReadContainerEnd();
}

} // namespace bond
    
