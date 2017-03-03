// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <boost/static_assert.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/find_if.hpp>
#include <boost/mpl/for_each.hpp>
#include <boost/mpl/list.hpp>
#include <boost/mpl/push_front.hpp>
#include <boost/mpl/copy_if.hpp>
#include <boost/assign.hpp>
#include <boost/assign/list_of.hpp>

#include <bond/core/bond_types.h>
#include "bonded.h"
#include "detail/metadata.h"
#include <functional>

namespace bond
{

using boost::mpl::_;

template <typename T> struct
remove_maybe
{
    typedef T type;
};


template <typename T> struct
remove_maybe<maybe<T> >
{
    typedef typename remove_maybe<T>::type type;
};


static const uint16_t invalid_field_id = 0xffff;


/** namespace bond::reflection */
namespace reflection
{
//
// Helper classes/templates
//
typedef std::map<std::string, std::string> Attributes;


// field is required
struct required_field_modifier
{
    static const bond::Modifier value = bond::Required;
};

// field is optional
struct optional_field_modifier
{
    static const bond::Modifier value = bond::Optional;
};

// field is required optional
struct required_optional_field_modifier
{
    static const bond::Modifier value = bond::RequiredOptional;
};


/// @brief Field description in compile-time schema
template <
    uint16_t field_id,
    typename ModifierTag,
    typename Struct,
    typename FieldType,
    FieldType Struct::*field_ptr,
    const bond::Metadata* metadata_ptr>
struct FieldTemplate
{
    /// @brief Type of the field's parent struct
    typedef Struct struct_type;

    /// @brief Type of the field pointer
    typedef FieldType Struct::*field_pointer;

    /// @brief Type of the field
    typedef typename remove_maybe<FieldType>::type field_type;

    /// @brief Type of the field value
    typedef FieldType value_type;

    /// @brief Modifier tag for the field
    ///
    /// Can be one of:
    /// - bond::reflection::optional_field_modifier
    /// - bond::reflection::required_field_modifier
    /// - bond::reflection::required_optional_field_modifier
    typedef ModifierTag field_modifier;

    /// @brief Static data member describing field metadata
    static const Metadata& metadata;

    /// @brief Static data member representing the field pointer
    static const field_pointer field;

    /// @brief Static data member equal to the field ordinal
    static const uint16_t id = field_id;

    /// @brief Static method returning const reference to the field value for a particular object
    static
    const value_type& GetVariable(const struct_type& object)
    {
        return object.*field;
    }

    /// @brief Static method returning reference to the field value for a particular object
    static
    value_type& GetVariable(struct_type& object)
    {
        return object.*field;
    }

    BOOST_STATIC_ASSERT(field_id != invalid_field_id);
};


template <
    uint16_t field_id,
    typename ModifierTag,
    typename Struct,
    typename FieldType,
    FieldType Struct::*field_ptr,
    const Metadata* metadata_ptr>
const bond::Metadata&
    FieldTemplate<field_id, ModifierTag, Struct, FieldType, field_ptr, metadata_ptr>::metadata = *metadata_ptr;

template
<
    uint16_t field_id,
    typename ModifierTag,
    typename Struct,
    typename FieldType,
    FieldType Struct::*field_ptr,
    const Metadata* metadata_ptr
>
const typename FieldTemplate<field_id, ModifierTag, Struct, FieldType, field_ptr, metadata_ptr>::field_pointer
    FieldTemplate<field_id, ModifierTag, Struct, FieldType, field_ptr, metadata_ptr>::field = field_ptr;


template <typename Service, typename Input, typename Result> struct
method_pointer
{
    typedef void (Service::*type)(
        const Input&,
        const std::function<void(const Result&)>&);
};


template <typename Service, typename Input> struct
method_pointer<Service, Input, void>
{
    typedef void (Service::*type)(const Input&);
};


/// @brief Method description in compile-time schema
template <
    typename Service,
    typename Input,
    typename Result,
    typename method_pointer<Service, Input, Result>::type method_ptr,
    const Metadata* metadata_ptr>
struct MethodTemplate
{
    /// @brief Type of the service
    typedef Service service_type;

    /// @brief Type of the request
    typedef Input input_type;

    /// @brief Type of the response
    typedef Result result_type;

    /// @brief Static data member describing method metadata
    static const Metadata& metadata;

    /// @brief Static data member representing the member pointer to the method
    static const typename method_pointer<service_type, input_type, result_type>::type method;
};


template <
    typename Service,
    typename Input,
    typename Result,
    typename method_pointer<Service, Input, Result>::type method_ptr,
    const bond::Metadata* metadata_ptr>
const bond::Metadata&
    MethodTemplate<Service, Input, Result, method_ptr, metadata_ptr>::metadata = *metadata_ptr;

template <
    typename Service,
    typename Input,
    typename Result,
    typename method_pointer<Service, Input, Result>::type method_ptr,
    const bond::Metadata* metadata_ptr>
const typename method_pointer<Service, Input, Result>::type
    MethodTemplate<Service, Input, Result, method_ptr, metadata_ptr>::method = method_ptr;


// Metadata initializer for fields
inline
bond::Metadata MetadataInit(const char* name)
{
    bond::Metadata metadata;

    metadata.name = name;
    return metadata;
}

inline
bond::Metadata MetadataInit(const char* name, bond::Modifier modifier, const Attributes& attributes)
{
    bond::Metadata metadata;

    metadata.name = name;
    metadata.modifier = modifier;
    metadata.attributes = attributes;

    return metadata;
}

inline
bond::Metadata MetadataInit(const char* name, const Attributes& attributes)
{
    bond::Metadata metadata;

    metadata.name = name;
    metadata.attributes = attributes;

    return metadata;
}

template <typename T>
bond::Metadata MetadataInit(const T& default_value, const char* name)
{
    bond::Metadata metadata;

    metadata.name = name;
    detail::VariantSet(metadata.default_value, default_value);
    return metadata;
}

template <typename T>
bond::Metadata MetadataInit(const T& default_value, const char* name, bond::Modifier modifier, const Attributes& attributes)
{
    bond::Metadata metadata = MetadataInit(name, modifier, attributes);

    detail::VariantSet(metadata.default_value, default_value);

    return metadata;
}

struct nothing
{};

inline
bond::Metadata MetadataInit(const nothing&, const char* name)
{
    bond::Metadata metadata;

    metadata.name = name;
    metadata.default_value.nothing = true;
    return metadata;
}

inline
bond::Metadata MetadataInit(const nothing&, const char* name, bond::Modifier modifier, const Attributes& attributes)
{
    bond::Metadata metadata = MetadataInit(name, modifier, attributes);

    metadata.default_value.nothing = true;

    return metadata;
}


// Metadata initializer for structs
inline
bond::Metadata MetadataInit(const char* name, const char* qual_name, const Attributes& attributes)
{
    bond::Metadata metadata;

    metadata.name = name;
    metadata.qualified_name = qual_name;
    metadata.attributes = attributes;

    return metadata;
}


// Metadata initializer for generic structs
template <typename Params>
bond::Metadata MetadataInit(const char* name, const char* qual_name, const Attributes& attributes)
{
    bond::Metadata metadata = MetadataInit(name, qual_name, attributes);

    std::string params;

    // boost::mpl::for_each instantiates object of each type in the sequence.
    // We transform the Params to a sequence of type pointers to avoid creating
    // actual complex types that might not even support default ctor.
    typedef typename boost::mpl::transform<Params, boost::add_pointer<_> >::type ParamsPtr;

    boost::mpl::for_each<ParamsPtr>(detail::TypeListBuilder(params));

    metadata.name += "<" + params + ">";
    metadata.qualified_name += "<" + params + ">";

    return metadata;
}

} // namespace reflection


const reflection::nothing nothing = {};

template <typename T, typename Iter> struct
field_id
{
    static const uint16_t value = boost::mpl::deref<Iter>::type::id;
};

template <typename T> struct
field_id<T, typename boost::mpl::end<T>::type>
{
    static const uint16_t value = invalid_field_id;
};

template <typename T, uint16_t minId = 0> struct
next_required_field
{
private:
    template <typename Field> struct
    is_next_required
    {
        static const bool value = Field::id >= minId
                               && is_same<typename Field::field_modifier, typename reflection::required_field_modifier>::value;
    };

public:
    static const uint16_t value = field_id<T, typename boost::mpl::find_if<T, is_next_required<_> >::type>::value;
};


struct no_base {};


template <typename T, typename Enable = void> struct
is_writer
    : false_type {};


template <typename T> struct
is_writer<T, typename boost::enable_if<check_method<void (T::*)(const Metadata&, bool), &T::WriteStructBegin> >::type>
     : true_type {};


template <typename T>
inline typename T::base*
base_class()
{
    return NULL;
}


template <typename T> struct
remove_bonded
{
    typedef T type;
};


template <typename T> struct
remove_bonded<bonded<T> >
{
    typedef typename remove_bonded<T>::type type;
};


template <typename T> struct
is_bond_type
{
    typedef typename remove_const<T>::type U;

    static const bool value = is_bonded<U>::value
                           || has_schema<U>::value;
};


struct Unknown;

template <typename Unused> struct
schema<Unknown, Unused>
{
    struct type
    {
        typedef no_base base;
        typedef boost::mpl::list<>::type fields;
        static const Metadata metadata;

        type()
        {
            (void)metadata;
        }
    };
};

template <typename Unused>
const Metadata schema<Unknown, Unused>::type::metadata
    = reflection::MetadataInit("Unknown", "Unknown", reflection::Attributes());

template <typename T, typename Enable> struct
schema_for_passthrough
    : schema<typename remove_bonded<T>::type>
{};

template <typename T> struct
schema_for_passthrough<T, typename boost::disable_if<has_schema<typename remove_bonded<T>::type> >::type>
{
    // If type T doesn't have schema we return schema of an empty struct;
    // this allows pass-through of bonded<T> with only forward declaration for T.
    typedef typename schema<Unknown>::type type;
};

template <typename T> struct
is_container
{
    typedef typename remove_const<T>::type U;

    static const bool value = is_list_container<U>::value
                           || is_set_container<U>::value
                           || is_map_container<U>::value;
};


template <typename Field, typename Transform, typename Enable = void> struct
is_fast_path_field
    : false_type {};


template <typename Field, typename Transform> struct
is_fast_path_field<Field, Transform, typename boost::enable_if<is_same<typename Field::struct_type,
                                                                       typename Transform::FastPathType> >::type>
     : true_type {};


template <typename T> struct
is_nested_field
    : is_bond_type<typename T::field_type> {};


template <typename T> struct
is_struct_field
    : has_schema<typename T::field_type> {};


template <typename T1, typename T2, typename Enable = void> struct
is_matching_container
    : false_type {};


template <typename T1, typename T2> struct
is_matching_basic
{
    static const bool value =
           (is_string<T1>::value && is_string<T2>::value)
        || (is_wstring<T1>::value && is_wstring<T2>::value)
        || ((sizeof(T1) <= sizeof(T2))
         && ((is_unsigned<T1>::value && is_unsigned<T2>::value)
          || (is_signed_int_or_enum<T1>::value && is_signed_int_or_enum<T2>::value)));
};


template <typename T> struct
is_matching_basic<typename aliased_type<T>::type, T>
    : boost::true_type {};


template <typename T1, typename T2> struct
is_matching
{
    static const bool value =
           (is_bond_type<T1>::value && is_bond_type<T2>::value)
        || (is_matching_basic<T1, T2>::value)
        || (is_matching_container<T1, T2>::value);
};


template <typename T> struct
is_matching_basic<T, T>
    : true_type {};


template <> struct
is_matching_basic<bool, bool>
    : true_type {};


template <typename T> struct
is_matching_basic<bool, T>
    : false_type {};


template <> struct
is_matching_basic<uint8_t, bool>
    : false_type {};


template <> struct
is_matching_basic<float, double>
    : true_type {};


template <typename T> struct
get_type_id;


template <typename T1, typename T2> struct
is_matching_container<T1, T2,
    typename boost::enable_if_c<is_container<T1>::value
                             && get_type_id<T1>::value == get_type_id<T2>::value>::type>
    : is_matching<typename element_type<T1>::type,
                  typename element_type<T2>::type> {};


// tuples match if the elements match
template <typename T1, typename T2, typename U1, typename U2> struct
is_matching<std::pair<T1, T2>, std::pair<U1, U2> >
{
    static const bool value = is_matching<T1, U1>::value
                           && is_matching<T2, U2>::value;
};


template <typename T1, typename T2> struct
is_matching<std::pair<T1, T2>, std::pair<T1, T2> >
    : true_type {};


// value<T> matches if type matches
template <typename T1, typename Reader, typename T2> struct
is_matching<value<T1, Reader>, T2>
    : is_matching<T1, T2> {};


// value<void> matches every container
template <typename T, typename Reader> struct
is_matching<value<void, Reader>, T>
    : is_container<T> {};


template <typename T, typename X, typename Enable = void> struct
is_element_matching
    : false_type {};


template <typename T, typename X> struct
is_element_matching<T, X, typename boost::enable_if<is_container<X> >::type>
    : is_matching<T, typename element_type<X>::type> {};


template <typename X, typename Reader> struct
is_element_matching<value<void, Reader>, X, typename boost::enable_if<is_container<X> >::type>
{
    static const bool value = is_bond_type<typename element_type<X>::type>::value
                           || is_container<typename element_type<X>::type>::value;
};


template <typename T, typename X, typename Enable = void> struct
is_map_element_matching
    : false_type {};


template <typename X, typename T> struct
is_map_element_matching<T, X, typename boost::enable_if<is_map_container<X> >::type>
    : is_matching<T, typename element_type<X>::type::second_type> {};


template <typename X, typename Reader> struct
is_map_element_matching<value<void, Reader>, X, typename boost::enable_if<is_map_container<X> >::type>
{
    static const bool value = is_bond_type<typename element_type<X>::type::second_type>::value
                           || is_container<typename element_type<X>::type::second_type>::value;
};


template <typename T, typename X, typename Enable = void> struct
is_map_key_matching
    : false_type {};


template <typename T, typename X> struct
is_map_key_matching<T, X, typename boost::enable_if<is_map_container<X> >::type>
    : is_matching<T, typename element_type<X>::type::first_type> {};


template <typename T> struct
is_basic_type
{
    static const bool value = !(is_container<T>::value || is_bond_type<T>::value);
};

template <> struct
is_basic_type<void>
    : false_type {};


// is_nested_container
template <typename T, typename Enable = void> struct
is_nested_container
    : false_type {};

template <typename T> struct
is_nested_container<T, typename boost::enable_if<is_map_container<T> >::type>
{
    static const bool value = !is_basic_type<typename element_type<T>::type::second_type>::value;
};

template <typename T> struct
is_nested_container<T, typename boost::enable_if<is_list_container<T> >::type>
{
    static const bool value = !is_basic_type<typename element_type<T>::type>::value;
};


template <typename T, typename Enable = void> struct
is_struct_container
    : false_type {};

template <typename T> struct
is_struct_container<T, typename boost::enable_if<is_map_container<T> >::type>
{
    static const bool value = has_schema<typename element_type<T>::type::second_type>::value
                           || is_struct_container<typename element_type<T>::type::second_type>::value;
};


template <typename T> struct
is_struct_container<T, typename boost::enable_if<is_list_container<T> >::type>
{
    static const bool value = has_schema<typename element_type<T>::type>::value
                           || is_struct_container<typename element_type<T>::type>::value;
};


// is_struct_container_field
template <typename T> struct
is_struct_container_field
    : is_struct_container<typename T::field_type> {};


// is_basic_container
template <typename T, typename Enable = void> struct
is_basic_container
    : false_type {};

template <typename T> struct
is_basic_container<T, typename boost::enable_if<is_map_container<T> >::type>
    : is_basic_type<typename element_type<T>::type::second_type> {};

template <typename T> struct
is_basic_container<T, typename boost::enable_if<is_list_container<T> >::type>
    : is_basic_type<typename element_type<T>::type> {};

template <typename T> struct
is_basic_container<T, typename boost::enable_if<is_set_container<T> >::type>
    : true_type {};


template <typename T, typename F> struct
is_matching_container_field
    : is_matching_container<T, typename F::field_type> {};


template <typename T> struct
is_container_field
    : is_container<typename T::field_type> {};


template <typename T, typename F> struct
is_matching_basic_field
    : is_matching_basic<T, typename F::field_type> {};


template <typename T, typename Reader> struct
is_basic_type<value<T, Reader> >
    : false_type {};


template <typename T1, typename T2> struct
is_basic_type<std::pair<T1, T2> >
    : false_type {};


template <typename T, typename X, typename Enable = void> struct
matching_fields
    : boost::mpl::copy_if<typename schema<T>::type::fields,
                          is_matching_basic_field<X, _>,
                          boost::mpl::front_inserter<boost::mpl::list<> > >
{
    BOOST_STATIC_ASSERT((is_basic_type<X>::value));
};


template <typename T, typename X> struct
matching_fields<T, X, typename boost::enable_if<is_container<X> >::type>
    : boost::mpl::copy_if<typename schema<T>::type::fields,
                          is_matching_container_field<X, _>,
                          boost::mpl::front_inserter<boost::mpl::list<> > > {};


template <typename T> struct
nested_fields
    : boost::mpl::copy_if<typename schema<T>::type::fields,
                          is_nested_field<_>,
                          boost::mpl::front_inserter<boost::mpl::list<> > > {};


template <typename T> struct
struct_fields
    : boost::mpl::copy_if<typename schema<T>::type::fields,
                          is_struct_field<_>,
                          boost::mpl::front_inserter<boost::mpl::list<> > > {};


template <typename T> struct
container_fields
    : boost::mpl::copy_if<typename schema<T>::type::fields,
                          is_container_field<_>,
                          boost::mpl::front_inserter<boost::mpl::list<> > > {};


template <typename T> struct
has_base
    : has_schema<typename schema<T>::type::base> {};


template <typename T>
BondDataType
GetTypeId(const T&)
{
    return get_type_id<T>::value;
}


template <typename Reader>
BondDataType
GetTypeId(const value<void, Reader>& value)
{
    return value.GetTypeId();
}


template <typename T, typename Reader> struct
get_type_id<value<T, Reader> >
    : get_type_id<T> {};


template <typename T1, typename T2> struct
get_type_id<std::pair<T1, T2> >
{
    static const std::pair<BondDataType, BondDataType> value;
};

template <typename T1, typename T2>
const std::pair<BondDataType, BondDataType>
get_type_id<std::pair<T1, T2> >::value = std::make_pair(
    get_type_id<typename remove_const<T1>::type>::value,
    get_type_id<T2>::value);

template <> struct
get_type_id<bool>
    : boost::integral_constant<BondDataType, BT_BOOL> {};

template <> struct
get_type_id<uint8_t>
    : boost::integral_constant<BondDataType, BT_UINT8> {};

template <> struct
get_type_id<uint16_t>
    : boost::integral_constant<BondDataType, BT_UINT16> {};

template <> struct
get_type_id<uint32_t>
    : boost::integral_constant<BondDataType, BT_UINT32> {};

template <> struct
get_type_id<uint64_t>
    : boost::integral_constant<BondDataType, BT_UINT64> {};

template <> struct
get_type_id<int8_t>
    : boost::integral_constant<BondDataType, BT_INT8> {};

template <> struct
get_type_id<int16_t>
    : boost::integral_constant<BondDataType, BT_INT16> {};

template <> struct
get_type_id<int32_t>
    : boost::integral_constant<BondDataType, BT_INT32> {};

template <> struct
get_type_id<int64_t>
    : boost::integral_constant<BondDataType, BT_INT64> {};

template <> struct
get_type_id<float>
    : boost::integral_constant<BondDataType, BT_FLOAT> {};

template <> struct
get_type_id<double>
    : boost::integral_constant<BondDataType, BT_DOUBLE> {};

template <> struct
get_type_id<void>
    : boost::integral_constant<BondDataType, BT_UNAVAILABLE> {};

template <typename T> struct
get_type_id
{
    typedef typename remove_const<T>::type U;

    static const BondDataType value =
        is_enum<T>::value           ? get_type_id<int32_t>::value :
        is_bond_type<T>::value      ? BT_STRUCT :
        is_set_container<U>::value  ? BT_SET :
        is_map_container<U>::value  ? BT_MAP :
        is_list_container<U>::value ? BT_LIST :
        is_string<U>::value         ? BT_STRING :
        is_wstring<U>::value        ? BT_WSTRING :
                                      get_type_id<typename aliased_type<T>::type>::value;
};

template <typename T>
const BondDataType get_type_id<T>::value;

template <typename T> struct
get_list_sub_type_id
{
    typedef typename remove_const<T>::type U;

    static const ListSubType value =
        is_nullable<U>::value       ? NULLABLE_SUBTYPE :
        is_blob<U>::value           ? BLOB_SUBTYPE     :
                                      NO_SUBTYPE;
};

template <typename T>
const ListSubType get_list_sub_type_id<T>::value;

class PrimitiveTypes
{
    struct Init
    {
        Init(uint32_t* _sizeof)
            : _sizeof(_sizeof)
        {}

        template <typename T>
        void operator()(const T&)
        {
            _sizeof[get_type_id<T>::value] = sizeof(T);
        }

        uint32_t* _sizeof;
    };

public:
    typedef boost::mpl::list
    <
        bool, float, double,
        uint8_t, uint16_t, uint32_t, uint64_t,
        int8_t,  int16_t,  int32_t,  int64_t
    >type;

    PrimitiveTypes(uint32_t* _sizeof)
    {
        boost::mpl::for_each<type>(Init(_sizeof));
    }
};

} // namespace bond
