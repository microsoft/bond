// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "reflection.h"
#include "detail/typeid_value.h"
#include "value.h"
#include "transforms.h"
#include "merge.h"
#include "schema.h"
#include "detail/inheritance.h"
#include <bond/protocol/simple_binary_impl.h>
#include <bond/protocol/simple_json_reader_impl.h>

namespace bond
{

namespace detail
{

class ParserCommon
{
protected:
    template <typename Transform>
    bool
    ReadFields(const boost::mpl::l_iter<boost::mpl::l_end>&, const Transform&)
    {
        return false;
    }

    template <typename Fields>
    void SkipFields(const Fields&)
    {
    }

    template <typename T, typename Transform>
    typename boost::disable_if<is_fast_path_field<T, Transform>, bool>::type
    OmittedField(const T&, const Transform& transform)
    {
        return transform.OmittedField(T::id, T::metadata, get_type_id<typename T::field_type>::value);
    }


    template <typename T, typename Transform>
    typename boost::enable_if<is_fast_path_field<T, Transform>, bool>::type
    OmittedField(const T& field, const Transform& transform)
    {
        return transform.OmittedField(field);
    }

    template <typename Transform>
    struct UnknownFieldBinder
        : detail::nonassignable
    {
        UnknownFieldBinder(Transform& transform)
            : transform(transform)
        {}

        template <typename T>
        bool Field(uint16_t id, const Metadata& /*metadata*/, const T& value) const
        {
            return transform.UnknownField(id, value);
        }

        Transform& transform;
    };

    template <typename Transform>
    UnknownFieldBinder<Transform> BindUnknownField(Transform& transform)
    {
        return UnknownFieldBinder<Transform>(transform);
    }
};

} // namespace detail

//
// StaticParser iterates serialized data using type schema and calls 
// specified transform for each data field. 
// The schema may be provided at compile-time (schema<T>::type::fields) or at runtime 
// (const RuntimeSchema&). StaticParser is used with protocols which don't 
// tag fields in serialized format with ids or types, e.g. Apache Avro protocol.
//
template <typename Input>
class StaticParser
    : protected detail::ParserInheritance<Input, StaticParser<Input> >,
      public detail::ParserCommon
{
public:
    StaticParser(Input input, bool base = false)
        : detail::ParserInheritance<Input, StaticParser<Input> >(input, base)
    {}

    
    template <typename Schema, typename Transform>
    bool
    Apply(const Transform& transform, const Schema& schema)
    {                                               
        detail::StructBegin(_input, _base);
        bool result = this->Read(schema, transform);
        detail::StructEnd(_input, _base);
        return result;
    }

    friend class detail::ParserInheritance<Input, StaticParser<Input> >;


protected:
    using detail::ParserInheritance<Input, StaticParser<Input> >::_input;
    using detail::ParserInheritance<Input, StaticParser<Input> >::_base;
    using detail::ParserCommon::ReadFields;

private:
    template <typename Fields>
    void SkipFields(const Fields& fields)
    {
        // Skip the structure by reading fields to Null transform
        ReadFields(fields, Null());
    }

    // use compile-time schema
    template <typename Fields, typename Transform>
    bool
    ReadFields(const Fields&, const Transform& transform)
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        if (detail::ReadFieldOmitted(_input))
            OmittedField(Head(), transform);
        else
            if (bool done = NextField(Head(), transform))
                return done;
        
        return ReadFields(typename boost::mpl::next<Fields>::type(), transform);
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<is_reader<Input>::value && !is_nested_field<T>::value
                             && !is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T&, const Transform& transform)
    {
        return transform.Field(T::id, T::metadata, value<typename T::field_type, Input>(_input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<is_reader<Input>::value && !is_nested_field<T>::value
                             && is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T& field, const Transform& transform)
    {
        return transform.Field(field, value<typename T::field_type, Input>(_input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<is_reader<Input>::value && is_nested_field<T>::value
                             && !is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T&, const Transform& transform)
    {
        return transform.Field(T::id, T::metadata, bonded<typename T::field_type, Input>(_input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<is_reader<Input>::value && is_nested_field<T>::value
                             && is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T& field, const Transform& transform)
    {
        return transform.Field(field, bonded<typename T::field_type, Input>(_input));
    }


    template <typename T, typename Transform>
    typename boost::disable_if<is_reader<Input, T>, bool>::type
    NextField(const T&, const Transform& transform)
    {
        return transform.Field(T::id, T::metadata, T::GetVariable(_input));
    }


    // use runtime schema
    template <typename Transform>
    bool
    ReadFields(const RuntimeSchema& schema, const Transform& transform)
    {
        bool done = false;

        for (const_enumerator<std::vector<FieldDef> > enumerator(schema.GetStruct().fields); enumerator.more() && !done;)
        {
            const FieldDef& field = enumerator.next();

            if (detail::ReadFieldOmitted(_input))
            {
                transform.OmittedField(field.id, field.metadata, field.type.id);
                continue;
            }

            if (field.type.id == bond::BT_STRUCT)
            {
                done = transform.Field(field.id, field.metadata, bonded<void, Input>(_input, RuntimeSchema(schema, field)));
            }
            else if (field.type.id == bond::BT_LIST || field.type.id == bond::BT_SET || field.type.id == bond::BT_MAP)
            {
                done = transform.Field(field.id, field.metadata, value<void, Input>(_input, RuntimeSchema(schema, field)));
            }
            else
            {
                done = detail::BasicTypeField(field.id, field.metadata, field.type.id, transform, _input);
            }
        }

        return done;
    }
};


//
// DynamicParser iterates serialized data using field tags included in the
// data by the protocol and calls specified transform for each data field. 
// DynamicParser uses schema only for auxiliary metadata, such as field 
// names or modifiers, and determines what fields are present from the data itself. 
// The schema may be provided at compile-time (schema<T>::type::fields) or at runtime 
// (const RuntimeSchema&). 
// DynamicParser is used with protocols which tag fields in serialized  
// format with ids and types, e.g. Mafia, Thrift or Protocol Buffers.
//
template <typename Input>
class DynamicParser
    : protected detail::ParserInheritance<Input, DynamicParser<Input> >,
      public detail::ParserCommon
{
public:
    DynamicParser(Input input, bool base)
        : detail::ParserInheritance<Input, DynamicParser<Input> >(input, base)
    {}

    
    template <typename Schema, typename Transform>
    bool
    Apply(const Transform& transform, const Schema& schema)
    {                                               
        detail::StructBegin(_input, _base);
        bool result = this->Read(schema, transform);
        detail::StructEnd(_input, _base);
        return result;
    }

    friend class detail::ParserInheritance<Input, DynamicParser<Input> >;


protected:
    using detail::ParserInheritance<Input, DynamicParser<Input> >::_input;
    using detail::ParserInheritance<Input, DynamicParser<Input> >::_base;


private:      
    template <typename Fields, typename Transform>
    bool 
    ReadFields(const Fields& fields, const Transform& transform)
    {
        uint16_t     id;
        BondDataType type;

        _input.ReadFieldBegin(type, id);

        ReadFields(fields, id, type, transform);

        if (!_base)
        {
            // If we are not parsing a base class, and we still didn't get to
            // the end of the struct, it means that:
            // 
            // 1) Actual data in the payload had deeper hierarchy than payload schema.
            //
            // or
            //
            // 2) We parsed only part of the hierarchy because that was what
            //    the transform "expected".
            //
            // In both cases we emit remaining fields as unknown
            
            for (; type != bond::BT_STOP; _input.ReadFieldEnd(), _input.ReadFieldBegin(type, id))
            {
                if (type == bond::BT_STOP_BASE)
                    transform.UnknownEnd();
                else
                    UnknownField(id, type, transform);
            }
        }

        _input.ReadFieldEnd();

        return false;
    }

    
    // use compile-time schema
    template <typename Fields, typename Transform>
    void
    ReadFields(const Fields&, uint16_t& id, BondDataType& type, const Transform& transform)
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        for (;;)
        {
            if (Head::id == id && get_type_id<typename Head::field_type>::value == type)
            {
                // Exact match
                NextField(Head(), transform);
            }
            else if (Head::id >= id && type != bond::BT_STOP && type != bond::BT_STOP_BASE)
            {
                // Unknown field or non-exact type match
                UnknownFieldOrTypeMismatch(
                    Head::id, 
                    is_basic_type<typename Head::field_type>::value, 
                    Head::metadata, 
                    id, 
                    type, 
                    transform);
            }
            else
            {
                OmittedField(Head(), transform);
                goto NextSchemaField;
            }

            _input.ReadFieldEnd();
            _input.ReadFieldBegin(type, id);

            if (Head::id < id || type == bond::BT_STOP || type == bond::BT_STOP_BASE)
            {
                NextSchemaField: return ReadFields(typename boost::mpl::next<Fields>::type(), id, type, transform);
            }
        }
    }


    template <typename Transform>
    void
    ReadFields(const boost::mpl::l_iter<boost::mpl::l_end>&, uint16_t& id, BondDataType& type, const Transform& transform)
    {
        for (; type != bond::BT_STOP && type != bond::BT_STOP_BASE;
               _input.ReadFieldEnd(), _input.ReadFieldBegin(type, id))
        {
            UnknownField(id, type, transform);
        }
    }

 
    template <typename T, typename Transform>
    typename boost::enable_if_c<is_nested_field<T>::value
                            && !is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T&, const Transform& transform)
    {
        return transform.Field(T::id, T::metadata, bonded<typename T::field_type, Input>(_input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<is_nested_field<T>::value
                             && is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T& field, const Transform& transform)
    {
        return transform.Field(field, bonded<typename T::field_type, Input>(_input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<!is_nested_field<T>::value
                             && !is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T&, const Transform& transform)
    {
        return transform.Field(T::id, T::metadata, value<typename T::field_type, Input>(_input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<!is_nested_field<T>::value
                             && is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T& field, const Transform& transform)
    {
        return transform.Field(field, value<typename T::field_type, Input>(_input));
    }


    // This function is called only when payload has unknown field id or type is not
    // matching exactly. This relativly rare so we don't inline the function to help 
    // the compiler to optimize the common path. 
    template <typename Transform>
    BOND_NO_INLINE
    bool
    UnknownFieldOrTypeMismatch(uint16_t expected_id, bool is_basic_type, const Metadata& metadata, uint16_t id, BondDataType type, const Transform& transform)
    {
        if (id == expected_id &&
            is_basic_type &&
            type != bond::BT_LIST &&
            type != bond::BT_SET &&
            type != bond::BT_MAP &&
            type != bond::BT_STRUCT)
        {
            return detail::BasicTypeField(expected_id, metadata, type, transform, _input);
        }
        else
        {
            return UnknownField(id, type, transform);
        }
    }


    // use runtime schema
    template <typename Transform>
    bool
    ReadFields(const RuntimeSchema& schema, const Transform& transform)
    {
        uint16_t                                id;
        BondDataType                            type;
        std::vector<FieldDef>::const_iterator   it = schema.GetStruct().fields.begin(),
                                                end = schema.GetStruct().fields.end();

        _input.ReadFieldBegin(type, id);

        for (;; _input.ReadFieldEnd(), _input.ReadFieldBegin(type, id))
        {
            while (it != end && (it->id < id || type == bond::BT_STOP || type == bond::BT_STOP_BASE))
            {
                const FieldDef& field = *it++;
                transform.OmittedField(field.id, field.metadata, field.type.id);
            }

            if (type == bond::BT_STOP || type == bond::BT_STOP_BASE)
            {
                break;
            }
            
            if (it != end && it->id == id)
            {
                const FieldDef& field = *it++;

                if (type == bond::BT_STRUCT)
                {
                    if (field.type.id == type)
                    {
                        transform.Field(id, field.metadata, bonded<void, Input>(_input, RuntimeSchema(schema, field)));
                        continue;
                    }
                }
                else if (type == bond::BT_LIST || type == bond::BT_SET || type == bond::BT_MAP)
                {
                    if (field.type.id == type)
                    {
                        transform.Field(id, field.metadata, value<void, Input>(_input, RuntimeSchema(schema, field)));
                        continue;
                    }
                }
                else
                {
                    detail::BasicTypeField(id, field.metadata, type, transform, _input);
                    continue;
                }
            }

            UnknownField(id, type, transform);
        }

        if (!_base)
        {
            // If we are not parsing a base class, and we still didn't get to
            // the end of the struct, it means that:
            // 
            // 1) Actual data in the payload had deeper hierarchy than payload schema.
            //
            // or
            //
            // 2) We parsed only part of the hierarchy because that was what
            //    the transform "expected".
            //
            // In both cases we emit remaining fields as unknown
            
            for (; type != bond::BT_STOP; _input.ReadFieldEnd(), _input.ReadFieldBegin(type, id))
            {
                if (type == bond::BT_STOP_BASE)
                {
                    transform.UnknownEnd();
                }
                else
                {
                    UnknownField(id, type, transform);
                }
            }
        }

        _input.ReadFieldEnd();

        return false;
    }


    template <typename T, typename Protocols, typename Validator>
    bool UnknownField(uint16_t, BondDataType type, const To<T, Protocols, Validator>&)
    {
        _input.Skip(type);
        return false;
    }


    template <typename Transform>
    bool UnknownField(uint16_t id, BondDataType type, const Transform& transform)
    {
        if (type == bond::BT_STRUCT)
        {
            return transform.UnknownField(id, bonded<void, Input>(_input, GetRuntimeSchema<Unknown>()));
        }
        else if (type == bond::BT_LIST || type == bond::BT_SET || type == bond::BT_MAP)
            return transform.UnknownField(id, value<void, Input>(_input, type));
        else
            return detail::BasicTypeField(id, schema<Unknown>::type::metadata, type, BindUnknownField(transform), _input);
    }
};


// DOM parser works with protocol implementations using Document Object Model, 
// e.g. JSON or XML. The parser assumes that fields in DOM are unordered and 
// are identified by either ordinal or metadata. DOM based protocols may loosly
// map to Bond meta-schema types thus the parser delegates to the protocol for 
// field type match checking. 
template <typename Input>
class DOMParser
    : protected detail::ParserInheritance<Input, DOMParser<Input> >
{
    typedef typename remove_reference<Input>::type Reader;

public:
    DOMParser(Input input, bool base = false)
        : detail::ParserInheritance<Input, DOMParser<Input> >(input, base)
    {}

    
    template <typename Schema, typename Transform>
    bool Apply(const Transform& transform, const Schema& schema)
    {
        if (!_base) _input.Parse();
        return this->Read(schema, transform);
    }

    friend class detail::ParserInheritance<Input, DOMParser<Input> >;


protected:
    using detail::ParserInheritance<Input, DOMParser<Input> >::_input;
    using detail::ParserInheritance<Input, DOMParser<Input> >::_base;

private:
    template <typename Fields>
    void SkipFields(const Fields&)
    {}

    // use compile-time schema
    template <typename Fields, typename Transform>
    bool ReadFields(const Fields&, const Transform& transform)
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        if (const typename Reader::Field* field = _input.FindField(
                Head::id,  
                Head::metadata, 
                get_type_id<typename Head::field_type>::value,
                is_enum<typename Head::field_type>::value))
        {
            Reader input(_input, *field);
            NextField(Head(), transform, input);
        }
        
        return ReadFields(typename boost::mpl::next<Fields>::type(), transform);
    }

    template <typename Transform>
    bool ReadFields(const boost::mpl::l_iter<boost::mpl::l_end>&, const Transform&)
    {
        return false;
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<is_nested_field<T>::value
                            && !is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T&, const Transform& transform, Input input)
    {
        return transform.Field(T::id, T::metadata, bonded<typename T::field_type, Input>(input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<is_nested_field<T>::value
                             && is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T& field, const Transform& transform, Input input)
    {
        return transform.Field(field, bonded<typename T::field_type, Input>(input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<!is_nested_field<T>::value
                             && !is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T&, const Transform& transform, Input input)
    {
        return transform.Field(T::id, T::metadata, value<typename T::field_type, Input>(input));
    }


    template <typename T, typename Transform>
    typename boost::enable_if_c<!is_nested_field<T>::value
                             && is_fast_path_field<T, Transform>::value, bool>::type
    NextField(const T& field, const Transform& transform, Input input)
    {
        return transform.Field(field, value<typename T::field_type, Input>(input));
    }


    // use runtime schema
    template <typename Transform>
    bool ReadFields(const RuntimeSchema& schema, const Transform& transform)
    {
        bool done = false;

        for (const_enumerator<std::vector<FieldDef> > enumerator(schema.GetStruct().fields); enumerator.more() && !done;)
        {
            const FieldDef& fieldDef = enumerator.next();
            
            if (const typename Reader::Field* field = _input.FindField(fieldDef.id, fieldDef.metadata, fieldDef.type.id))
            {
                Reader input(_input, *field);

                if (fieldDef.type.id == BT_STRUCT)
                    done = transform.Field(fieldDef.id, fieldDef.metadata, bonded<void, Input>(input, RuntimeSchema(schema, fieldDef)));
                else if (fieldDef.type.id == BT_LIST || fieldDef.type.id == BT_SET || fieldDef.type.id == BT_MAP)
                    done = transform.Field(fieldDef.id, fieldDef.metadata, value<void, Input>(input, RuntimeSchema(schema, fieldDef)));
                else
                    done = detail::BasicTypeField(fieldDef.id, fieldDef.metadata, fieldDef.type.id, transform, input);
            }
        }

        return done;
    }
};


} // namespace bond
