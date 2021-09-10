// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/inheritance.h"
#include "detail/omit_default.h"
#include "detail/parser_utils.h"
#include "detail/typeid_value.h"
#include "merge.h"
#include "reflection.h"
#include "schema.h"
#include "transforms.h"
#include "value.h"

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
    template <typename Fields, typename Transform, typename I = Input,
        typename boost::enable_if<detail::implements_field_omitting<I> >::type* = nullptr>
    bool
    ReadFields(const Fields&, const Transform& transform)
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        if (detail::ReadFieldOmitted(_input))
            detail::OmittedField(Head(), transform);
        else
            if (bool done = detail::NonBasicTypeField(Head(), transform, _input))
                return done;

        return ReadFields(typename boost::mpl::next<Fields>::type(), transform);
    }

    template <typename Fields, typename Transform, typename I = Input,
        typename boost::disable_if<detail::implements_field_omitting<I> >::type* = nullptr>
    bool
    ReadFields(const Fields&, const Transform& transform)
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        if (bool done = detail::NonBasicTypeField(Head(), transform, _input))
            return done;

        return ReadFields(typename boost::mpl::next<Fields>::type(), transform);
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
            const auto type = field.type.id;

            if (detail::ReadFieldOmitted(_input))
            {
                transform.OmittedField(field.id, field.metadata, type);
                continue;
            }

            if (type == bond::BT_STRUCT || type == bond::BT_LIST || type == bond::BT_SET || type == bond::BT_MAP)
            {
                done = detail::NonBasicTypeField(field, schema, transform, _input);
            }
            else
            {
                done = detail::BasicTypeField(field.id, field.metadata, type, transform, _input);
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

        bool done;

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

            ReadUnknownFields(type, id, transform);
            done = false;
        }
        else
        {
            done = (type == bond::BT_STOP);
        }

        _input.ReadFieldEnd();

        return done;
    }


    // use compile-time schema
    template <typename Fields, typename Transform>
    void
    ReadFields(const Fields&, uint16_t& id, BondDataType& type, const Transform& transform)
    {
        typedef typename boost::mpl::deref<Fields>::type Head;

        for (;;)
        {
            const bool moveSchemaField = Head::id <= id;
            if (Head::id == id && get_type_id<typename Head::field_type>::value == type)
            {
                // Exact match
                detail::NonBasicTypeField(Head(), transform, _input);
            }
            else if (Head::id >= id && type != bond::BT_STOP && type != bond::BT_STOP_BASE)
            {
                // Unknown field or non-exact type match
                UnknownFieldOrTypeMismatch<is_basic_type<typename Head::field_type>::value>(
                    Head::id,
                    Head::metadata,
                    id,
                    type,
                    transform);
            }
            else
            {
                detail::OmittedField(Head(), transform);
                goto NextSchemaField;
            }

            ReadSubsequentField(type, id);

            if (moveSchemaField)
            {
                NextSchemaField: return ReadFields(typename boost::mpl::next<Fields>::type(), id, type, transform);
            }
        }
    }


    template <typename Transform>
    void
    ReadFields(const boost::mpl::l_iter<boost::mpl::l_end>&, uint16_t& id, BondDataType& type, const Transform& transform)
    {
        for (; type != bond::BT_STOP && type != bond::BT_STOP_BASE; ReadSubsequentField(type, id))
        {
            UnknownField(id, type, transform);
        }
    }


    // This function is called only when payload has unknown field id or type is not
    // matching exactly. This relativly rare so we don't inline the function to help
    // the compiler to optimize the common path.
    template <bool IsBasicType, typename Transform>
    BOND_NO_INLINE
    typename boost::enable_if_c<IsBasicType, bool>::type
    UnknownFieldOrTypeMismatch(uint16_t expected_id, const Metadata& metadata, uint16_t id, BondDataType type, const Transform& transform)
    {
        if (id == expected_id &&
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

    template <bool IsBasicType, typename Transform>
    BOND_NO_INLINE
    typename boost::disable_if_c<IsBasicType, bool>::type
    UnknownFieldOrTypeMismatch(uint16_t /*expected_id*/, const Metadata& /*metadata*/, uint16_t id, BondDataType type, const Transform& transform)
    {
        return UnknownField(id, type, transform);
    }


    // use runtime schema
    template <typename Transform>
    void
    ReadFields(const RuntimeSchema& schema, uint16_t& id, BondDataType& type, const Transform& transform)
    {
        const auto& fields = schema.GetStruct().fields;

        for (auto it = fields.begin(), end = fields.end(); ; ReadSubsequentField(type, id))
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

                if (type == bond::BT_STRUCT || type == bond::BT_LIST || type == bond::BT_SET || type == bond::BT_MAP)
                {
                    if (field.type.id == type)
                    {
                        detail::NonBasicTypeField(field, schema, transform, _input);
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
    }


    template <typename Transform>
    void ReadUnknownFields(BondDataType& type, uint16_t& id, const Transform& transform)
    {
        for (; type != bond::BT_STOP; ReadSubsequentField(type, id))
        {
            if (type == bond::BT_STOP_BASE)
                transform.UnknownEnd();
            else
                UnknownField(id, type, transform);
        }
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


    void ReadSubsequentField(BondDataType& type, uint16_t& id)
    {
        _input.ReadFieldEnd();
        _input.ReadFieldBegin(type, id);
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
    typedef typename std::remove_reference<Input>::type Reader;

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
                std::is_enum<typename Head::field_type>::value))
        {
            Reader input(_input, *field);
            detail::NonBasicTypeField(Head(), transform, input);
        }

        return ReadFields(typename boost::mpl::next<Fields>::type(), transform);
    }

    template <typename Transform>
    bool ReadFields(const boost::mpl::l_iter<boost::mpl::l_end>&, const Transform&)
    {
        return false;
    }


    // use runtime schema
    template <typename Transform>
    bool ReadFields(const RuntimeSchema& schema, const Transform& transform)
    {
        bool done = false;

        for (const_enumerator<std::vector<FieldDef> > enumerator(schema.GetStruct().fields); enumerator.more() && !done;)
        {
            const FieldDef& fieldDef = enumerator.next();
            const auto type = fieldDef.type.id;

            if (const typename Reader::Field* field = _input.FindField(fieldDef.id, fieldDef.metadata, type))
            {
                Reader input(_input, *field);

                if (type == bond::BT_STRUCT || type == bond::BT_LIST || type == bond::BT_SET || type == bond::BT_MAP)
                {
                    done = detail::NonBasicTypeField(fieldDef, schema, transform, input);
                }
                else
                {
                    done = detail::BasicTypeField(fieldDef.id, fieldDef.metadata, type, transform, input);
                }
            }
        }

        return done;
    }
};


} // namespace bond


#ifdef BOND_LIB_TYPE
#if BOND_LIB_TYPE != BOND_LIB_TYPE_HEADER
#include "detail/parser_extern.h"
#endif
#else
#error BOND_LIB_TYPE is undefined
#endif
