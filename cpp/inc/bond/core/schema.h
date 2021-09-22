// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/once.h"
#include "detail/tags.h"
#include "reflection.h"
#include "runtime_schema.h"

#include <boost/bind/bind.hpp>
#include <boost/make_shared.hpp>

#include <limits>
#include <algorithm>
#include <iterator>

namespace bond
{


inline RuntimeSchema::RuntimeSchema(const RuntimeSchema& schema)
    : schema(schema.schema),
      type(schema.type),
      instance(schema.instance)
{}

inline RuntimeSchema::RuntimeSchema(const SchemaDef& schema)
    : schema(&schema),
      type(&schema.root)
{}

inline RuntimeSchema::RuntimeSchema(const boost::shared_ptr<SchemaDef>& schema)
    : schema(schema.get()),
      type(&schema->root),
      instance(schema)
{}

inline RuntimeSchema::RuntimeSchema(const RuntimeSchema& schema, const TypeDef& type)
    : schema(schema.schema),
      type(&type),
      instance(schema.instance)
{}

inline RuntimeSchema::RuntimeSchema(const RuntimeSchema& schema, const FieldDef& field)
    : schema(schema.schema),
      type(&field.type),
      instance(schema.instance)
{}

inline bool RuntimeSchema::HasBase() const
{
    return !GetStruct().base_def.empty();
}

inline RuntimeSchema RuntimeSchema::GetBaseSchema() const
{
    return RuntimeSchema(*this, GetStruct().base_def.value());
}

inline const StructDef& RuntimeSchema::GetStruct() const
{
    BOOST_ASSERT(type->id == BT_STRUCT);
    return schema->structs[type->struct_def];
}

inline BondDataType RuntimeSchema::GetTypeId() const
{
    return type->id;
}


template <typename Protocols = BuiltInProtocols, typename Writer>
inline void Serialize(const RuntimeSchema& schema, Writer& output)
{
    Apply<Protocols>(SerializeTo<Protocols>(output), schema.GetSchema());
}


template <typename Protocols = BuiltInProtocols, typename Writer>
inline void Marshal(const RuntimeSchema& schema, Writer& output)
{
    Apply<Protocols>(MarshalTo<Protocols>(output), schema.GetSchema());
}

class InitSchemaDef;

namespace detail
{
    inline uint16_t schema_depth(const RuntimeSchema& schema)
    {
        uint16_t depth = 1;

        if (schema.HasBase())
            depth += schema_depth(schema.GetBaseSchema());

        return depth;
    }

    template <typename T, typename Unused = void>
    class SchemaCache
    {
    public:
        static const SchemaDef& Get()
        {
            // The schema object can't be initialized as a global static
            // because InitSchemaDef uses schema's and fields' metadata which
            // are global static variables, and we can't depends on them being
            // initialized before the schema. Instead we initialize the schema
            // on the first call to Get().
            // Note that older versions of GNU C++ don't handle rvalue argument
            // forwarding in Boost call_once implementation so we are using
            // the old trusty boost::bind.
            call_once(flag, boost::bind(&AppendStructDef, &schema));
            return schema;
        }

        static void AppendStructDef(SchemaDef* s);

    private:
        static SchemaDef schema;
        static once_flag flag;
    };

    // We need the Unused template parameter because otherwise the 'schema'
    // static would not be a static member of a class template and would have
    // to be defined in a .cpp and we want Bond to be header-only for some
    // scenarios.
    template <typename Unused>
    class SchemaCache<Unknown, Unused>
    {
    public:
        static const SchemaDef& Get()
        {
            return schema;
        }

        static SchemaDef NewSchemaDef()
        {
            // SchemaDef for unknown types: struct with no fields
            SchemaDef s;
            s.root.id = BT_STRUCT;
            s.structs.resize(1);
            return s;
        }

    private:
        static const SchemaDef schema;
    };

    template <typename T, typename Unused>
    SchemaDef SchemaCache<T, Unused>::schema;

    template <typename Unused>
    const SchemaDef SchemaCache<Unknown, Unused>::schema
        = SchemaCache<Unknown>::NewSchemaDef();

    template <typename T, typename Unused>
    once_flag SchemaCache<T, Unused>::flag;
}


struct SchemaReader
{
    using Parser = StaticParser<SchemaReader&>;
};


template <typename Unused> struct
uses_marshaled_bonded<SchemaReader&, Unused> : std::false_type
{};


//
// InitSchemaDef transform creates an instance of runtime schema for the input
//
class InitSchemaDef
    : public SerializingTransform
{
public:
    InitSchemaDef(SchemaDef& schema)
        : _schema(schema),
          _struct_def(schema.structs.size())
    {
        _schema.structs.push_back(StructDef());
    }


    void Begin(const Metadata& metadata) const
    {
        This().metadata = metadata;
    }

    void End() const
    {
    }

    template <typename T>
    bool Base(const T& /*value*/) const
    {
        TypeDef type = GetTypeDef<typename remove_bonded<T>::type>();
        This().base_def.set(type);
        return false;
    }

    // field
    template <typename T>
    bool Field(uint16_t id, const Metadata& metadata, const T&) const
    {
        FieldDef field;

        field.id = id;
        field.metadata = metadata;
        field.type = GetTypeDef<typename remove_bonded_value<T>::type>();

        This().fields.push_back(field);
        return false;
    }

private:
    template <typename T>
    typename boost::enable_if<is_basic_type<T>, TypeDef>::type
    GetTypeDef() const
    {
        TypeDef type;

        type.id = get_type_id<T>::value;

        return type;
    }


    template <typename T>
    typename boost::enable_if_c<is_container<T>::value && !is_map_container<T>::value, TypeDef>::type
    GetTypeDef() const
    {
        TypeDef type;

        type.id = get_type_id<T>::value;
        type.element.set() = GetTypeDef<typename element_type<T>::type>();

        return type;
    }


    template <typename T>
    typename boost::enable_if<is_map_container<T>, TypeDef>::type
    GetTypeDef() const
    {
        TypeDef type;

        type.id = get_type_id<T>::value;
        type.key.set() = GetTypeDef<typename element_type<T>::type::first_type>();
        type.element.set() = GetTypeDef<typename element_type<T>::type::second_type>();

        return type;
    }


    template <typename T>
    typename boost::enable_if<is_bond_type<T>, TypeDef>::type
    GetTypeDef() const
    {
        TypeDef type;

        type.id = get_type_id<T>::value;
        type.struct_def = GetStructDef<typename remove_bonded<T>::type>();
        type.bonded_type = is_bonded<T>::value;

        return type;
    }


    template <typename T>
    uint16_t GetStructDef() const
    {
        const auto& structs = _schema.structs;

        BOOST_ASSERT(structs.size() <= (std::numeric_limits<uint16_t>::max)());

        auto it = std::find_if(
            std::begin(structs),
            std::end(structs),
            [](const StructDef& def)
            {
                return def.metadata.qualified_name == schema<T>::type::metadata.qualified_name;
            });

        const auto index = static_cast<uint16_t>(std::distance(std::begin(structs), it));

        if (it == std::end(structs))
        {
            detail::SchemaCache<T>::AppendStructDef(&_schema);
        }

        return index;
    }


    // Note that This() returns a reference to StructDef in the structs vector
    // which may be invalidated when the vector grows. In particular This()
    // can't be used in an expression that may result in adding items to the
    // vector.
    StructDef& This() const
    {
        BOOST_ASSERT(_schema.structs.size() > _struct_def);
        return _schema.structs[_struct_def];
    }


    SchemaDef&      _schema;
    const size_t    _struct_def;
};

namespace detail
{

template <typename T, typename Unused>
void SchemaCache<T, Unused>::AppendStructDef(SchemaDef* s)
{
    Apply<T>(InitSchemaDef{ *s });
}

} // detail

/// @brief Returns an instance of RuntimeSchema for a user defined struct
template <typename T>
inline RuntimeSchema GetRuntimeSchema()
{
    return RuntimeSchema(detail::SchemaCache<T>::Get());
}

template <typename T>
inline RuntimeSchema GetRuntimeSchema(const T&)
{
    return RuntimeSchema(detail::SchemaCache<T>::Get());
}


inline RuntimeSchema element_schema(const RuntimeSchema& schema)
{
    if (!schema.GetType().element.empty())
        return RuntimeSchema(schema, schema.GetType().element.value());
    else
        return GetRuntimeSchema<Unknown>();
}


inline RuntimeSchema key_schema(const RuntimeSchema& schema)
{
    if (!schema.GetType().key.empty())
        return RuntimeSchema(schema, schema.GetType().key.value());
    else
        return GetRuntimeSchema<Unknown>();
}


/// @brief Returns a const reference to a map of values for a user defined enum
template <typename T, typename Map = std::map<T, std::string> >
inline const Map& GetEnumValues()
{
    return GetValueToNameMap(T{}, detail::mpl::identity<Map>{});
}


/// @brief Returns a const reference to a map of names for a user defined enum
template <typename T, typename Map = std::map<std::string, T> >
inline const Map& GetEnumNames()
{
    return GetNameToValueMap(T{}, detail::mpl::identity<Map>{});
}


} // namespace bond
