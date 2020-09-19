// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

namespace bond
{
namespace detail
{

inline bool ValidateType(BondDataType src, BondDataType dst)
{
    switch (src)
    {
        case BT_UINT8:
        case BT_UINT16:
        case BT_UINT32:
            switch (dst)
            {
                case BT_UINT16:
                case BT_UINT32:
                case BT_UINT64:
                    return src <= dst;
                default:
                    break;
            }
            break;

        case BT_INT8:
        case BT_INT16:
        case BT_INT32:
            switch (dst)
            {
                case BT_INT16:
                case BT_INT32:
                case BT_INT64:
                    return src <= dst;
                default:
                    break;
            }
            break;

        case BT_FLOAT:
            return (dst == BT_DOUBLE);

	default:
            break;
    }

    return (src == dst);
}

struct struct_list
{
    const struct_list* last;
    const StructDef* dst;
    const StructDef* src;
};

inline void ValidateStruct(const RuntimeSchema& src,
                           const RuntimeSchema& dst,
                           const struct_list* list,
                           bool& identical);

inline bool ValidateType(const RuntimeSchema& src,
                         const RuntimeSchema& dst,
                         const struct_list* list,
                         bool& identical)
{
    if (dst.GetTypeId() != src.GetTypeId())
    {
        identical = false;
        return ValidateType(src.GetTypeId(), dst.GetTypeId());
    }

    if (dst.GetType().bonded_type != src.GetType().bonded_type)
    {
        identical = false;
    }

    if (dst.GetTypeId() == BT_STRUCT && !dst.GetType().bonded_type)
    {
        ValidateStruct(src, dst, list, identical);
        return true;
    }

    if (!dst.GetType().element.empty())
    {
        RuntimeSchema r_dst(dst, *dst.GetType().element);
        RuntimeSchema r_src(src, *src.GetType().element);

        if (!ValidateType(r_src, r_dst, list, identical))
        {
            return false;
        }
    }

    if (!dst.GetType().key.empty())
    {
        RuntimeSchema r_dst(dst, *dst.GetType().key);
        RuntimeSchema r_src(src, *src.GetType().key);

        if (!ValidateType(r_src, r_dst, list, identical))
        {
            return false;
        }
    }

    return true;
}

inline void ValidateFields(const RuntimeSchema& src,
                           const RuntimeSchema& dst,
                           const struct_list* list,
                           bool& identical)
{
    const StructDef& s_dst = dst.GetStruct();
    const StructDef& s_src = src.GetStruct();

    size_t n_dst = s_dst.fields.size();
    size_t n_src = s_src.fields.size();

    if (n_dst != n_src)
    {
        identical = false;
    }

    for (size_t i_dst = 0, i_src = 0; i_dst < n_dst; )
    {
        const FieldDef* f_dst = &s_dst.fields[i_dst];
        const FieldDef* f_src = (i_src < n_src) ? &s_src.fields[i_src] : NULL;

        if (f_src && f_src->id < f_dst->id)
        {
            i_src++;
            continue;
        }

        if (!f_src || f_src->id > f_dst->id)
        {
            if (f_dst->metadata.modifier == Required)
            {
                RequiredFieldMissingException(s_dst, *f_dst);
            }

            i_dst++;
            continue;
        }

        if (f_dst->metadata.modifier == Required &&
            f_src->metadata.modifier == Optional)
        {
            OptionalToRequiredException(s_src, s_dst, *f_src, *f_dst);
        }

        if (!ValidateType(RuntimeSchema(src, *f_src),
                          RuntimeSchema(dst, *f_dst),
                          list, identical))
        {
            FieldTypeIncompatibleException(s_src, s_dst, *f_src, *f_dst);
        }

        i_dst++;
        i_src++;
    }
}

inline void ValidateStruct(const RuntimeSchema& src,
                           const RuntimeSchema& dst,
                           const struct_list* list,
                           bool& identical)
{
    struct_list next = {
        list, &dst.GetStruct(), &src.GetStruct()
    };

    for (; list; list = list->last)
    {
        if (list->dst == next.dst &&
            list->src == next.src)
        {
            return;
        }
    }

    uint16_t d_dst = schema_depth(dst);
    uint16_t d_src = schema_depth(src);

    if (d_dst > d_src)
    {
        StructBaseDifferentException(src.GetStruct(), dst.GetStruct());
    }

    if (d_src > d_dst)
    {
        return ValidateStruct(src.GetBaseSchema(), dst, &next, identical = false);
    }

    if (dst.HasBase())
    {
        ValidateStruct(src.GetBaseSchema(), dst.GetBaseSchema(), &next, identical);
    }

    ValidateFields(src, dst, &next, identical);
}


// Checks if payload contains unknown fields
template <typename Protocols>
class SchemaValidator
    : public DeserializingTransform
{
public:
    void Begin(const Metadata&) const
    {}

    void End() const
    {}

    void UnknownEnd() const
    {}

    template <typename T>
    bool Base(const T& value) const
    {
        return Recurse(value);
    }

    template <typename T>
    bool Field(uint16_t, const Metadata&, const T& value) const
    {
        return Recurse(value);
    }

    template <typename T>
    [[noreturn]] bool UnknownField(uint16_t id, const T&) const
    {
        UnknownSchemaDefException(id);
    }

    template <typename T>
    void Container(const T& element, uint32_t size) const
    {
        while (size--)
            Recurse(element);
    }

    template <typename Key, typename T>
    void Container(const Key&, const T& value, uint32_t size) const
    {
        while (size--)
            Recurse(value);
    }

private:
    template <typename T>
    bool Recurse(const T&) const
    {
        return false;
    }

    template <typename T, typename Reader>
    typename boost::enable_if<is_basic_type<T>, bool>::type
    Recurse(const value<T, Reader>&) const
    {
        return false;
    }

    template <typename T, typename Reader>
    typename boost::disable_if<is_basic_type<T>, bool>::type
    Recurse(const value<T, Reader>& value) const
    {
        Apply<Protocols>(*this, value);
        return false;
    }

    template <typename T, typename Reader>
    bool Recurse(const bonded<T, Reader>& value) const
    {
        return Apply<Protocols>(*this, value);
    }
};

} // namespace detail
} // namespace bond
