// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/omit_default.h"
#include "detail/tags.h"
#include "exception.h"
#include "reflection.h"
#include "transforms.h"


namespace bond
{


template <typename T, typename Writer, typename Protocols = BuiltInProtocols>
class Merger
    : public Serializer<Writer, Protocols>
{
public:
    typedef T FastPathType;

    using Serializer<Writer, Protocols>::Base;
    using Serializer<Writer, Protocols>::Field;
    using Serializer<Writer, Protocols>::Write;
    using Serializer<Writer, Protocols>::OmittedField;
    using Serializer<Writer, Protocols>::Container;

    Merger(const T& var, Writer& output, bool base = false)
        : Serializer<Writer, Protocols>(output, base),
          _var(var)
    {}

    template <typename Pass0>
    Merger<T, Pass0, Protocols> Rebind(Pass0& pass0) const
    {
        return Merger<T, Pass0, Protocols>(_var, pass0);
    }

    template <typename X, typename Reader>
    typename boost::enable_if<has_schema<X>, bool>::type
    Base(const bonded<X, Reader>& value) const
    {
        return Apply<Protocols>(Merger<typename schema<T>::type::base, Writer, Protocols>(_var, _output, true), value);
    }


    template <typename FieldT, typename X>
    typename boost::enable_if_c<is_struct_field<FieldT>::value
                             || is_struct_container_field<FieldT>::value, bool>::type
    Field(const FieldT&, const X& value) const
    {
        _output.WriteFieldBegin(GetTypeId(value), FieldT::id, FieldT::metadata);
        Merge(FieldT::GetVariable(_var), value);
        _output.WriteFieldEnd();
        return false;
    }


    template <typename FieldT, typename X>
    typename boost::disable_if_c<is_struct_field<FieldT>::value
                              || is_struct_container_field<FieldT>::value, bool>::type
    Field(const FieldT&, const X&) const
    {
        return this->Field(FieldT::id, FieldT::metadata, FieldT::GetVariable(_var));
    }


    template <typename FieldT>
    bool OmittedField(const FieldT&) const
    {
        return this->Field(FieldT::id, FieldT::metadata, FieldT::GetVariable(_var));
    }


    template <typename X, typename Reader>
    typename boost::enable_if<is_element_matching<value<X, Reader>, T> >::type
    Container(const value<X, Reader>& element, uint32_t size) const
    {
        Merge(_var, element, size);
    }


    template <typename X, typename Reader>
    typename boost::enable_if<is_element_matching<bonded<X, Reader>, T> >::type
    Container(const bonded<X, Reader>& element, uint32_t size) const
    {
        Merge(_var, element, size);
    }


    template <typename Key, typename X, typename Reader>
    typename boost::enable_if_c<is_map_element_matching<X, T>::value
                             && is_map_key_matching<Key, T>::value>::type
    Container(const value<Key, Reader>& key, const X& value, uint32_t size) const
    {
        Merge(_var, key, value, size);
    }


protected:
    using Serializer<Writer, Protocols>::_output;


private:
    template <typename U, typename X>
    void Merge(const U& var, const X& value) const
    {
        Apply<Protocols>(Merger<U, Writer, Protocols>(var, _output), value);
    }


    template <typename U, typename X>
    void Merge(const U& var, const X& element, uint32_t size) const
    {
        if (size != container_size(var))
            MergerContainerException(size, container_size(var));

        _output.WriteContainerBegin(size, get_type_id<typename element_type<U>::type>::value);

        for (const_enumerator<U> items(var); items.more();)
            Merge(items.next(), element);

        _output.WriteContainerEnd();
    }


    template <typename U, typename Key, typename X>
    void Merge(const U& var, const Key& key, const X& value, uint32_t size) const
    {
        if (size != container_size(var))
            MergerContainerException(size, container_size(var));

        _output.WriteContainerBegin(size, get_type_id<typename element_type<U>::type>::value);

        typename element_type<U>::type::first_type k;

        while (size--)
        {
            key.template Deserialize<Protocols>(k);

            Write(k);

            // Elements of a map migth be serialized out of order, so we must
            // look up the element to merge by key.
            Merge(mapped_at(var, k), value);
        }

        _output.WriteContainerEnd();
    }

    const T& _var;
};


} // namespace bond
