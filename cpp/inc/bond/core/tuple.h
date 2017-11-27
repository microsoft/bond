// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bond.h"
#include "detail/tuple_fields.h"

#include <tuple>

namespace bond
{

// Specialize bond::schema<T> for std::tuple<T...>
// This allows treating instances of std::tuple as Bond structs.
template <typename ...T>
struct schema<std::tuple<T...>>
{
    struct type
    {
        typedef no_base base;
        typedef typename detail::tuple_fields<std::tuple<T...>, 0, T...>::type fields;

        static const Metadata metadata;

        type()
        {
            // Force instantiation of template statics
            (void)metadata;
        }

        static Metadata GetMetadata()
        {
            Metadata m = reflection::MetadataInit(
                "tuple", "bond.tuple", reflection::Attributes());

            std::string params;

            boost::mpl::for_each<typename detail::param_list<T...>::type>(
                detail::TypeListBuilder(params));

            m.name += "<" + params + ">";
            m.qualified_name += "<" + params + ">";

            return m;
        }
    };
};


template <typename... T>
const Metadata schema<std::tuple<T...>>::type::metadata
    = schema<std::tuple<T...>>::type::GetMetadata();


template <typename Protocols = BuiltInProtocols, typename Writer, typename... T>
inline void Pack(Writer& writer, T&&... args)
{
    Serialize<Protocols>(std::forward_as_tuple(args...), writer);
}


template <typename Protocols = BuiltInProtocols, typename Reader, typename... T>
inline void Unpack(Reader reader, T&... arg)
{
    auto pack = std::tie(arg...);
    Deserialize<Protocols>(reader, pack);
}

} // namepsace bond
