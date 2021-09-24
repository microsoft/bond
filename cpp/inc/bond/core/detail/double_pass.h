// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/bond_fwd.h>

#include <boost/utility/enable_if.hpp>

namespace bond
{
    namespace detail
    {
        template <typename Transform, typename Enable = void> struct
        need_double_pass
            : std::false_type {};

        template <typename Transform> struct
        need_double_pass<
            Transform,
            typename boost::enable_if_c<!std::is_same<typename Transform::writer_type,
                                                 typename Transform::writer_type::Pass0>::value>::type
        > : std::true_type {};

        template <typename Protocols, typename Transform, typename T>
        inline bool DoublePassApply(const Transform& transform, const T& value)
        {
            typedef typename Transform::writer_type Writer;
            typedef Serializer<Writer, Protocols> Serializer;

            typename Writer::Pass0::Buffer output;
            typename Writer::Pass0 pass0(output, transform.Serializer::_output);

            Apply<Protocols>(transform.Rebind(pass0), value);
            return transform.Serializer::_output.WithPass0(pass0), Apply<Protocols>(transform, value);
        }

    } // namespace detail

} // namespace bond
