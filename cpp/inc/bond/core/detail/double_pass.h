// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

namespace bond
{
    namespace detail
    {
        template <typename Transform, typename Enable = void> struct 
        need_double_pass
            : false_type {};

        template <typename Transform> struct 
        need_double_pass<
            Transform, 
            typename boost::enable_if_c<!is_same<typename Transform::writer_type, 
                                                 typename Transform::writer_type::Pass0>::value>::type
        > : true_type {};

        template <typename Transform, typename T>
        inline bool DoublePassApply(const Transform& transform, const T& value)
        {
            typedef typename Transform::writer_type Writer;

            typename Writer::Pass0::Buffer output;
            typename Writer::Pass0 pass0(output, transform.Serializer<Writer>::_output);

            Apply(transform.Rebind(pass0), value);
            return transform.Serializer<Writer>::_output.WithPass0(pass0), Apply(transform, value);
        }

    } // namespace detail

} // namespace bond
