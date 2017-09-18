// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/reflection.h>

namespace bond { namespace ext { namespace gRPC { namespace reflection {


/// @brief Method description in compile-time schema
template <
    typename Service,
    typename Input,
    typename Result,
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
};


template <
    typename Service,
    typename Input,
    typename Result,
    const bond::Metadata* metadata_ptr>
const bond::Metadata&
    MethodTemplate<Service, Input, Result, metadata_ptr>::metadata = *metadata_ptr;


} } } } // namespace bond::ext::gRPC::reflection
