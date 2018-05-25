// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "serialization.h"

#include <boost/assert.hpp>
#include <boost/optional.hpp>

namespace bond { namespace ext { namespace grpc { namespace detail
{
    template <typename T>
    class lazy_bonded
    {
    public:
        lazy_bonded() = default;

        explicit lazy_bonded(const ::grpc::ByteBuffer& buffer)
            : _value{},
              _buffer{ buffer }
        {}

        const bonded<T>& get() const
        {
            TryDeserialize();
            BOOST_ASSERT(_value);
            return *_value;
        }

        bonded<T>& get()
        {
            TryDeserialize();
            BOOST_ASSERT(_value);
            return *_value;
        }

        ::grpc::ByteBuffer& buffer() noexcept
        {
            return _buffer;
        }

    private:
        void TryDeserialize() const
        {
            if (!_value)
            {
                _value = Deserialize<T>(_buffer);
            }
        }

        mutable boost::optional<bonded<T>> _value;
        /*::grpc::*/ByteBuffer _buffer;
    };

} } } } //namespace bond::ext::grpc::detail
