// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "detail/tags.h"

namespace bond
{

class Null
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
    bool Base(const T& base) const
    {
        return Apply(*this, base);
    }

    template <typename T, template <typename BufferT, typename MarshaledBondedProtocolsT> class Reader, typename Buffer, typename MarshaledBondedProtocols>
    bool Base(const bonded<T, Reader<Buffer, MarshaledBondedProtocols>&>& base) const
    {
        return Apply<MarshaledBondedProtocols>(*this, base);
    }

    template <typename T>
    bool Field(uint16_t, const Metadata&, const T&) const
    {
        return false;
    }

    template <typename T>
    bool UnknownField(uint16_t, const T&) const
    {
        return false;
    }
};

}
