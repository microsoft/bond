// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "output_buffer.h"

#include <bond/core/blob.h>

#include <stdio.h>
#include <stdint.h>

namespace bond
{

class StdioOutputStream
{
public:
    StdioOutputStream(FILE* file)
        : _file(file)
    {}

    template<typename T>
    void Write(const T& value)
    {
        Write(&value, sizeof(value));
    }

    void Write(const blob& buffer)
    {
        Write(buffer.data(), buffer.length());
    }

    void Write(const void* value, uint32_t size)
    {
        fwrite(value, size, 1, _file);
    }

protected:
    FILE* _file;
};


// Returns a default OutputBuffer since StdioOutputStream is not capable
// of holding a memory buffer.
inline OutputBuffer CreateOutputBuffer(const StdioOutputStream& /*other*/)
{
    return OutputBuffer();
}

}
