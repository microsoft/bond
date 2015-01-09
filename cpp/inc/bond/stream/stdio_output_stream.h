// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <stdio.h>
#include <stdint.h>
#include <bond/core/blob.h>

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

}
