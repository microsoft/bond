// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "detail/string_stream.h"
#include <bond/core/bond_types.h>

#define BOND_THROW(x, y)  throw x((bond::detail::basic_string_stream<1024>() << y).content());

namespace bond
{

//
// Base type for all bond exceptions.
//
class Exception
    : public std::exception,
      public SerializableExceptionBase
{
public:
    const char* what() const BOND_NOEXCEPT
    {
        return message.c_str();
    }

    virtual ~Exception() BOND_NOEXCEPT
    {}

protected:
    Exception(const char* msg) BOND_NOEXCEPT
    {
        message = msg;
    }

    Exception() BOND_NOEXCEPT
    {}
};


struct CoreException
    : Exception
{
    CoreException(const char* message)
        : Exception(message)
    {}
};


inline void MergerContainerException(uint32_t payload, uint32_t obj)
{
    BOND_THROW(CoreException,
          "Merge failed: container mismatch, length in the payload: "
          << payload << " length in the object: " << obj);
}


template <typename Key>
inline void ElementNotFoundException(const Key& key)
{
    BOND_THROW(CoreException,
          "Map element not found: key: " << key);
}


inline void UnknownProtocolException()
{
    BOND_THROW(CoreException,
          "Unmarshaling failed: unsupported protocol"); 
}


inline void UnknownProtocolException(uint16_t magic)
{
    BOND_THROW(CoreException,
          "Unsupported protocol: " 
          << (char)(magic & 0xFF) << (char)(magic >> 8)); 
}


inline void NothingException()
{
    BOND_THROW(CoreException,
          "Field value is 'nothing'");
}


inline void InvalidEnumValueException(const char* value, const char* enum_)
{
    BOND_THROW(bond::CoreException, 
        "Unexpected value " << value << " for enum " << enum_);
}


inline void InvalidEnumValueException(int32_t value, const char* enum_)
{
    BOND_THROW(bond::CoreException, 
        "Unexpected value " << value << " for enum " << enum_);
}


inline void RapidJsonException(const char* error, size_t offset)
{
    BOND_THROW(CoreException,
        "JSON parser error: " << error << " at offset " << offset);
}

 
struct StreamException
    : Exception
{
    StreamException(const char* message)
        : Exception(message)
    {}
};


struct SchemaValidateException
    : CoreException
{
    SchemaValidateException(const char* message)
        : CoreException(message)
    {}
};


inline void StructBaseDifferentException(const StructDef& src,
                                         const StructDef& dst)
{
    BOND_THROW(SchemaValidateException, 
        "Schemas are incompatible; struct base different: " 
        << src.metadata.name << ", " << dst.metadata.name);
}


inline void RequiredFieldMissingException(const StructDef& s_dst,
                                          const FieldDef& f_dst)
{
    BOND_THROW(SchemaValidateException, 
        "Schemas are incompatible; required field missing: " 
        << s_dst.metadata.name << "::" << f_dst.metadata.name);
}


inline void OptionalToRequiredException(const StructDef& s_src,
                                        const StructDef& s_dst,
                                        const FieldDef& f_src,
                                        const FieldDef& f_dst)
{
    BOND_THROW(SchemaValidateException, 
        "Schemas are incompatible; required modifier removed: " 
        << s_src.metadata.name << "::" << f_src.metadata.name << ", " 
        << s_dst.metadata.name << "::" << f_dst.metadata.name);
}


inline void FieldTypeIncompatibleException(const StructDef& s_src,
                                           const StructDef& s_dst,
                                           const FieldDef& f_src,
                                           const FieldDef& f_dst)
{
    BOND_THROW(SchemaValidateException, 
        "Schemas are incompatible; field types incompatible: " 
        << s_src.metadata.name << "::" << f_src.metadata.name << ", " 
        << s_dst.metadata.name << "::" << f_dst.metadata.name);
}


inline void UnknownSchemaDefException(uint16_t id)
{
    BOND_THROW(SchemaValidateException, 
        "Failed to validate schema compatibility; "
        "SchemaDef contains unknown field: " << id);
}

} // namespace bond
