// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include "bonded.h"
#include "schema.h"
#include "select_protocol.h"
#include "detail/nonassignable.h"

namespace bond
{


/// @brief Untyped specialization of bonded<T>, used for dynamic binding (i.e. schema known at runtime)
///
/// See [User's Manual](../../manual/bond_cpp.html#understanding-bondedt)
template <typename Reader>
class bonded<void, Reader>
    : detail::nonassignable
{
public:
    /// @brief Initialize from serialized data and runtime schema
    bonded(Reader data, const RuntimeSchema& schema, bool base = false)
        : _data(data),
          _schema(schema),
          _skip(true),
          _base(base)
    {}

    /// @brief Copy constructor
    bonded(const bonded& other)
        : _data(other._data),
          _schema(other._schema),
          _skip(true),
          _base(false)
    {}

    /// @brief Explicit cast from bonded<T>
    template <typename T, typename ReaderT>
    explicit bonded(const bonded<T, ReaderT>& other)
        : _data(other._data),
          _schema(other._schema.get() ? other._schema : GetRuntimeSchema<T>()),
          _skip(true),
          _base(false)
    {}


#ifndef BOND_NO_CXX11_RVALUE_REFERENCES
    /// @brief Move constructor
    bonded(bonded&& other)
        : _data(std::move(other._data)),
          _schema(std::move(other._schema)),
          _skip(std::move(other._skip)),
          _base(std::move(other._base))
    {
        other._skip = false;
    }
#endif

    
    ~bonded()
    {
        // Skip the struct if it wasn't deserialized
        if (_skip)
            detail::Skip(_data, *this, std::nothrow);
    }

   
    /// @brief Serialize bonded using specified protocol writer
    template <typename Writer>
    typename boost::disable_if<uses_marshaled_bonded<typename Writer::Reader> >::type
    Serialize(Writer& output) const
    {
        Apply(SerializeTo(output), *this);
    }


    template <typename Writer>
    typename boost::enable_if<uses_marshaled_bonded<typename Writer::Reader> >::type
    Serialize(Writer& output) const
    {
        if (_schema.GetType().bonded_type)    
            detail::MarshalToBlob(*this, output);
        else
            Apply(SerializeTo(output), *this);
    }

    
    /// @brief Deserialize an object of type T
    template <typename T>
    T Deserialize() const
    {
        T tmp;
        Apply(To<T>(tmp), *this);
        return tmp;
    }

    
    /// @brief Deserialize to an object of type T
    template <typename T>
    void Deserialize(T& var) const
    {
        Apply(To<T>(var), *this);
    }


    /// @brief Deserialize to a bonded<T>
    template <typename T>
    void Deserialize(bonded<T>& var) const
    {
        if (uses_marshaled_bonded<Reader>::value && _schema.GetType().bonded_type)
        {
            bonded<T> tmp;
            _SelectProtocolAndApply(boost::ref(tmp));
            tmp.Deserialize(var);
        }
        else
        {
            var = bonded<T>(*this);
        }
    }


    /// @brief Skip struct data in the underlying payload
    void Skip()
    {
        _skip = false;
        detail::Skip(_data, *this);
    }


    template <typename Transform, typename U, typename ReaderT>
    friend typename boost::disable_if<detail::need_double_pass<Transform>, bool>::type inline
    Apply(const Transform& transform, const bonded<U, ReaderT>& bonded);

    template <typename Transform, typename U, typename ReaderT>
    friend typename boost::enable_if<detail::need_double_pass<Transform>, bool>::type inline
    Apply(const Transform& transform, const bonded<U, ReaderT>& bonded);

    template <typename T, typename ReaderT> 
    friend class bonded;

private:    
    // Apply transform to serialized data
    template <typename Transform>
    bool _Apply(const Transform& transform) const
    {        
        if (uses_marshaled_bonded<Reader>::value && _schema.GetType().bonded_type)
        {
            return _SelectProtocolAndApply(transform);
        }
        else
        {
            _skip = false;
            return detail::Parse<void>(transform, _data, _schema, NULL, _base);
        }
    }

    
    template <typename Transform>
    typename boost::enable_if<uses_marshaled_bonded<Reader, Transform>, bool>::type
    _SelectProtocolAndApply(const Transform& transform) const
    {
        _skip = false;
        InputBuffer input(detail::ReadBlob(_data));
        return SelectProtocolAndApply(_schema, input, transform).second;
    }


    template <typename Transform>
    typename boost::disable_if<uses_marshaled_bonded<Reader, Transform>, bool>::type
    _SelectProtocolAndApply(const Transform&) const
    {
        BOOST_ASSERT(false);
        return false;
    }


    Reader _data;
    const RuntimeSchema _schema;
    mutable bool _skip;
    const bool _base;
};

} // namespace bond
