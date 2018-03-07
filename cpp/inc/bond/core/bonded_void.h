// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bonded.h"
#include "detail/nonassignable.h"
#include "schema.h"
#include "select_protocol.h"

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


    /// @brief Move constructor
    bonded(bonded&& other) BOND_NOEXCEPT_IF(
        BOND_NOEXCEPT_EXPR(detail::move_data<Reader>(other._data))
        && std::is_nothrow_move_constructible<RuntimeSchema>::value)
        : _data(detail::move_data<Reader>(other._data)),
          _schema(std::move(other._schema)),
          _skip(std::move(other._skip)),
          _base(std::move(other._base))
    {
        other._skip = false;
    }


    ~bonded()
    {
        // Skip the struct if it wasn't deserialized
        if (_skip)
            detail::Skip(_data, *this, std::nothrow);
    }


    /// @brief Serialize bonded using specified protocol writer
    template <typename Protocols = BuiltInProtocols, typename Writer>
    typename boost::disable_if<uses_marshaled_bonded<typename Writer::Reader> >::type
    Serialize(Writer& output) const
    {
        Apply<Protocols>(SerializeTo<Protocols>(output), *this);
    }


    template <typename Protocols = BuiltInProtocols, typename Writer>
    typename boost::enable_if<uses_marshaled_bonded<typename Writer::Reader> >::type
    Serialize(Writer& output) const
    {
        if (_schema.GetType().bonded_type)
            detail::MarshalToBlob<Protocols>(*this, output);
        else
            Apply<Protocols>(SerializeTo<Protocols>(output), *this);
    }


    /// @brief Deserialize an object of type T
    template <typename T, typename Protocols = BuiltInProtocols>
    T Deserialize() const
    {
        T tmp;
        Apply<Protocols>(To<T, Protocols>(tmp), *this);
        return tmp;
    }


    /// @brief Deserialize to an object of type T
    template <typename Protocols = BuiltInProtocols, typename T>
    void Deserialize(T& var) const
    {
        Apply<Protocols>(To<T, Protocols>(var), *this);
    }


    /// @brief Deserialize to a bonded<T>
    template <typename Protocols = BuiltInProtocols, typename T>
    typename boost::enable_if<uses_marshaled_bonded<Reader, T> >::type
    Deserialize(bonded<T>& var) const
    {
        if (_schema.GetType().bonded_type)
        {
            bonded<T> tmp;
            _SelectProtocolAndApply<Protocols>(boost::ref(tmp));
            tmp.template Deserialize<Protocols>(var);
        }
        else
        {
            var = bonded<T>(*this);
        }
    }


    /// @brief Deserialize to a bonded<T>
    template <typename Protocols = BuiltInProtocols, typename T>
    typename boost::disable_if<uses_marshaled_bonded<Reader, T> >::type
    Deserialize(bonded<T>& var) const
    {
        var = bonded<T>(*this);
    }


    /// @brief Skip struct data in the underlying payload
    void Skip()
    {
        _skip = false;
        detail::Skip(_data, *this);
    }


    template <typename Protocols, typename Transform, typename U, typename ReaderT>
    friend typename boost::disable_if<detail::need_double_pass<Transform>, bool>::type inline
    detail::ApplyTransform(const Transform& transform, const bonded<U, ReaderT>& bonded);

    template <typename Protocols, typename Transform, typename U, typename ReaderT>
    friend typename boost::enable_if<detail::need_double_pass<Transform>, bool>::type inline
    detail::ApplyTransform(const Transform& transform, const bonded<U, ReaderT>& bonded);

    template <typename T, typename ReaderT>
    friend class bonded;

private:
    // Apply transform to serialized data
    template <typename Protocols, typename Transform>
    typename boost::enable_if<uses_marshaled_bonded<Reader, Transform>, bool>::type
    _Apply(const Transform& transform) const
    {
        if (_schema.GetType().bonded_type)
        {
            return _SelectProtocolAndApply<Protocols>(transform);
        }
        else
        {
            _skip = false;
            return detail::Parse<void, Protocols>(transform, _data, _schema, nullptr, _base);
        }
    }

    template <typename Protocols, typename Transform>
    typename boost::disable_if<uses_marshaled_bonded<Reader, Transform>, bool>::type
    _Apply(const Transform& transform) const
    {
        _skip = false;
        return detail::Parse<void, Protocols>(transform, _data, _schema, nullptr, _base);
    }


    template <typename Protocols, typename Transform>
    bool _SelectProtocolAndApply(const Transform& transform) const
    {
        BOOST_STATIC_ASSERT(uses_marshaled_bonded<Reader>::value);

        _skip = false;
        auto input = CreateInputBuffer(_data.GetBuffer(), detail::ReadBlob(_data));
        return SelectProtocolAndApply<Protocols>(_schema, input, transform).second;
    }


    Reader _data;
    const RuntimeSchema _schema;
    mutable bool _skip;
    const bool _base;
};

} // namespace bond
