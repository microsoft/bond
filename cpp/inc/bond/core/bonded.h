// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "bond_fwd.h"
#include "detail/double_pass.h"
#include "detail/marshaled_bonded.h"
#include "detail/protocol_visitors.h"
#include "protocol.h"
#include "runtime_schema.h"
#include "select_protocol_fwd.h"

namespace bond
{
namespace detail
{

template <typename Protocols, typename Transform, typename T, typename Reader>
typename boost::disable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const bonded<T, Reader>& bonded);

template <typename Protocols, typename Transform, typename T, typename Reader>
typename boost::enable_if<need_double_pass<Transform>, bool>::type inline
ApplyTransform(const Transform& transform, const bonded<T, Reader>& bonded);


// Helper function move_data for dealing with [not] moving a Reader& in bonded<T, Reader&>
template <typename T, typename U>
typename boost::enable_if<std::is_reference<T>, T>::type
inline move_data(U& data) BOND_NOEXCEPT
{
    BOOST_STATIC_ASSERT(std::is_same<T, U&>::value);
    return data;
}

template <typename T, typename U>
typename boost::disable_if<std::is_reference<T>, T&&>::type
inline move_data(U& data) BOND_NOEXCEPT_IF(
    std::is_nothrow_move_constructible<T>::value)
{
    BOOST_STATIC_ASSERT(std::is_same<T, U>::value);
    return std::move(data);
}

} // namespace detail


template <typename T, typename Reader, typename Unused = void> struct
is_marshaled_bonded
    : std::integral_constant<bool,
        uses_marshaled_bonded<Reader, Unused>::value
        && is_bonded<T>::value> {};


/// @brief Represents data for a struct T known at compile-time
///
/// See [User's Manual](../../manual/bond_cpp.html#understanding-bondedt)
template <typename T, typename Reader>
class bonded
{
public:
    /// @brief Default constructor
    bonded()
        : _skip(false),
          _base(false)
    {}

    /// @brief Copy constructor
    bonded(const bonded& bonded)
        : _data(bonded._data),
          _schema(bonded._schema),
          _skip(true),
          _base(false)
    {}

    /// @brief Move constructor
    bonded(bonded&& bonded) BOND_NOEXCEPT_IF(
        BOND_NOEXCEPT_EXPR(detail::move_data<Reader>(bonded._data))
        && std::is_nothrow_move_constructible<RuntimeSchema>::value)
        : _data(detail::move_data<Reader>(bonded._data)),
          _schema(std::move(bonded._schema)),
          _skip(std::move(bonded._skip)),
          _base(std::move(bonded._base))
    {
        bonded._skip = false;
    }

    bonded& operator=(const bonded& rhs) = default;

    /// @brief Explicit up/down-casting from/to bonded of a derived type
    template <typename U, typename ReaderT>
    explicit
    bonded(const bonded<U, ReaderT>& bonded)
        : _data(bonded._data),
          _schema(bonded._schema),
          _skip(true),
          _base(false)
    {
        BOOST_STATIC_ASSERT((std::is_base_of<U, T>::value || std::is_base_of<T, U>::value));
    }


    /// @brief Explicit initialization from an instance of U which is convertible to Reader.
    ///
    /// When `Reader` is an instance of `ProtocolReader` template, value can be one of:
    /// - reference to an instance of object that is convertible to `T`;
    ///   will store a copy of the object
    /// - `boost::shared_ptr<U>` to an object convertible to `T`;
    ///   will store an up-casted `shared_ptr`
    /// - `boost::reference_wrapper<U>` to an object convertible to `T`;
    ///   will store an up-casted raw pointer to the object
    template <typename U>
    explicit
    bonded(const U& value)
        : _data(value),
          _skip(true),
          _base(false)
    {}


    /// @brief Initialize from serialized data
    explicit
    bonded(Reader data, bool base = false)
        : _data(data),
          _skip(true),
          _base(base)
    {}


    /// @brief Explicit cast from `bonded<void>`
    template <typename ReaderT>
    explicit
    bonded(const bonded<void, ReaderT>& bonded)
        : _data(bonded._data),
          _schema(bonded._schema),
          _skip(true),
          _base(false)
    {}


    ~bonded()
    {
        // Skip the struct if it wasn't deserialized
        if (_skip)
            detail::Skip(_data, *this, std::nothrow);
    }


    /// @brief Implicit up-casting to bonded of a base type
    template <typename U, typename ReaderT>
    operator bonded<U, ReaderT>() const
    {
        BOOST_STATIC_ASSERT((std::is_base_of<U, T>::value));
        return bonded<U, ReaderT>(*this);
    }

    /// @brief Implicit conversion to `bonded<void>`
    template <typename ReaderT>
    operator bonded<void, ReaderT>() const
    {
        return bonded<void, ReaderT>(*this);
    }

    /// @brief Serialize bonded using specified protocol writer
    template <typename Protocols = BuiltInProtocols, typename Writer>
    void Serialize(Writer& output) const
    {
        Apply<Protocols>(SerializeTo<Protocols>(output), *this);
    }

    /// @brief Deserialize an object of type X
    template <typename X = T, typename Protocols = BuiltInProtocols>
    X Deserialize() const
    {
        X tmp;
        Apply<Protocols>(To<X, Protocols>(tmp), *this);
        return tmp;
    }

    /// @brief Deserialize to an object of type X
    template <typename Protocols = BuiltInProtocols, typename X>
    void Deserialize(X& var) const
    {
        Apply<Protocols>(To<X, Protocols>(var), *this);
    }

    /// @brief Deserialize to a bonded<U>
    template <typename Protocols = BuiltInProtocols, typename U>
    typename boost::enable_if<is_marshaled_bonded<T, Reader, U> >::type
    Deserialize(bonded<U>& var) const
    {
        _SelectProtocolAndApply<Protocols>(boost::ref(var));
    }


    template <typename Protocols = BuiltInProtocols, typename U>
    typename boost::disable_if<is_marshaled_bonded<T, Reader, U> >::type
    Deserialize(bonded<U>& var) const
    {
        var._data = _data;
    }


    /// @brief Update bonded<T> payload by merging it with an object of type X
    template <typename Protocols = BuiltInProtocols, typename X>
    void Merge(const X& var)
    {
        detail::Merge<Protocols>(var, _data);
    }


    /// @brief Skip struct data in the underlying payload
    void Skip()
    {
        _skip = false;
        detail::Skip(_data, *this);
    }


    /// @brief Compare for equality
    ///
    /// Returns true if both `bonded` point to the same instance of `T` or the same input stream.
    /// It does not compare values of objects `T` or contents of input streams.
    bool operator==(const bonded& rhs) const
    {
        return _data == rhs._data;
    }


    template <typename Protocols, typename Transform, typename U, typename ReaderT>
    friend typename boost::disable_if<detail::need_double_pass<Transform>, bool>::type inline
    detail::ApplyTransform(const Transform& transform, const bonded<U, ReaderT>& bonded);

    template <typename Protocols, typename Transform, typename U, typename ReaderT>
    friend typename boost::enable_if<detail::need_double_pass<Transform>, bool>::type inline
    detail::ApplyTransform(const Transform& transform, const bonded<U, ReaderT>& bonded);

    template <typename U, typename ReaderT>
    friend class bonded;

private:
    // Apply transform to serialized data
    template <typename Protocols, typename Transform>
    typename boost::enable_if<is_marshaled_bonded<T, Reader, Transform>, bool>::type
    _Apply(const Transform& transform) const
    {
        return _SelectProtocolAndApply<Protocols>(transform);
    }


    template <typename Protocols, typename Transform>
    typename boost::disable_if<is_marshaled_bonded<T, Reader, Transform>, bool>::type
    _Apply(const Transform& transform) const
    {
        _skip = false;
        return detail::Parse<T, Protocols>(transform, _data, typename schema_for_passthrough<T>::type(), _schema.get(), _base);
    }


    template <typename Protocols, typename Transform>
    bool _SelectProtocolAndApply(const Transform& transform) const
    {
        _skip = false;
        auto input = CreateInputBuffer(_data.GetBuffer(), detail::ReadBlob(_data));
        return SelectProtocolAndApply<typename remove_bonded<T>::type, Protocols>(input, transform).second;
    }

    Reader _data;
    RuntimeSchema _schema;
    mutable bool _skip;
    bool _base;
};

} // namespace bond
