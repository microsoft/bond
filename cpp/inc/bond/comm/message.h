// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "exception.h"

#include <bond/comm/comm_types.h>
#include <bond/core/bond.h>
#include <bond/core/traits.h>
#include <bond/stream/output_buffer.h>

#include <boost/mpl/if.hpp>
#include <boost/variant.hpp>

namespace bond { namespace comm
{
typedef void* message_context;

//
// Class template to represent request (or event) payload that is sent from client to server endpoint:
//
//  @typename T: bond generated structure that represents encapsulated data payload.
//
template <typename T>
class payload
{
public:
    typedef typename boost::mpl::if_<std::is_void<T>,
        Void,
        typename std::remove_reference<T>::type
    >::type value_type;

    template <typename TT = T>
    payload(const message_context& context = message_context(),
            typename std::enable_if <std::is_void<TT>::value>::type* = nullptr)
        : m_data(bonded<value_type>(value_type()))
        , m_context(context)
    {}


    // copy ctor
    payload(const payload& value)
        : m_data(value.m_data)
        , m_context(value.m_context)
    {}


    // move ctor
    payload(payload&& value)
        : m_data(std::move(value.m_data))
        , m_context(std::move(value.m_context))
    {}


    payload(const value_type& value,
            const message_context& context = message_context())
        : m_data(std::is_reference<T>::value
                    ? bonded<value_type>(boost::cref(value)) // if T is a reference, pass value by explicit ref
                    : bonded<value_type>(value)),
          m_context(context)
    {}


    payload(const boost::reference_wrapper<const value_type>& value,
            const message_context& context = message_context())
        : m_data(bonded<value_type>(value)),
          m_context(context)
    {}


    payload(const bonded<value_type>& value,
            const message_context& context = message_context())
        : m_data(value)
        , m_context(context)
    {}


    payload(bonded<value_type>&& value,
            message_context&& context = message_context())
        : m_data(std::move(value))
        , m_context(std::move(context))
    {}


    const bonded<value_type>& value() const
    {
        return m_data;
    }


#if !defined(_MSC_VER) || (_MSC_VER >= 1900)
    payload& operator=(payload&& value) = default;
    payload& operator=(const payload& value) = default;
#endif


    //
    // Access context to be passed (or the one that was passed) together with payload:
    //
    //  @returns: context.
    //
    const message_context& context() const
    {
        return m_context;
    }

private:

    bonded<value_type> m_data;

    message_context m_context;
};

//
// Enity that is used as indication of error response when passed to message<T> ctor
//
class error
{
public:
    error(int32_t error_code, std::string message)
    {
        m_error.error_code = error_code;
        m_error.message = std::move(message);
    }


    error(int32_t error_code, std::string message, bonded<Error> inner)
    {
        m_error.error_code = error_code;
        m_error.message = std::move(message);
        m_error.inner_error.set(std::move(inner));
    }


    explicit
    error(const Error& err)
        : m_error(err)
    {}


    explicit
    error(Error&& err)
        : m_error(std::move(err))
    {}


    int32_t error_code() const
    {
        return m_error.error_code;
    }


    const std::string& message() const
    {
        return m_error.message;
    }


    const nullable<bonded<Error>> inner_error() const
    {
        return m_error.inner_error;
    }

    operator const Error& () const
    {
        return m_error;
    }

private:

    Error m_error;
};


//
// Class template to represent response message that is sent from serve to client endpoint:
//
//  @typename T: bond generated structure that represents encapsulated data payload.
//
template <typename T>
class message
{
public:
    typedef typename boost::mpl::if_<std::is_void<T>,
        Void,
        typename std::remove_reference<T>::type
    >::type value_type;


    message(const message_context& context = message_context())
        : m_data(bonded<value_type>(value_type()))
        , m_context(context)
    {}


    // copy ctor
    message(const message& value)
        : m_data(value.m_data)
        , m_context(value.m_context)
    {}


    // move ctor
    message(message&& value)
        : m_data(std::move(value.m_data))
        , m_context(std::move(value.m_context))
    {}


    //
    // Construct message from given data and optional context:
    //
    //  @param value: payload data of bond generated type;
    //  @param context: context to pass together with the payload.
    //
    message(const value_type& value,
            const message_context& context = message_context())
        : m_data(std::is_reference<T>::value
            ? bonded<value_type>(boost::cref(value)) // if T is a reference, pass value by explicit ref
            : bonded<value_type>(value))
        , m_context(context)
    {}


    message(const boost::reference_wrapper<const value_type>& value,
            const message_context& context = message_context())
        : m_data(bonded<value_type>(value))
        , m_context(context)
    {}

    //
    // Construct message from bonded data and optional context:
    //
    //  @param value: bonded payload data;
    //  @param context: context to pass together with the payload.
    //
    message(const bonded<value_type>& value,
            const message_context& context = message_context())
        : m_data(value)
        , m_context(context)
    {}


    message(bonded<value_type>&& value,
            message_context&& context)
        : m_data(std::move(value))
        , m_context(std::move(context))
    {}

    //
    // Construct message from exception and optional context:
    //
    //  @param value: bonded exception;
    //  @param context: context to pass together with the payload;
    //  @remarks: unlike other ctors, this will construct message with encapsulated exception.
    //
    // TODO: check if "explicit" needed
    message(const error& err,
            const message_context& context = message_context())
        : m_data(err)
        , m_context(context)
    {}

    // TODO: check if "explicit" needed
    message(error&& err,
            message_context&& context = message_context())
        : m_data(std::move(err))
        , m_context(std::move(context))
    {}

    //
    // Flag to check if message contains regular data payload or exception:
    //
    //  @return: true if it is regular payload
    //
    bool is_error() const
    {
        return m_data.which() != 0;
    }

    //
    // Access regular data payload encapsulated inside this message.
    //
    //  @return: bonded data;
    //  @remarks: method will raise InvalidInvocationException if message contains an error.
    //
    const bonded<value_type>& value() const
    {
        if (const bonded<value_type>* payload = boost::get<bonded<value_type> >(&m_data))
        {
            return *payload;
        }

        // This is an expected exception to indicate that user is trying to access
        // payload while there is an embeded error.
        BOND_THROW(InvalidInvocationException, "Trying to read payload when error");

#if defined(_MSC_VER) && (_MSC_VER < 1900)
        // Fix warning C4715 in vc12: we should never reach this statement
        BOOST_ASSERT(false);
        return *boost::get<bonded<value_type> >(&m_data);
#endif
    }

    //
    // Access exception encapsulated inside this message.
    //
    //  @return: exception_ptr;
    //  @remarks: method will raise InvalidInvocationException if message
    //            encapsulates an regular data payload.
    //
    const error& err() const
    {
      if (const error* ex = boost::get<error>(&m_data))
        {
            return *ex;
        }

        // This is an expected exception to indicate that user is trying to access
        // payload while there is an embeded error.
        BOND_THROW(InvalidInvocationException, "Trying to access bonded exception of regular payload");
    }

#if !defined(_MSC_VER) || (_MSC_VER >= 1900)
    message& operator=(message&& value) = default;
    message& operator=(const message& value) = default;
#endif

    //
    // Access context to be passed (or the one that was passed) together with payload:
    //
    //  @returns: context.
    //
    const message_context& context() const
    {
        return m_context;
    }

private:

    boost::variant<bonded<value_type>, class error> m_data;

    message_context m_context;
};

} } // namespace bond.comm
