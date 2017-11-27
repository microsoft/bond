// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/core/exception.h>
#include <bond/comm/comm_types.h>

namespace bond { namespace comm
{

class CommException
    : public Exception
    , public Error
{
public:

    CommException(ErrorCode errorCode, const char* message)
        : Exception(message)
    {
        Error::error_code = errorCode;
        Error::message = message;
    }
};


class MethodNotFoundException
    : public CommException
{
public:

    MethodNotFoundException(const char* message)
        : CommException(METHOD_NOT_FOUND, message)
    {}
};


class InvalidInvocationException
    : public CommException
{
public:

    InvalidInvocationException(const char* message)
        : CommException(INVALID_INVOCATION, message)
    {}
};


class TransportException
    : public CommException
{
public:

    TransportException(const char* message)
        : CommException(TRANSPORT_ERROR, message)
    {}
};


class ConnectionShutDownException
    : public CommException
{
public:

    ConnectionShutDownException(const char* message)
        : CommException(CONNECTION_SHUT_DOWN, message)
    {}
};


class InternalServerException
    : public CommException
{
public:

    InternalServerException(const char* message)
        : CommException(INTERNAL_SERVER_ERROR, message)
    {}
};

} } // namespace bond.comm
