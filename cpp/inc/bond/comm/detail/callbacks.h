// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include <bond/comm/services.h>

//
// List of helper wrappers and typedefs.
//

namespace bond { namespace comm
{

//
// Wrapper that guarantees packet callback will be invoked only once.
//
class SingleInvoke : private boost::noncopyable
{
public:
    SingleInvoke(const ResponseCallback& callback)
        : m_callback(callback)
    {}

    void Invoke(Response& response)
    {
        if (!m_callback)
        {
            // This is an expected exception to indicate that user passed response twice.
            BOND_THROW(InvalidInvocationException, "Callback invoked more than once.");
        }

        m_callback(response);

        m_callback = nullptr;
    }

private:

    ResponseCallback m_callback;
};

//
// Helper function.
//
inline
ResponseCallback single_invoke(const ResponseCallback& callback)
{
    return boost::bind(&SingleInvoke::Invoke,
                       boost::make_shared<SingleInvoke>(callback),
                       _1);
}

//
// Wrapper that guarantees packet callback will be invoked.
//
class GuaranteedCallback : private boost::noncopyable
{
public:
    explicit
    GuaranteedCallback(const ResponseCallback& callback)
        : m_callback(callback)
    {}

    void Invoke(Response& response)
    {
        m_callback(response);

        m_callback = nullptr;
    }

    ~GuaranteedCallback()
    {
        if (m_callback)
        {
            Response response;
            response.is_error = true;
            response.error.error_code = ErrorCode::INVALID_INVOCATION;
            response.error.message = "Callback was dropped";

            m_callback(response);
        }
    }

private:

    ResponseCallback m_callback;
};

inline
ResponseCallback guaranteed_callback(const ResponseCallback& callback)
{
    return boost::bind(&GuaranteedCallback::Invoke,
                       boost::make_shared<GuaranteedCallback>(callback),
                       _1);
}

} } // namespace bond.comm
