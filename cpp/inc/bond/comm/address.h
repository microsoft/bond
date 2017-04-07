#pragma once

#include "exception.h"

#include <boost/algorithm/string/find.hpp>
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/classification.hpp>
#include <boost/asio/ip/address.hpp>

#if defined (__APPLE__)
    // Work-around: 'OSMemoryBarrier' has been explicitly marked deprecated
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wdeprecated-declarations"
    #include <boost/asio/ip/tcp.hpp>
    #pragma GCC diagnostic pop
#elif defined (_MSC_VER)
    #pragma warning(push)
    #pragma warning(disable: 4242) // C4242: 'identifier' : conversion from 'type1' to 'type2', possible loss of data
    #include <boost/asio/ip/tcp.hpp>
    #pragma warning(pop)
#else
    #include <boost/asio/ip/tcp.hpp>
#endif

#include <boost/lexical_cast.hpp>


namespace bond { namespace comm
{
//
// Wrapper of endpoint socket address information.
//
class SocketAddress
    : public boost::asio::ip::tcp::endpoint
{
public:

    //
    // Create socket address:
    // @param ip: char[] string with IP;
    // @param port: port number.
    //
    SocketAddress(const char* ip, uint16_t port)
        : boost::asio::ip::tcp::endpoint(boost::asio::ip::address::from_string(ip), port)
    {}


    //
    // Create socket address:
    // @param ip: 32bit packed ip address;
    // @param port: port number.
    // @remark: network byte ordering is used, i.e. 127.0.0.1 -> 0x0100007F
    SocketAddress(uint32_t ip, uint16_t port)
        : boost::asio::ip::tcp::endpoint(boost::asio::ip::address_v4(ip), port)
    {
    }
    //
    // Create socket address:
    // @param address: in_addr field;
    // @param port: port number.
    //
    SocketAddress(const boost::asio::ip::address& address, uint16_t port)
        : boost::asio::ip::tcp::endpoint(address, port)
    {}

    //
    // Create socket address:
    // @param address: in_addr field;
    // @param port: port number.
    //
    SocketAddress(const boost::asio::ip::tcp::endpoint& endpoint)
        : boost::asio::ip::tcp::endpoint(endpoint)
    {}


    //
    // Create socket address, with INADDR_ANY as an ip address and explicit port:
    // @param port: port number.
    //
    explicit
    SocketAddress(uint16_t port)
        : boost::asio::ip::tcp::endpoint(boost::asio::ip::address(), port)
    {}


    //
    // Default constructor
    //
    SocketAddress()
        : boost::asio::ip::tcp::endpoint(boost::asio::ip::address(), 0)
    {}


    boost::asio::ip::address GetAddress() const
    {
        return boost::asio::ip::tcp::endpoint::address();
    }


    uint16_t GetPort() const
    {
        return boost::asio::ip::tcp::endpoint::port();
    }
};


inline
::bond::detail::string_stream&
operator<<( ::bond::detail::string_stream& stream, const ::bond::comm::SocketAddress& address)
{
    return stream << address.GetAddress().to_string() << ":" << address.GetPort();
}


template <class CharT, class Traits>
inline
std::basic_ostream<CharT, Traits>&
operator<<(std::basic_ostream<CharT, Traits>& stream, const bond::comm::SocketAddress& address)
{
    return stream << address.GetAddress() << ":" << address.GetPort();
}

} } // namespace bond.comm
