// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.

#pragma once

#include <bond/core/config.h>

#include "packet.h"

#include <bond/comm/address.h>

namespace bond { namespace comm
{
namespace detail
{
//
// Minimal required implementation for null packet transport
//
template <typename WireProtocol>
class NullTransport
    : public PacketTransport<SocketAddress, WireProtocol>
    , private boost::noncopyable
{
    //
    // PacketTransport interfaces.
    //
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkConnectionSink INetworkConnectionSink;
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkServerSink INetworkServerSink;
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkConnection INetworkConnection;
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkServer INetworkServer;


    //
    // Server entity.
    //
    class NetworkServer
        : public INetworkServer
        , private boost::noncopyable
    {
    public:

        NetworkServer(const boost::shared_ptr<INetworkServerSink>& handler)
            : m_handler(handler)
        {}

    private:

        boost::shared_ptr<INetworkServerSink> m_handler;
    };

    //
    // Connection entity.
    //
    class NetworkConnection
        : public INetworkConnection
        , private boost::noncopyable
    {
    public:

        NetworkConnection(const boost::weak_ptr<INetworkServerSink>& handler,
                          const SocketAddress& address)
            : m_handler(handler)
            , m_address(address)
        {}

        NetworkConnection(const boost::weak_ptr<INetworkConnectionSink>& dstHandler,
                          const SocketAddress& address)
            : m_handler(m_empty)
            , m_dstHandler(dstHandler)
            , m_address(address)
        {}

        ~NetworkConnection()
        {
            if (boost::shared_ptr<INetworkConnectionSink> dstHandler = m_dstHandler.lock())
            {
                dstHandler->ConnectionClosed();
            }

            m_srcHandler->ConnectionClosed();

            BOND_LOG(LOG_DEBUG,
                     "NetworkConnection",
                     "Connection to " << m_address << " to be destroyed");
        }

        void SetSrcHandler(const boost::shared_ptr<INetworkConnectionSink>& srcHandler)
        {
            m_srcHandler = srcHandler;
        }

    // Implement INetworkConnection.

        void Send(uint64_t conversation_id,
                  Packet packet) override
        {
            boost::shared_ptr<INetworkConnectionSink> dstHandler = m_dstHandler.lock();
            if (!dstHandler)
            {
                //
                // Try to reconnect
                //

                //
                // Check if server exists to accept new connection.
                //
                if (boost::shared_ptr<INetworkServerSink> handler = m_handler.lock())
                {
                    //
                    // Create dummy remote client endpoint address.
                    //
                    SocketAddress srcAddress(m_address.GetPort());

                    //
                    // Create a new server side connection.
                    //
                    boost::shared_ptr<NetworkConnection> connection(new NetworkConnection(m_srcHandler, srcAddress));

                    //
                    // Notify server about new connection.
                    //
                    dstHandler = handler->ConnectionAccepted(connection, srcAddress, srcAddress);

                    connection->SetSrcHandler(dstHandler);
                }

                m_dstHandler = dstHandler;
            }

            if (!!dstHandler)
            {
                // Emit notification of received packet to remote handler.
                dstHandler->PacketReceived(true, conversation_id, std::move(packet));

                // Notify local handler of successfully sent packet.
                m_srcHandler->PacketSent(true, conversation_id);
            }
            else
            {
                // Notify local handler of failed packet sent.
                m_srcHandler->PacketSent(false, conversation_id);
            }
        }

    private:

        boost::weak_ptr<INetworkServerSink> m_empty;

        const boost::weak_ptr<INetworkServerSink>& m_handler;

        boost::weak_ptr<INetworkConnectionSink> m_dstHandler;

        boost::shared_ptr<INetworkConnectionSink> m_srcHandler;

        SocketAddress m_address;
    };

    //
    // Implement PacketTransport.
    //
    boost::shared_ptr<INetworkConnection>
    StartClientConnection(const SocketAddress& address,
                          const boost::shared_ptr<INetworkConnectionSink>& srcHandler) override
    {
        //
        // Create loop back interface.
        //
        boost::shared_ptr<NetworkConnection> connection;

        //
        // For NullTransport there is no exact ip:port match, check for port match.
        //
        connection = boost::make_shared<NetworkConnection>(m_servers[SocketAddress(address.GetPort())],
                                                           address);

        connection->SetSrcHandler(srcHandler);

        //
        // Return handler of new client connection.
        //
        return connection;
    }


    boost::shared_ptr<INetworkServer>
    StartServer(const SocketAddress& address,
                const boost::shared_ptr<INetworkServerSink>& handler) override
    {
        boost::shared_ptr<NetworkServer> networkServer =
            boost::make_shared<NetworkServer>(handler);
        //
        // Add handler to the list of listeners.
        //
        m_servers[SocketAddress(address.GetPort())] = handler;

        //
        // Notify server event sink about successful start.
        //
        handler->ServerStarted();

        //
        // Return handler of new server.
        //
        return networkServer;
    }

public:

    NullTransport()
    {}

private:

    std::map<SocketAddress, boost::weak_ptr<INetworkServerSink> > m_servers;
};

}; // namespace transport

typedef detail::NullTransport<bond::comm::FastWireProtocol> NullTransport;

} } // namespace bond.comm
