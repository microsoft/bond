
#pragma once

#include <bond/comm/transport.h>
#include <bond/comm/packet_apply.h>
#include <bond/comm/packet_reflection.h>

#include <bond/protocol/compact_binary.h>
#include <bond/stream/output_buffer.h>

#include <bond/core/bond.h>
#include <bond/comm/detail/locks.h>
#include <bond/comm/detail/logging.h>
#include <bond/comm/detail/callbacks.h>

#include <boost/atomic.hpp>
#include <boost/foreach.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/weak_ptr.hpp>

#include <unordered_set>
#include <unordered_map>


namespace bond { namespace comm
{

//
// Generic transport core,
// to be used by binary TCP transports implementations.
//
template <typename Address,
          typename WireProtocol,
          typename ConnectionContext = typename boost::call_traits<Address>::param_type>
class PacketTransport
    : public Transport<Address, WireProtocol, ConnectionContext>
{
public:

    //
    // Interface of connection handler (either on client or server endpoint).
    //
    // Implementation is provided by network core.
    //
    struct INetworkConnectionSink
    {
        //
        // Method shall be invoked when connection is finally closed.
        //
        virtual
        void ConnectionClosed() = 0;

        //
        // Method shall be invoked when connection was temporary dropped,
        // and may or may not reconnect.
        //
        virtual
        void ConnectionDropped() = 0;

        //
        // Method shall be invoked when new packet arrives.
        //
        // @param success: packet was successfully received, and is not malformed;
        // @param conversation_id: unique packet id;
        // @param data: packet payload.
        //
        virtual
        void PacketReceived(bool success,
                            uint64_t conversation_id,
                            Packet packet) = 0;

        //
        // Method shall be invoked when packet was removed from outstanding queue.
        //
        // @param success: packet was successfully sent;
        // @param conversation_id: unique packet id;
        //
        virtual
        void PacketSent(bool success,
                        uint64_t conversation_id) = 0;

        virtual ~INetworkConnectionSink()
        {}
    };

    //
    // Interface of network connection to be implemented by custom service.
    //
    struct INetworkConnection
    {
        //
        // Schedule a packet into outstanding queue:
        //
        // @param conversation_id: unique packet id;
        // @param buffers: packet payload;
        //
        virtual
        void Send(uint64_t conversation_id,
                  Packet packet) = 0;

        //
        // Custom network connection dtor shall release all resources
        // and gracefully close any network connections associated.
        //
        // Assigned INetworkConnectionSink::ConnectionClosed() shall be invoked
        // once connection shutdown is complete.
        //
        virtual ~INetworkConnection()
        {}
    };

    //
    // Interface implemented by server event handler.
    //
    // Implementation is provided by network core.
    //
    struct INetworkServerSink
    {
        //
        // Method shall be invoked when server is successfully bound and ready to accept client connections.
        //
        virtual
        void ServerStarted() = 0;

        //
        // Method shall be invoked when a new connection was accepted by server,
        //
        // @param connection: accepted connection;
        // @param context: raw transport connection context;
        //
        // @return: handler of connection events.
        //
        virtual
        boost::shared_ptr<INetworkConnectionSink> ConnectionAccepted(
            const boost::shared_ptr<INetworkConnection>& connection,
            const Address& address,
            ConnectionContext context) = 0;

        virtual ~INetworkServerSink()
        {}
    };

    //
    // Interface of network server to be implemented by custom service.
    //
    struct INetworkServer
    {
        //
        // Custom network server dtor should release all resources
        // and gracefully close any network connection associated.
        //
        virtual ~INetworkServer()
        {}
    };

protected:

    //
    // Interface of custom network service to be implemented.
    //

    //
    // Method shall create client connection against provided address.
    //
    // @param address: destination address;
    // @param handler: event handler of connection to be created;
    //
    // @return: created client connection.
    //
    virtual
    boost::shared_ptr<INetworkConnection> StartClientConnection(
        const Address& address,
        const boost::shared_ptr<INetworkConnectionSink>& handler) = 0;

    //
    // Method shall bind to a specific address.
    //
    // @param address: address to listen at;
    // @param handler: event handler of server to be created;
    //
    // @return: created server.
    //
    virtual
    boost::shared_ptr<INetworkServer> StartServer(
        const Address& address,
        const boost::shared_ptr<INetworkServerSink>& handler) = 0;

// Implement transport interface.

    boost::shared_ptr<IService> ConnectTo(const Address& address) override
    {
        boost::shared_ptr<ConnectionSink> handler =
            boost::make_shared<ConnectionSink>(address);

        boost::shared_ptr<INetworkConnection> connection =
            StartClientConnection(address, handler);

        handler->SetConnection(connection);

        return boost::make_shared<Connection>(handler,
                                              connection,
                                              address);
    }

    
    boost::shared_ptr<void> BindTo(
        const Address& address,
        const std::function<boost::shared_ptr<IService>(const Address&, ConnectionContext)>& sessionFactory) override
    {
        return boost::make_shared<Server>(*this,
                                          address,
                                          sessionFactory);
    }


    //
    // Default ctor.
    //
    PacketTransport() = default;


    explicit
    PacketTransport(const WireProtocol& wireProtocol)
        : Transport<Address, WireProtocol, ConnectionContext>(wireProtocol)
    {}

private:

    class ConnectionSink
        : public INetworkConnectionSink
    {
        //
        // Dispatch response.
        //
        static
        void DispatchResponse(const boost::weak_ptr<INetworkConnection>& ptr,
                              const Address& address,
                              uint64_t conversation_id,
                              Response& response)
        {
            if (boost::shared_ptr<INetworkConnection> connection = ptr.lock())
            {
                BOND_LOG(LOG_DEBUG,
                         "ConnectionSink",
                         "Send response (id: " << conversation_id << ") to " << address);

                Packet packet;
                packet.message_type = MessageType::RESPONSE;
                packet.message_data.is_error = response.is_error;
                if (response.is_error)
                {
                    // FIXME -- delegate to implementation here
                    OutputBuffer output;
                    CompactBinaryWriter<OutputBuffer> cbw(output);
                    Marshal(response.error, cbw);
                    packet.message_data.data.emplace_back(output.GetBuffer());
                }
                else
                {
                    packet.message_data.data = std::move(response.payload);
                }
                packet.layers = std::move(response.layers);

                connection->Send(conversation_id, std::move(packet));
            }
            else
            {
                BOND_LOG(LOG_INFO,
                         "ConnectionSink",
                         "Response (id: " << conversation_id << ") to " << address << " dropped, connection closed");
            }
        }


        void HandleResponse(uint64_t conversation_id,
                            Packet& packet)
        {
            BOND_LOG(LOG_DEBUG,
                     "ConnectionSink",
                     "Handle remote response (id: " << conversation_id << ") from " << m_address);

            // Find the listener.
            //
            ResponseCallback callback = CompleteConversation(conversation_id);

            if (!callback)
            {
                BOND_LOG(LOG_WARNING,
                         "ConnectionSink",
                         "Ignore " << ToString(packet.message_type) << " packet (id: " << conversation_id << ") from " << m_address);

                return;
            }

            Response response;
            response.is_error = packet.message_data.is_error;
            if (packet.message_data.is_error)
            {
                InputBuffer buffer(merge(packet.message_data.data.begin(), packet.message_data.data.end()));
                // FIXME - handle Unmarshal errors
                try
                {
                    Unmarshal<Error, InputBuffer>(buffer, response.error);
                }
                catch (const bond::Exception&)
                {
                    response.error.error_code = ErrorCode::INTERNAL_SERVER_ERROR;
                    response.error.message = "Got an Error from the server and couldn't unmarshal it.";
                }
            }
            else
            {
                response.payload = std::move(packet.message_data.data);
            }
            response.layers = std::move(packet.layers);

            callback(response);
        }


        void HandleRequest(uint64_t conversation_id,
                           Packet& packet)
        {
            BOND_LOG(LOG_DEBUG,
                     "ConnectionSink",
                     "Receive request (id: " << conversation_id << ") from " << m_address);

            ResponseCallback callback =
                boost::bind(&ConnectionSink::DispatchResponse,
                            m_connection,
                            m_address,
                            conversation_id,
                            _1);

            if (packet.message_data.is_error)
            {
                BOND_LOG(LOG_ERROR,
                    "PacketTransport::ConnectionSink",
                    "Received error message in HandleRequest for incoming request (id: " << conversation_id << ") from " << m_address);

                Response response;
                response.is_error = true;
                response.error.error_code = ErrorCode::INVALID_INVOCATION;
                response.error.message = "Received request with an error message";

                callback(response);
            }
            else
            {
                Request request;
                request.service_name = std::move(packet.service_name);
                request.method_name = std::move(packet.method_name);
                request.payload = std::move(packet.message_data.data);
                request.layers = std::move(packet.layers);

                try
                {
                    callback = guaranteed_callback(callback);

                    m_service->Invoke(request, callback);
                }
                catch (const CommException& ex)
                {
                    BOND_LOG(LOG_ERROR,
                        "ConnectionSink",
                        "Internal Bond RPC error for incoming request (id: " << conversation_id << ") from " << m_address << ": " << ex.what());

                    //
                    // If de-serialization/invocation failed, respond with exception.
                    //
                    Response response;
                    response.is_error = true;
                    response.error = ex;

                    callback(response);
                }
                catch (...)
                {
                    BOND_LOG(LOG_ERROR,
                        "ConnectionSink",
                        "Unexpected Bond RPC error for incoming request (id: " << conversation_id << ") from " << m_address);

                    //
                    // If other errors failed, respond with exception.
                    //
                    Response response;
                    response.is_error = true;
                    response.error.error_code = ErrorCode::INTERNAL_SERVER_ERROR;

                    callback(response);
                }
            }
        }


        void HandleEvent(uint64_t conversation_id, Packet& packet)
        {
            if (packet.message_data.is_error)
            {
                BOND_LOG(LOG_ERROR,
                    "PacketTransport::ConnectionSink",
                    "Received error message in HandleEvent for incoming event (id: " << conversation_id << ") from " << m_address);
                return;
            }

            BOND_LOG(LOG_DEBUG,
                     "ConnectionSink",
                     "Receive event (id:" << conversation_id << ") from " << m_address);

            Event event;
            event.service_name = std::move(packet.service_name);
            event.method_name = std::move(packet.method_name);
            event.payload = std::move(packet.message_data.data);
            event.layers = std::move(packet.layers);

            m_service->Notify(event);
        }

    protected:

    // Implement INetworkConnectionSink.

        //
        // In case of connection close, drop all outstanding requests.
        //
        void ConnectionClosed() override
        {
            BOND_LOG(LOG_DEBUG,
                     "ConnectionSink",
                     "Closing connection to " << m_address);

            std::unordered_map<uint64_t, ResponseCallback> listeners;

            {
                std::unordered_set<uint64_t> transmitted;

                //
                // Erase listeners and information of transmitted packets.
                //
                lock(m_listeners)->swap(listeners);
                lock(m_transmitted)->swap(transmitted);
            }

            BOOST_FOREACH(const auto& item, listeners)
            {
                const ResponseCallback& callback = item.second;

                Response response;
                response.is_error = true;
                response.error.error_code = ErrorCode::CONNECTION_SHUT_DOWN;
                response.error.message = "Request failed: connection closed";
                callback(response);
            }

            BOND_LOG(LOG_WARNING,
                     "ConnectionSink",
                     "Connection to " << m_address << " was closed, " <<
                     listeners.size() << " requests aborted");
        }


        //
        // In case of connection drop, drop all outstanding requests transmitted so far.
        //
        void ConnectionDropped() override
        {
            BOND_LOG(LOG_DEBUG,
                     "ConnectionSink",
                     "Dropping connection to " << m_address);

            std::vector<ResponseCallback> listeners;

            {
                std::unordered_set<uint64_t> transmitted;

                lock(m_transmitted)->swap(transmitted);
                listeners.reserve(transmitted.size());

                //
                // Collect transmitted listeners.
                //
                BOOST_FOREACH(uint64_t id, transmitted)
                {
                    listeners.push_back(CompleteConversation(id));
                }
            }

            BOOST_FOREACH(const ResponseCallback& callback, listeners)
            {
                if (callback)
                {
                    Response response;
                    response.is_error = true;
                    response.error.error_code = ErrorCode::CONNECTION_SHUT_DOWN;
                    response.error.message = "Request failed: connection dropped";
                    callback(response);
                }
            }

            BOND_LOG(LOG_WARNING,
                     "ConnectionSink",
                     "Connection to " << m_address << " is dropped. " <<
                     listeners.size() << " outstanding requests are aborted")
        }

        //
        // Process incoming binary packets.
        //
        void PacketReceived(bool success,
                            uint64_t conversation_id,
                            Packet packet) override
        {
            if (!success)
            {
                ResponseCallback callback = CompleteConversation(conversation_id);

                if (!callback)
                {
                    BOND_LOG(LOG_WARNING,
                             "ConnectionSink",
                             "Invalid packet (id: " << conversation_id << ") from " << m_address << " is ignored");
                }
                else
                {
                    //
                    // Destroy listener in case of bad packet, this may trigger exception.
                    // FIXME: No, we don't!
                    //
                    BOND_LOG(LOG_WARNING,
                             "ConnectionSink",
                             "Packet (id: " << conversation_id << ") from " << m_address << " is dropped");

                    Response response;
                    response.is_error = true;
                    response.error.error_code = ErrorCode::TRANSPORT_ERROR;
                    response.error.message = "Invalid response received";
                    callback(response);
                }

                return;
            }

            //
            // Execute appropriate handler against de-serialized packet.
            //
            switch (packet.message_type)
            {
                case MessageType::EVENT:
                {
                    HandleEvent(conversation_id, packet);
                    break;
                }

                case MessageType::REQUEST:
                {
                    HandleRequest(conversation_id, packet);
                    break;
                }

                case MessageType::RESPONSE:
                {
                    HandleResponse(conversation_id, packet);
                    break;
                }

                default:
                {
                    BOND_LOG(LOG_ERROR,
                             "ConnectionSink",
                             "Unexpected packet type (value: " << packet.message_type << ", id: " << conversation_id << ") from " << m_address);

                    //
                    // We don't recognize this payload type. Create an Error and return it to the local proxy or the
                    // remote client, as appropriate.
                    //
                    // FIXME: Should return a ProtocolError, not an Error, to remote sources of this problem. Should
                    // maybe throw for local sources.
                    //
                    Response response;
                    response.is_error = true;
                    response.error.error_code = ErrorCode::TRANSPORT_ERROR;
                    response.error.message = 
                        (bond::detail::string_stream() << "Unexpected message type: " << packet.message_type).str();

                    if (ResponseCallback callback = CompleteConversation(conversation_id))
                    {
                        callback(response);
                    }
                    else
                    {
                        DispatchResponse(m_connection,
                                         m_address,
                                         conversation_id,
                                         response);
                    }
                    break;
                }
            }
        }

        //
        // Mark all requests transmitted so far.
        //
        void PacketSent(bool success,
                        uint64_t conversation_id) override
        {
            if (success)
            {
                BOND_LOG(LOG_DEBUG,
                         "ConnectionSink",
                         "Packet (id: " << conversation_id << ") to " << m_address << " was sent.");

                //
                // Add successfully transmitted packets to the transmitted set.
                //
                auto l = lock(m_listeners);
                std::unordered_map<uint64_t, ResponseCallback>& map = *l;

                if (map.end() != map.find(conversation_id))
                {
                    lock(m_transmitted)->insert(conversation_id);
                }

                return;
            }

            BOND_LOG(LOG_DEBUG,
                     "ConnectionSink",
                     "Packet (id: " << conversation_id << ") to " << m_address << " failed.");

            //
            // Drop requests for the packets that were not sent.
            //
            ResponseCallback callback = CompleteConversation(conversation_id);
            if (!callback)
            {
                //
                // Response could be already delivered.
                //
                BOND_LOG(LOG_WARNING,
                         "ConnectionSink",
                         "Ignore unknown packet (id: " << conversation_id << ") confirmation " << m_address);
            }
            else
            {
                BOND_LOG(LOG_WARNING,
                         "ConnectionSink",
                         "Packet (id: " << conversation_id << ") to " << m_address << " is dropped.");

                Response response;
                response.is_error = true;
                response.error.error_code = ErrorCode::TRANSPORT_ERROR;
                response.error.message = "Request failed to send";
                callback(response);
            }
        }

        //
        // Set of transmitted conversations.
        //
        lockable<std::unordered_set<uint64_t> > m_transmitted;  // set of transmitted conversations

        //
        // Map of conversation listeners.
        //
        lockable<std::unordered_map<uint64_t, ResponseCallback> > m_listeners;

        //
        // Conversation enum.
        //
        boost::atomic_uint64_t m_conversationEnum;

        //
        // Remote endpoint address.
        //
        Address m_address;

        //
        // Network connection associated with this event handler.
        //
        boost::weak_ptr<INetworkConnection> m_connection;

        //
        // Default service instance.
        //
        boost::shared_ptr<IService> m_service;

    public:

        ConnectionSink(const Address& address)
            : m_conversationEnum(0)
            , m_address(address)
        {}


        uint64_t SkipConversation()
        {
            return ++m_conversationEnum;
        }


        uint64_t StartConversation(const ResponseCallback& callback)
        {
            uint64_t conversation_id = SkipConversation();

            lock(m_listeners)->insert(std::make_pair(conversation_id, callback));

            return conversation_id;
        }


        ResponseCallback CompleteConversation(uint64_t conversation_id)
        {
            ResponseCallback callback;

            {
                auto l = lock(m_listeners);
                std::unordered_map<uint64_t, ResponseCallback>& map = *l;
                auto it = map.find(conversation_id);

                if (map.end() != it)
                {
                    callback = it->second;

                    map.erase(it);
                }
                lock(m_transmitted)->erase(conversation_id);
            }

            return callback;
        }


        void SetConnection(const boost::weak_ptr<INetworkConnection>& connection)
        {
            m_connection = connection;
        }
    };


    class Connection
        : public IService,
          private boost::noncopyable
    {
        void Send(Packet&& packet)
        {
            //
            // Initiate unique conversation for an event with no listener.
            //
            const uint64_t conversation_id = m_handler->SkipConversation();

            //
            // And send it.
            //
            m_connection->Send(conversation_id, std::move(packet));
        }


        void Call(Packet&& packet,
                  const ResponseCallback& responseCallback)
        {
            //
            // Initiate listener for this request.
            //
            const uint64_t conversation_id = m_handler->StartConversation(
                guaranteed_callback(responseCallback));

            m_connection->Send(conversation_id, std::move(packet));
        }

    public: // Connection interface.

        void Invoke(Request& request,
                    const ResponseCallback& callback) override
        {
            Packet packet;
            packet.service_name = std::move(request.service_name);
            packet.method_name = std::move(request.method_name);
            packet.message_type = MessageType::REQUEST;
            packet.message_data.is_error = false;
            packet.message_data.data = std::move(request.payload);
            packet.layers = std::move(request.layers);

            Call(std::move(packet), callback);
        }


        void Notify(Event& event) override
        {
            Packet packet;
            packet.service_name = std::move(event.service_name);
            packet.method_name = std::move(event.method_name);
            packet.message_type = MessageType::EVENT;
            packet.message_data.is_error = false;
            packet.message_data.data = std::move(event.payload);
            packet.layers = std::move(event.layers);

            Send(std::move(packet));
        }


        Connection(const boost::shared_ptr<ConnectionSink>& handler,
                   const boost::shared_ptr<INetworkConnection>& connection,
                   const uint32_t& instance,
                   const Address& address)
            : m_handler(handler),
              m_connection(connection),
              m_address(address),
              m_instance(instance)
        {}


        Connection(const boost::shared_ptr<ConnectionSink>& handler,
                   const boost::shared_ptr<INetworkConnection>& connection,
                   const Address& address)
            : m_handler(handler),
              m_connection(connection),
              m_address(address)
        {}


        ~Connection()
        {
            m_connection.reset();
        }

    private:

        boost::shared_ptr<ConnectionSink> m_handler;

        boost::shared_ptr<INetworkConnection> m_connection;

        Address m_address;

        ::bond::maybe<uint32_t> m_instance;
    };


    typedef lockable<std::set<boost::shared_ptr<INetworkConnection> > > Connections;

    class ServerConnectionSink
        : public ConnectionSink
    {
        void ConnectionClosed() override
        {
            ConnectionSink::ConnectionClosed();

            if (boost::shared_ptr<INetworkConnection> connection = this->m_connection.lock())
            {
                lock(*m_connections)->erase(connection);
            }
        }

        boost::shared_ptr<Connections> m_connections;

    public:

        ServerConnectionSink(const boost::weak_ptr<INetworkConnection>& connection,
                             const Address& address,
                             const boost::shared_ptr<IService>& service,
                             const boost::shared_ptr<Connections>& connections)
            : ConnectionSink(address),
              m_connections(connections)
        {
            this->SetConnection(connection);

            this->m_service = service;
        }
    };


    class Server : private boost::noncopyable
    {
        class ServerSink
            : public INetworkServerSink
        {
            void ServerStarted() override
            {} // nop

            boost::shared_ptr<INetworkConnectionSink> ConnectionAccepted(
                const boost::shared_ptr<INetworkConnection>& connection,
                const Address& address,
                ConnectionContext context) override
            {
                {
                    lock(*m_connections)->insert(connection);
                }

                const boost::shared_ptr<IService> service = m_sessionFactory(address, context);
                return boost::make_shared<ServerConnectionSink>(connection,
                                                                address,
                                                                service,
                                                                m_connections);
            }

            Address m_address;

            boost::shared_ptr<Connections> m_connections;

            std::function<boost::shared_ptr<IService> (const Address&, ConnectionContext)> m_sessionFactory;

        public:
            ServerSink(const Address& address,
                       const std::function<boost::shared_ptr<IService> (const Address&, ConnectionContext)>& sessionFactory,
                       const boost::shared_ptr<Connections>& connections)
                : m_address(address)
                , m_connections(connections)
                , m_sessionFactory(sessionFactory)
            {}
        };

    public: // Server interface.

        Server(PacketTransport& transportCore,
               const Address& address,
               const std::function<boost::shared_ptr<IService> (const Address&, ConnectionContext)>& sessionFactory)
            : m_connections(boost::make_shared<Connections>())
        {
            boost::shared_ptr<ServerSink> serverHandler =
                boost::make_shared<ServerSink>(address, sessionFactory, m_connections);

            m_server = transportCore.StartServer(address, serverHandler);
        }


        ~Server()
        {
            m_server.reset();

            lock(*m_connections)->clear();
        }

    private:

        boost::shared_ptr<Connections> m_connections;

        boost::shared_ptr<INetworkServer> m_server;
    };
};

} } // namespace bond.comm
