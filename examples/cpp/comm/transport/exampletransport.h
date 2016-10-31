
// include preferred transport template
//
#include <bond/comm/address.h>
#include <bond/comm/transport/packet.h>


//
// Example transport implementation details
//
namespace examples
{

namespace transport
{
//
// Entity to represent single event
//
struct ExampleNetworkEvent
{
    enum EventType
    {
        // data transfer
        DataSent,
        DataRecieved,

        // connection state
        Close,

        // server state
        Start,
        Connect,
    };

    EventType m_eventType;
    uint64_t m_conversationId;
    uint16_t m_srcPort;
    uint16_t m_dstPort;
    bond::blob m_data;
};

typedef std::list<ExampleNetworkEvent> ExampleNetworkEventBus;

//
// Example network service implemented.
//
template <typename WireProtocol>
struct ExampleTransport
    : public bond::comm::PacketTransport<bond::comm::SocketAddress, WireProtocol>
    , private boost::noncopyable
{
    //
    // PacketTransport interfaces.
    //
    typedef typename bond::comm::PacketTransport<bond::comm::SocketAddress, WireProtocol>::INetworkConnectionSink INetworkConnectionSink;
    typedef typename bond::comm::PacketTransport<bond::comm::SocketAddress, WireProtocol>::INetworkServerSink INetworkServerSink;
    typedef typename bond::comm::PacketTransport<bond::comm::SocketAddress, WireProtocol>::INetworkConnection INetworkConnection;
    typedef typename bond::comm::PacketTransport<bond::comm::SocketAddress, WireProtocol>::INetworkServer INetworkServer;

    //
    // Network connection implemented by simple service.
    //
    class ExampleNetworkConnection
        : public INetworkConnection,
          private boost::noncopyable
    {
    public:
        ExampleNetworkConnection(uint16_t srcPort,
                                 uint16_t dstPort,
                                 const boost::shared_ptr<ExampleNetworkEventBus>& events,
                                 const boost::shared_ptr<bond::comm::critical_section>& baseCS)
            : m_srcPort(srcPort)
            , m_dstPort(dstPort)
            , m_events(events)
            , m_baseCS(baseCS)
        {
        }

        //
        // Schedule a packet into outstanding queue:
        //
        // @param transactionId: unique packet id;
        // @param buffers: packet payload;
        //
        void Send(uint64_t transactionId,
                  bond::comm::Packet packet) override
        {
            std::vector<bond::blob> buffers;

            bond::OutputBuffer stream;
            bond::FastBinaryWriter<bond::OutputBuffer> writer(stream);
            bond::Marshal(packet, writer);
            stream.GetBuffers(buffers);

            boost::lock_guard<bond::comm::critical_section> acs(*m_baseCS);

            ExampleNetworkEvent inEvent {
                ExampleNetworkEvent::DataSent,
                transactionId,
                m_dstPort,
                m_srcPort,
                bond::blob()
            };
            m_events->push_back(inEvent);

            ExampleNetworkEvent outEvent {
                ExampleNetworkEvent::DataRecieved,
                transactionId,
                m_srcPort,
                m_dstPort,
                bond::merge(buffers.begin(), buffers.end())
            };

            m_events->push_back(outEvent);
        }

        //
        // dtor releases all resources
        // and gracefully close any network connections associated.
        //
        // Assigned INetworkConnectionSink::ConnectionClosed() shall be invoked
        // once connection shutdown is complete.
        //
        ~ExampleNetworkConnection()
        {
            boost::lock_guard<bond::comm::critical_section> acs(*m_baseCS);

            ExampleNetworkEvent inEvent {
                ExampleNetworkEvent::Close,
                0,
                m_dstPort,
                m_srcPort,
                bond::blob()
            };
            m_events->push_back(inEvent);

            ExampleNetworkEvent outEvent {
                ExampleNetworkEvent::Close,
                0,
                m_srcPort,
                m_dstPort,
                bond::blob()
            };
            m_events->push_back(outEvent);
        }

    private:
        uint16_t m_srcPort;
        uint16_t m_dstPort;
        boost::shared_ptr<ExampleNetworkEventBus> m_events;
        boost::shared_ptr<bond::comm::critical_section> m_baseCS;
    };


    //
    // Network server implemented by simple service.
    //
    class ExampleNetworkServer
        : public INetworkServer,
          private boost::noncopyable
    {
    public:

        ExampleNetworkServer(std::map<uint16_t, boost::shared_ptr<INetworkServerSink> >& servers,
                            boost::shared_ptr<bond::comm::critical_section> baseCS,
                            uint16_t port)
            : m_port(port)
            , m_servers(servers)
            , m_baseCS(baseCS)
        {}

        //
        // dtor releases all resources
        // and gracefully close any network connection associated.
        //
        ~ExampleNetworkServer()
        {
            boost::lock_guard<bond::comm::critical_section> acs(*m_baseCS);

            m_servers.erase(m_port);
        }

    private:

        uint16_t m_port;
        std::map<uint16_t, boost::shared_ptr<INetworkServerSink> >& m_servers;
        boost::shared_ptr<bond::comm::critical_section> m_baseCS;
    };

public:
    //
    // Method creates client connection against provided address.
    //
    // @param address: destination address;
    // @param handler: event handler of connection to be created;
    //
    // @return: created client connection.
    //
    boost::shared_ptr<INetworkConnection> StartClientConnection(const bond::comm::SocketAddress& address,
                                                                const boost::shared_ptr<INetworkConnectionSink>& handler) override
    {
        boost::lock_guard<bond::comm::critical_section> acs(*m_baseCS);

        ++ m_nextPort;
        while (m_servers.find(m_nextPort) != m_servers.end())
        {
            ++ m_nextPort;
        }

        uint16_t srcPort = m_nextPort;
        uint16_t dstPort = address.GetPort();

        m_connections[std::make_pair(dstPort, srcPort)] = handler;

        ExampleNetworkEvent outEvent {
            ExampleNetworkEvent::Connect,
            0,
            srcPort,
            dstPort,
            bond::blob()
        };
        m_events->push_back(outEvent);

        return boost::shared_ptr<INetworkConnection>(
            new ExampleNetworkConnection(srcPort,
                                        dstPort,
                                        m_events,
                                        m_baseCS));
    }

    //
    // Method binds provided server sink to a specific address.
    //
    // @param address: address to listen at;
    // @param handler: event sink of server to be created;
    //
    // @return: created server.
    //
    boost::shared_ptr<INetworkServer> StartServer(const bond::comm::SocketAddress& address,
                                                  const boost::shared_ptr<INetworkServerSink>& handler) override
    {
        boost::lock_guard<bond::comm::critical_section> acs(*m_baseCS);

        m_servers[address.GetPort()] = handler;

        ExampleNetworkEvent outEvent {
            ExampleNetworkEvent::Start,
            0,
            0,
            address.GetPort(),
            bond::blob()
        };
        m_events->push_back(outEvent);

        return boost::shared_ptr<INetworkServer>(
            new ExampleNetworkServer(m_servers,
                                    m_baseCS,
                                    address.GetPort()));
    }

    ExampleTransport()
        : m_nextPort(0),
          m_baseCS(new bond::comm::critical_section()),
          m_events(new ExampleNetworkEventBus())
    {}

    //
    // Custom event bus management
    //
    bool PumpEvent()
    {
        boost::lock_guard<bond::comm::critical_section> acs(*m_baseCS);

        if (m_events->empty())
        {
            return false;
        }

        const ExampleNetworkEvent& e = m_events->front();
        std::pair<uint16_t, uint16_t> pp = std::make_pair(e.m_srcPort, e.m_dstPort);

        switch (e.m_eventType)
        {
        case ExampleNetworkEvent::DataSent:
            {
                m_connections.find(pp)->second->PacketSent(true, e.m_conversationId);
            }
            break;

        case ExampleNetworkEvent::DataRecieved:
            {
                if (m_connections.end() == m_connections.find(pp))
                {
                    //
                    // reschedule event, since server hasn't been subscribed yet
                    //
                    m_events->push_back(e);
                    break;
                }

                bond::comm::Packet packet;
                bond::Unmarshal(bond::InputBuffer(e.m_data), packet);

                m_connections.find(pp)->second->PacketReceived(true, e.m_conversationId, packet);
            }
            break;

        case ExampleNetworkEvent::Close:
            {
                if (m_connections.end() != m_connections.find(pp))
                {
                    m_connections.find(pp)->second->ConnectionClosed();
                    break;
                }

                m_connections.erase(pp);
            }
            break;

        case ExampleNetworkEvent::Connect:
            {
                if (NULL == m_servers[e.m_dstPort])
                {
                    //
                    // reschedule event, since server hasn't been subscribed yet
                    //
                    m_events->push_back(e);
                    break;
                }

                bond::comm::SocketAddress address(e.m_srcPort);

                boost::shared_ptr<INetworkConnection> connection(
                    new ExampleNetworkConnection(e.m_dstPort,
                                                e.m_srcPort,
                                                m_events,
                                                m_baseCS));

                m_connections[pp] = m_servers[e.m_dstPort]->ConnectionAccepted(connection, address, address);
            }
            break;

        case ExampleNetworkEvent::Start:
            {
                m_servers[e.m_dstPort]->ServerStarted();
            }
            break;

        default: assert(0);
        }

        m_events->pop_front();
        return true;
    }

private:

    uint16_t m_nextPort;
    boost::shared_ptr<bond::comm::critical_section> m_baseCS;
    boost::shared_ptr<ExampleNetworkEventBus> m_events;
    std::map<uint16_t, boost::shared_ptr<INetworkServerSink> > m_servers;
    std::map<std::pair<uint16_t, uint16_t>,
             boost::shared_ptr<INetworkConnectionSink> > m_connections;
};

}; // namespace transport

typedef transport::ExampleTransport<bond::comm::FastWireProtocol> ExampleTransport;

}; // namespace examples


