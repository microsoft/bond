#pragma once

#include <boost/asio.hpp>

#include <bond/comm/layers.h>
#include <bond/comm/address.h>
#include <bond/comm/epoxy_transport_apply.h>
#include <bond/comm/epoxy_transport_reflection.h>
#include <bond/comm/transport/detail/epoxy_data_structs.h>
#include <bond/comm/transport/detail/epoxy_protocol.h>
#include <bond/comm/transport/thread_service.h>
#include <bond/comm/transport/packet.h>

#include <boost/atomic/atomic.hpp>
#include <boost/scoped_ptr.hpp>
#include <boost/thread/thread.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <thread>

namespace bond { namespace comm
{
namespace epoxy { namespace detail
{
//
// boost::asio based over-TCP/IP transport.
//
template <typename WireProtocol,
          typename Allocator = std::allocator<char>>
class EpoxyTransport
    : public PacketTransport<SocketAddress, WireProtocol>
    , private boost::noncopyable
{
public:
    //
    // Construct transport:
    //
    //  @param numberOfThreads: # of working threads;
    //  @param allocator: allocator instance;
    //  @remark: if numberOfThreads is 0, then 1 working thread per cpu will start.
    //
    explicit
    EpoxyTransport(uint32_t numberOfThreads = uint32_t(),
                   const Allocator& allocator = Allocator())
        : m_allocator(allocator)
        , m_threadService(boost::make_shared<ThreadService>(numberOfThreads))
    {}

    //
    // Construct transport:
    //
    //  @param allocator: allocator instance;
    //  @remark: 1 working thread per cpu will start.
    //
    explicit
    EpoxyTransport(const Allocator& allocator)
        : m_allocator(allocator)
        , m_threadService(boost::make_shared<ThreadService>())
    {}

    //
    // Construct transport:
    //
    //  @param threadService: shared instance of ThreadService; must not be null
    //  @param allocator: allocator instance.
    //
    explicit
    EpoxyTransport(boost::shared_ptr<ThreadService> threadService,
                   const Allocator& allocator = Allocator())
        : m_allocator(allocator)
        , m_threadService(threadService)
    {
        BOOST_ASSERT(nullptr != m_threadService);
    }

    typedef typename PacketTransport<SocketAddress, WireProtocol>::Address Address;

private:

    //
    // typedef PacketTransport interfaces
    //
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkConnectionSink INetworkConnectionSink;
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkServerSink INetworkServerSink;
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkConnection INetworkConnection;
    typedef typename PacketTransport<SocketAddress, WireProtocol>::INetworkServer INetworkServer;

    //
    // Implement INetworkConnection.
    //
    class EpoxyConnection
        : public INetworkConnection
        , public boost::enable_shared_from_this<EpoxyConnection>
    {
        // Helper functor to fix vc12
        struct Transform
        {
            boost::asio::const_buffer operator()(const blob& data) const
            {
                return boost::asio::const_buffer(data.content(), data.size());
            }
        };

        //
        // This method is called by io_service once data was sent.
        //
        void OnSend(uint64_t id, const boost::system::error_code& ec)
        {
            BOOST_ASSERT(m_sending.load());

            if (!ec)
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "Data (id: " << id << ") transmitted to " << m_address);

                DoSend();
                m_handler->PacketSent(true, id);
            }
            else
            {
                //
                // TODO: clarify possible errors and proper handling:
                //
                // && boost::asio::error::not_connected != ec
                // && boost::asio::error::connection_reset != ec
                // && boost::asio::error::not_socket != ec)
                //
                BOOST_VERIFY(m_sending.exchange(false));

                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Data (id: " << id << ") to " << m_address << " failed: " << ec.message());

                m_handler->PacketSent(false, id);
            }
        }


        void Write(uint64_t id, const std::shared_ptr<const std::vector<blob> >& buffers)
        {
            boost::weak_ptr<EpoxyConnection> ptr = this->shared_from_this();
            async_write(m_socket,
                        boost::adaptors::transform(*buffers, Transform()),
                        [ptr, id, buffers](const boost::system::error_code& ec, std::size_t) {
                            if (auto this_ = ptr.lock())
                            {
                                this_->OnSend(id, ec);
                            }
                            else
                            {
                                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                                         "Write for (id: " << id << ") failed: connection destroyed");
                            }
                        });
        }


        //
        // This method is called to send next packet in the queue.
        //
        void DoSend()
        {
            do
            {
                BOOST_ASSERT(m_sending.load());

                uint64_t id = 0;
                std::shared_ptr<const std::vector<blob>> buffers;

                {
                  auto l = lock(m_queue);
                  if (!l->empty())
                  {
                    auto& front = l->front();

                    id = front.first;
                    buffers.swap(front.second);
                    l->pop_front();
                  }
                }

                if (buffers != nullptr)
                {
                    Write(id, buffers);
                    break;
                }

                BOOST_VERIFY(m_sending.exchange(false));
            }
            while (!lock(m_queue)->empty() && !m_sending.exchange(true));
        }


        bool HandleFrame(bool success, detail::ClassifyResult&& result)
        {
            uint64_t conversation_id = UINT64_MAX;
            if (nullptr != result.headers)
            {
                conversation_id = result.headers->conversation_id;
            }

            Packet packet;
            if (success)
            {
                if (detail::FrameDisposition::ProcessConfig == result.disposition)
                {
                    BOND_LOG(LOG_ERROR, "HandleFrame",
                             "HandleFrame with handshake");

                    if (m_handshake_received.exchange(true))
                    {
                        BOND_LOG(LOG_ERROR, "HandleFrame",
                                 "HandleFrame called with duplicate handshake!");

                        return false;
                    }

                    if (!m_isClient)
                    {
                        DoHandshake();
                    }

                    return true;
                }

                if (result.disposition == detail::FrameDisposition::Indeterminate)
                {
                    BOND_LOG(LOG_ERROR, "HandleFrame",
                             "HandleFrame called with success=true but with no result!");

                    return false;
                }

                if (nullptr == result.headers)
                {
                    BOND_LOG(LOG_ERROR, "HandleFrame",
                             "HandleFrame called with null headers!");

                    return false;
                }

                if (!m_handshake_received.load())
                {
                    BOND_LOG(LOG_ERROR, "HandleFrame",
                             "Handshake has not arrived!");

                    return false;
                }

                packet.method_name = std::move(result.headers->method_name);
                packet.service_name = std::move(result.headers->service_name);
                packet.layers = std::move(result.layer_data);
                packet.message_data = std::move(result.message_data);

                //
                // Execute appropriate handler against de-serialized packet.
                //
                switch (result.headers->message_type)
                {
                    case EpoxyMessageType::REQUEST:
                    {
                        packet.message_type = MessageType::REQUEST;
                        break;
                    }

                    case EpoxyMessageType::EVENT:
                    {
                        packet.message_type = MessageType::EVENT;
                        break;
                    }

                    case EpoxyMessageType::RESPONSE:
                    {
                        packet.message_type = MessageType::RESPONSE;
                        break;
                    }

                    default:
                    {
                        BOND_LOG(LOG_ERROR, "HandleFrame",
                                 "HandleFrame called with unexpected message type: " << result.headers->message_type);

                        return false;
                    }
                }
            }

            m_handler->PacketReceived(success, conversation_id, std::move(packet));
            return true;
        }

        //
        // This method is called by io_service once a framelet has been read.
        //
        void OnReadFramelet(const std::shared_ptr<detail::FrameState>& state,
                            const boost::system::error_code& ec)
        {
            BOOST_ASSERT(m_reading.load());

            if (ec)
            {
                //
                // TODO: clarify possible errors and proper handling:
                // && boost::asio::error::connection_reset == ec
                // && boost::asio::error::not_connected != ec
                // && boost::asio::error::misc_errors::eof != ec)
                //
                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Unexpected error from " << m_address << ": " << ec.message());

                detail::ClassifyResult empty;
                HandleFrame(false, std::move(empty));

                DoDisconnect();
            }
            else
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "Data from " << m_address << " received");

                if (state->count > state->framelets.size())
                {
                    DoReadFrameletTag(state);
                }
                else
                {
                    std::unique_ptr<ClassifyResult> result = Classify(*state);
                    if (HandleFrame(true, std::move(*result)))
                    {
                        DoReadFrame(state);
                    }
                    else
                    {
                        DoDisconnect();
                    }
                }
            }
        }

        //
        // This method is called to read one framelet.
        //
        void DoReadFramelet(const std::shared_ptr<detail::FrameState>& state)
        {
            BOOST_ASSERT(m_reading.load());

            const uint32_t length = state->tag.length;
            boost::shared_ptr<char[]> data = boost::allocate_shared_noinit<char[]>(m_allocator, length);
            state->framelets.emplace_back(static_cast<detail::FrameletType>(state->tag.type), blob(data, length));

            boost::weak_ptr<EpoxyConnection> ptr = this->shared_from_this();
            async_read(
                m_socket,
                boost::asio::buffer(data.get(), length),
                [ptr, state](const boost::system::error_code& ec, std::size_t s) {
                    BOOST_VERIFY(!!ec || s == state->tag.length);

                    if (auto this_ = ptr.lock())
                    {
                        this_->OnReadFramelet(state, ec);
                    }
                });
        }

        //
        // This method is called by io_service once packet header was read.
        //
        void OnReadFrameletTag(const std::shared_ptr<detail::FrameState>& state,
                               const boost::system::error_code& ec)
        {
            BOOST_ASSERT(m_reading.load());

            if (ec)
            {
                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Unexpected error from " << m_address << ": " << ec.message());

                DoDisconnect();
            }
            else
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "Header from " << m_address << " received");

                DoReadFramelet(state);
            }
        }

        //
        // This method is called to read incoming packet header.
        //
        void DoReadFrameletTag(const std::shared_ptr<detail::FrameState>& state)
        {
            BOOST_ASSERT(m_reading.load());

            boost::weak_ptr<EpoxyConnection> ptr = this->shared_from_this();
            async_read(
                m_socket,
                boost::asio::buffer(&state->tag, sizeof(state->tag)),
                [ptr, state](const boost::system::error_code& ec, std::size_t) {
                    if (auto this_ = ptr.lock())
                    {
                        this_->OnReadFrameletTag(state, ec);
                    }
                });
        }

        //
        // This method is called by io_service once the beginning of a frame has been read.
        //
        void OnReadFrame(const std::shared_ptr<detail::FrameState>& state,
                         const boost::system::error_code& ec)
        {
            BOOST_ASSERT(m_reading.load());

            if (ec)
            {
                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Unexpected error from " << m_address << ": " << ec.message());

                DoDisconnect();
            }
            else
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "Header from " << m_address << " received");

                //
                // Reset state->framelets.
                //
                std::vector<std::pair<FrameletType, blob>> framelets;
                framelets.reserve(state->count);

                std::swap(framelets, state->framelets);

                //
                // Schedule first frame read.
                //
                OnReadFramelet(state, ec);
            }
        }

        //
        // This method is called to read the beginning of a frame.
        //
        void DoReadFrame(const std::shared_ptr<detail::FrameState>& state)
        {
            BOOST_ASSERT(m_reading.load());

            boost::weak_ptr<EpoxyConnection> ptr = this->shared_from_this();
            async_read(
                m_socket,
                boost::asio::buffer(&state->count, sizeof(state->count)),
                [ptr, state](const boost::system::error_code& ec, std::size_t) {
                    if (auto this_ = ptr.lock())
                    {
                        this_->OnReadFrame(state, ec);
                    }
                });
        }

        //
        // This method is called by io_service once connection was established.
        //
        void OnConnect(const boost::system::error_code& ec)
        {
            BOOST_ASSERT(m_isClient);
            BOOST_ASSERT(m_sending.load());
            BOOST_ASSERT(m_reading.load());

            if (!ec)
            {
                //
                // Successfully connected
                //
                BOND_LOG(LOG_INFO, "EpoxyConnection",
                         "Connected to " << m_address);

                {
                    //
                    // Config tcp/ip socket options
                    //
                    boost::system::error_code so_ec;
                    if (m_socket.set_option(boost::asio::socket_base::keep_alive(true), so_ec))
                    {
                        BOND_LOG(LOG_ERROR, "EpoxyConnection",
                                 "Failed to set keep_alive for a socket to " << m_address << ": " << so_ec.message());
                    }

                    if (m_socket.set_option(boost::asio::ip::tcp::no_delay(true), so_ec))
                    {
                        BOND_LOG(LOG_ERROR, "EpoxyConnection",
                                 "Failed to set no_delay for a socket to " << m_address << ": " << so_ec.message());
                    }

                    if (m_socket.set_option(boost::asio::socket_base::linger(false, 0), so_ec))
                    {
                        BOND_LOG(LOG_ERROR, "EpoxyConnection",
                                 "Failed to set linger for a socket to " << m_address << ": " << so_ec.message());
                    }
                }

                //
                // Reset local handshake and start remote handshake
                //
                m_handshake_received.exchange(false);
                DoHandshake();

                //
                // Schedule async read.
                //
                auto state = std::allocate_shared<detail::FrameState>(m_allocator);
                DoReadFrame(state);
            }
            else
            {
                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Failed to connect to " << m_address << ": " << ec.message());

                BOOST_VERIFY(m_sending.exchange(false));

                DoDisconnect();
            }
        }

        void OnHandshake(const boost::system::error_code& ec)
        {
            BOOST_ASSERT(m_sending.load());

            if (!ec)
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "Handshake config transmitted to " << m_address);

                DoSend();
            }
            else
            {
                //
                // TODO: clarify possible errors and proper handling:
                //
                // && boost::asio::error::not_connected != ec
                // && boost::asio::error::connection_reset != ec
                // && boost::asio::error::not_socket != ec)
                //
                BOOST_VERIFY(m_sending.exchange(false));

                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Handshake to " << m_address << " failed: " << ec.message());
            }
        }


        //
        // Send an empty config.
        //
        void DoHandshake()
        {
            BOOST_ASSERT(m_sending.load());

            BOND_LOG(LOG_ERROR, "EpoxyConnection",
                     "Send handshake to " << m_address);

            auto buffers = MakeConfigFrame(m_allocator);

            boost::weak_ptr<EpoxyConnection> ptr = this->shared_from_this();
            async_write(m_socket,
                        boost::adaptors::transform(*buffers, Transform()),
                        [ptr, buffers](const boost::system::error_code& ec, std::size_t) {
                            if (auto this_ = ptr.lock())
                            {
                                this_->OnHandshake(ec);
                            }
                        });
        }


        std::shared_ptr<std::vector<blob>> Frame(uint64_t id, const Packet& packet)
        {
            const bool have_layer_data = !packet.layers.empty();

            OutputBuffer stream(256, static_cast<uint32_t>(packet.layers.size() + packet.message_data.data.size() + 4));
            stream.Write(static_cast<uint16_t>(have_layer_data ? 3 : 2));

            // Headers
            {
                epoxy::EpoxyHeaders headers;

                switch (packet.message_type)
                {
                    case MessageType::EVENT: headers.message_type = epoxy::EpoxyMessageType::EVENT; break;
                    case MessageType::REQUEST: headers.message_type = epoxy::EpoxyMessageType::REQUEST; break;
                    case MessageType::RESPONSE: headers.message_type = epoxy::EpoxyMessageType::RESPONSE; break;

                    default: BOND_THROW(TransportException, "Unexpected message_type value " << packet.message_type);
                }

                headers.conversation_id = id;
                headers.service_name = packet.service_name;
                headers.method_name = packet.method_name;

                OutputBuffer output(256, 4);
                FastBinaryWriter<OutputBuffer> writer(output);

                Serialize(headers, writer);

                std::vector<blob> data;
                output.GetBuffers(data);

                uint32_t length = 0;
                for (const blob& buffer : data)
                {
                    length += buffer.length();
                }

                stream.Write(static_cast<uint16_t>(detail::FrameletType::EPOXY_HEADERS));
                stream.Write(static_cast<uint32_t>(length));

                for (const blob& buffer : data)
                {
                    stream.Write(buffer);
                }
            }

            // Layers
            if (have_layer_data)
            {
                uint32_t length = 0;
                for (const blob& buffer : packet.layers)
                {
                    length += buffer.length();
                }

                stream.Write(static_cast<uint16_t>(detail::FrameletType::LAYER_DATA));
                stream.Write(static_cast<uint32_t>(length));

                for (const blob& buffer : packet.layers)
                {
                    stream.Write(buffer);
                }
            }

            // Message data
            {
                uint32_t length = 0;
                for (const blob& buffer : packet.message_data.data)
                {
                    length += buffer.length();
                }

                FrameletType frameletType = (packet.message_data.is_error
                                                ? detail::FrameletType::ERROR_DATA
                                                : detail::FrameletType::PAYLOAD_DATA);
                stream.Write(static_cast<uint16_t>(frameletType));
                stream.Write(static_cast<uint32_t>(length));

                for (const blob& buffer : packet.message_data.data)
                {
                    stream.Write(buffer);
                }
            }

            std::shared_ptr<std::vector<blob>> buffers =
                std::allocate_shared<std::vector<blob>>(m_allocator);

            stream.GetBuffers(*buffers);
            return buffers;
        }


        void DoConnect()
        {
            BOOST_ASSERT(m_isClient);
            BOOST_ASSERT(m_sending.load());
            BOOST_ASSERT(m_reading.load());
            BOOST_ASSERT(!m_socket.is_open());

            boost::weak_ptr<EpoxyConnection> ptr = this->shared_from_this();
            m_socket.async_connect(m_address,
                                   [ptr](const boost::system::error_code& ec) {
                                        if (auto this_ = ptr.lock())
                                        {
                                            this_->OnConnect(ec);
                                        }
                                   });
        }


        void DropQueue()
        {
            auto l = lock(m_queue);
            auto it = l->begin();
            while (it != l->end())
            {
                m_handler->PacketSent(false, it->first);
                it = l->erase(it);
            }
        }


        void DoDisconnect()
        {
            BOOST_ASSERT(m_reading.load());

            m_socket.close();

            BOOST_VERIFY(m_reading.exchange(false));

            m_handler->ConnectionDropped();

            DropQueue();
        }


        Allocator m_allocator;

        SocketAddress m_address;

        boost::shared_ptr<boost::asio::io_service> m_ioServicePtr;

        boost::shared_ptr<INetworkConnectionSink> m_handler;

        boost::asio::ip::tcp::socket m_socket;

        boost::atomic_bool m_sending;

        boost::atomic_bool m_reading;

        boost::atomic_bool m_handshake_received;

        const bool m_reconnect;

        const bool m_isClient;

        lockable<std::list<std::pair<uint64_t, std::shared_ptr<const std::vector<blob>>>>> m_queue;

    public:

        //
        // Construct client side connection.
        //
        EpoxyConnection(const Allocator& allocator,
                        const SocketAddress& address,
                        const boost::shared_ptr<INetworkConnectionSink>& handler,
                        const boost::shared_ptr<boost::asio::io_service>& ioServicePtr)
            : m_allocator(allocator)
            , m_address(address)
            , m_ioServicePtr(ioServicePtr)
            , m_handler(handler)
            , m_socket(*ioServicePtr)
            , m_sending(false)
            , m_reading(false)
            , m_handshake_received(false)
            , m_reconnect(true)
            , m_isClient(true)
        {}

        //
        // Construct server side connection.
        //
        EpoxyConnection(const Allocator& allocator,
                        const SocketAddress& address,
                        boost::asio::ip::tcp::socket&& socket,
                        const boost::shared_ptr<boost::asio::io_service>& ioServicePtr)
            : m_allocator(allocator)
            , m_address(address)
            , m_ioServicePtr(ioServicePtr)
            , m_socket(std::move(socket))
            , m_sending(false)
            , m_reading(false)
            , m_handshake_received(false)
            , m_reconnect(false)
            , m_isClient(false)
        {}


        ~EpoxyConnection()
        {
            BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                     "Connection" << (m_isClient ? " to " : " from ") << m_address << " to be destroyed");

            boost::system::error_code ec;
            if (m_socket.shutdown(boost::asio::ip::tcp::socket::shutdown_both, ec))
            {
                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Fail to shutdown tcp/ip socket: " << ec.message() << ", while connection" << (m_isClient ? " to " : " from ") << m_address << " is being destroyed");
            }

            if (m_socket.close(ec))
            {
                BOND_LOG(LOG_ERROR, "EpoxyConnection",
                         "Fail to close tcp/ip socket: " << ec.message() << ", while connection" << (m_isClient ? " to " : " from ") << m_address << " is being destroyed");
            }

            m_handler->ConnectionClosed();
        }


        //
        // Start read from incoming connection (this is invoked after connection is accepted)
        //
        void Listen(const boost::shared_ptr<INetworkConnectionSink>& sink)
        {
            BOOST_ASSERT(!m_isClient);
            BOOST_ASSERT(!m_sending.load());
            BOOST_ASSERT(!m_reading.load());
            BOOST_ASSERT(m_socket.is_open());

            m_handler = sink;

            //
            // Schedule async read.
            //
            auto state = std::allocate_shared<detail::FrameState>(m_allocator);

            BOOST_VERIFY(!m_reading.exchange(true));
            BOOST_VERIFY(!m_sending.exchange(true));

            DoReadFrame(state);
        }

        //
        // Trigger connect of client connection 
        //
        void Connect()
        {
            BOOST_ASSERT(m_isClient);
            BOOST_ASSERT(!m_sending.load());
            BOOST_ASSERT(!m_reading.load());
            BOOST_ASSERT(!m_handshake_received.load());
            BOOST_ASSERT(!m_socket.is_open());

            BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                     "Connect to " << m_address);

            BOOST_VERIFY(!m_sending.exchange(true));
            BOOST_VERIFY(!m_reading.exchange(true));

            DoConnect();
        }

    // Implement INetworkConnection

        void Send(uint64_t id, Packet packet) override
        {
            BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                     "Send packet (id: " << id << ") to " << m_address);

            std::shared_ptr<const std::vector<blob>> buffers = Frame(id, packet);

            if (m_sending.exchange(true))
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "There is an outstanding send, enqueue packet (id: " << id << ") to be send to " << m_address);

                //
                // There is existing send in-progress, enqueue this frame
                //
                auto l = lock(m_queue);
                l->emplace_back(0, std::move(buffers));

                //
                // Try to start send if previous send finished by this time
                //
                if (!m_sending.exchange(true))
                {
                    DoSend();
                }
            }
            else if (m_reading.exchange(true))
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "No outstanding sends, write packet (id: " << id << ") directly to " << m_address);

                //
                // There is no existing send in-progress, try to send within this thread
                //
                Write(id, buffers);
            }
            else if (m_reconnect)
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "Connection for packet (id: " << id << ") is not established to " << m_address << ", start reconnect");

                //
                // There is no existing connection, enqueue this frame and start connect
                //
                auto l = lock(m_queue);
                l->emplace_back(id, std::move(buffers));

                DoConnect();
            }
            else
            {
                BOND_LOG(LOG_DEBUG, "EpoxyConnection",
                         "Reconnect for packet (id: " << id << ") to " << m_address << " is disabled");

                BOOST_VERIFY(m_reading.exchange(false));
                BOOST_VERIFY(m_sending.exchange(false));

                //
                // Reconnect is disabled, notify upper layer that this packed is dropped
                //
                m_handler->PacketSent(false, id);

                //
                // Drop all enqued packets so far
                DropQueue();
            }
        }
    };

    //
    // Implement INetworkServer.
    //
    class EpoxyServer
        : public INetworkServer
    {
        //
        // Acceptor logic
        //
        class EpoxyServerContext : public boost::enable_shared_from_this<EpoxyServerContext>
        {
            void OnAccept()
            {
                const SocketAddress address(m_socket.remote_endpoint());

                BOND_LOG(LOG_INFO, "EpoxyServerContext",
                         "Accepted connection from: " << address);

                boost::shared_ptr<EpoxyConnection> connection =
                    boost::make_shared<EpoxyConnection>(m_allocator,
                                                        address,
                                                        std::move(m_socket),
                                                        m_ioService);

                boost::shared_ptr<INetworkConnectionSink> sink =
                    m_handler->ConnectionAccepted(connection,
                                                  address,
                                                  address);
                connection->Listen(sink);

                DoAccept();
            }


            Allocator m_allocator;

            boost::shared_ptr<boost::asio::io_service> m_ioService;

            boost::shared_ptr<INetworkServerSink> m_handler;

            boost::asio::ip::tcp::socket m_socket;

            boost::asio::ip::tcp::acceptor m_acceptor;

        public:

            EpoxyServerContext(const Allocator& allocator,
                               const SocketAddress& address,
                               const boost::shared_ptr<INetworkServerSink>& handler,
                               const boost::shared_ptr<boost::asio::io_service>& ioService)
                : m_allocator(allocator)
                , m_ioService(ioService)
                , m_handler(handler)
                , m_socket(*ioService)
                , m_acceptor(*ioService)
            {
                BOOST_ASSERT(nullptr != m_handler);
                BOOST_ASSERT(nullptr != m_ioService);

                BOND_LOG(LOG_DEBUG, "EpoxyServerContext",
                         "Bind server to " << address);

                boost::system::error_code ec;
                BOOST_VERIFY(!m_acceptor.open(address.protocol(), ec));
                BOOST_VERIFY(!m_acceptor.set_option(boost::asio::socket_base::reuse_address(true), ec));
                if (m_acceptor.bind(address, ec))
                {
                    BOND_LOG(LOG_ERROR, "EpoxyServerContext",
                             "Failed to bind to " << address << ": " << ec.message());

                    BOND_THROW(TransportException,
                               "Failed to bind to " << address << ": " << ec.message());
                }

                BOOST_VERIFY(!m_acceptor.listen(boost::asio::socket_base::max_connections, ec));
            }

            //
            // Schedule next incoming connection acceptance.
            //
            void DoAccept()
            {
                boost::weak_ptr<EpoxyServerContext> ptr = this->shared_from_this();
                m_acceptor.async_accept(
                    m_socket,
                    [ptr](const boost::system::error_code& ec) {
                        auto this_ = ptr.lock();
                        if (!ec && this_)
                        {
                            this_->OnAccept();
                        }
                        else if (boost::asio::error::operation_aborted == ec)
                        {
                            BOND_LOG(LOG_INFO, "EpoxyServerContext",
                                     "Accept aborted: " << ec.message());
                        }
                        else
                        {
                            BOND_LOG(LOG_ERROR, "EpoxyServerContext",
                                     "Failed to accept: " << ec.message());
                        }
                    });
            }

            void Shutdown()
            {
                boost::system::error_code ec;
                if (m_acceptor.close(ec))
                {
                    BOND_LOG(LOG_ERROR, "EpoxyServerContext",
                             "Failed to stop server acceptor: " << ec.message());
                }
            }
        };

        boost::shared_ptr<ThreadService> m_threadService;

        boost::shared_ptr<EpoxyServerContext> m_context;

    public:

        EpoxyServer(const Allocator& allocator,
                    const SocketAddress& address,
                    const boost::shared_ptr<INetworkServerSink>& handler,
                    const boost::shared_ptr<ThreadService>& threadService)
            : m_threadService(threadService)
        {
            BOOST_ASSERT(nullptr != m_threadService);
            m_context = boost::make_shared<EpoxyServerContext>(allocator, address, handler, m_threadService->io_service());

            //
            // Start accepting incoming connections.
            //
            m_context->DoAccept();
        }


        ~EpoxyServer()
        {
            BOND_LOG(LOG_DEBUG, "EpoxyServer",
                     "Stopping network server");

            m_context->Shutdown();
        }
    };

    class ClientConnection
        : public INetworkConnection
    {
        boost::shared_ptr<ThreadService> m_threadService;

        boost::shared_ptr<EpoxyConnection> m_connection;

    public:
        //
        // Construct client side connection.
        //
        ClientConnection(const Allocator& allocator,
                         const SocketAddress& address,
                         const boost::shared_ptr<INetworkConnectionSink>& handler,
                         const boost::shared_ptr<ThreadService>& threadService)
            : m_threadService(threadService)
            , m_connection(boost::make_shared<EpoxyConnection>(allocator, address, handler, threadService->io_service()))
        {
            m_connection->Connect();
        }

        //
        // Schedule a packet into outstanding queue:
        //
        // @param conversation_id: unique packet id;
        // @param buffers: packet payload;
        //
        void Send(uint64_t conversation_id,
                  Packet packet) override
        {
            m_connection->Send(conversation_id, std::move(packet));
        }
    };

// Implement PacketTransport

    boost::shared_ptr<INetworkConnection>
    StartClientConnection(const SocketAddress& address,
                          const boost::shared_ptr<INetworkConnectionSink>& handler)
    override
    {
        return boost::make_shared<ClientConnection>(m_allocator, address, handler, m_threadService);
    }


    boost::shared_ptr<INetworkServer>
    StartServer(const bond::comm::SocketAddress& address,
                const boost::shared_ptr<INetworkServerSink>& handler)
    override
    {
        boost::shared_ptr<INetworkServer> server = 
            boost::make_shared<EpoxyServer>(m_allocator,
                                            address,
                                            handler,
                                            m_threadService);

        //
        // Notify bond.rpc of server start.
        //
        handler->ServerStarted();
        return server;
    }


    //
    //  Allocator instance to be used.
    //
    Allocator m_allocator;

    //
    // Bond thread service
    //
    boost::shared_ptr<ThreadService> m_threadService;
};
} // namespace detail


//
// Epoxy transport implementation details are aligned with C# counterpart.
//
typedef LayerTransport<
    detail::EpoxyTransport<CompactWireProtocol>, // transport impl
    CompactWireProtocol // wire protocol to be used by layers
> EpoxyTransport;

} } } // namespace bond.comm.epoxy
