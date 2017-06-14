
#pragma once

#include <bond/comm/message.h>
#include <bond/comm/comm_message.h>

namespace bond { namespace comm
{


template <typename Allocator>
class MemoryStreamFactory
{
public:
    typedef OutputMemoryStream<Allocator> OutputStream;

    MemoryStreamFactory(const Allocator& allocator = Allocator())
        : _reserveSize(8 * 1024 - 128), // preallocate not more than 8 KB to stay in low-fragmentation heap (leave some space for shared_ptr)
          _reserveBlobs(64),
          _allocator(allocator),
          _minChanningSize(32),
          _maxChainLength(60)           // target a chain of 60 buffers so that we don't need to merge before sending
    {}


    MemoryStreamFactory(uint32_t reserveSize,
                  uint32_t reserveBlobs,
                  uint32_t minChanningSize,
                  uint32_t maxChainLength,
                  const Allocator& allocator = Allocator())
        : _reserveSize(reserveSize),
          _reserveBlobs(reserveBlobs),
          _allocator(allocator),
          _minChanningSize(minChanningSize),
          _maxChainLength(maxChainLength)
    {}


    OutputStream Create() const
    {
        return OutputStream(_reserveSize,
                            _reserveBlobs,
                            _allocator,
                            _minChanningSize,
                            _maxChainLength);
    }

private:

    uint32_t _reserveSize;
    uint32_t _reserveBlobs;
    Allocator _allocator;
    uint32_t _minChanningSize;
    uint32_t _maxChainLength;
};


//
// Policy to serialize/de-serialize payloads
//
template <template <typename Stream> class Writer,
          typename StreamFactory = MemoryStreamFactory<std::allocator<char>>>
class WireProtocol
    : public StreamFactory
{
public:

    WireProtocol()
    {}


    WireProtocol(const StreamFactory& streamFactory)
        : StreamFactory(streamFactory)
    {}


    // Pack a typed payload into buffers using default protocol.
    template <typename T>
    std::vector<blob>
    Pack(const bonded<T>& payload) const
    {
        auto stream = StreamFactory::Create();

        Writer<typename StreamFactory::OutputStream> writer(stream);
        Marshal(payload, writer);

        std::vector<blob> buffers;
        stream.GetBuffers(buffers);

        return buffers;
    }


    // Pack a typed payload into buffers using explicit protocol.
    template <typename T>
    std::vector<blob>
    Pack(const bonded<T>& payload, bond::ProtocolType requestProtocol) const
    {
        auto stream = StreamFactory::Create();

        ::bond::Apply< ::bond::Marshaler>(payload, stream, static_cast<uint16_t>(requestProtocol));

        std::vector<blob> buffers;
        stream.GetBuffers(buffers);

        return buffers;
    }


    // Unpack buffers into typed payload
    template <typename T>
    bonded<T>
    Unpack(const std::vector<blob>& payload) const
    {
        bond::ProtocolType temp;
        return Unpack<T>(payload, temp);
    }

    // Unpack buffers into typed payload
    template <typename T>
    bonded<T>
    Unpack(const std::vector<blob>& payload,
           bond::ProtocolType& protocol) const
    {
        InputBuffer buffer(merge(payload.begin(), payload.end()));
        bond::bonded<T> bonded;

        protocol = SelectProtocolAndApply<T>(buffer, boost::ref(bonded)).first;

        return bonded;
    }
};


typedef WireProtocol<FastBinaryWriter> FastWireProtocol;
typedef WireProtocol<CompactBinaryWriter> CompactWireProtocol;

} } // namespace bond.comm
