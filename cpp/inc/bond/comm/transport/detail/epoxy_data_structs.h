#pragma once

#include <stdint.h>
#include <utility>
#include <vector>

#include <bond/core/blob.h>

namespace bond { namespace comm
{
namespace epoxy { namespace detail
{
enum class FrameletType
{
    EPOXY_CONFIG = 0x4743,           // "GC"
    EPOXY_HEADERS = 0x5248,          // "RH"
    ERROR_DATA = 0x4445,             // "DE"
    LAYER_DATA = 0x594C,             // "YL"
    PAYLOAD_DATA = 0x4450,           // "DP"
    PROTOCOL_ERROR = 0x5245,         // "RE"
};

#pragma pack(push, 2)
struct FrameTag
{
    uint16_t type;
    uint32_t length;
};
#pragma pack(pop)

struct FrameState
{
    uint16_t count;
    FrameTag tag;
    std::vector<std::pair<FrameletType, blob>> framelets;
};

inline
std::shared_ptr<std::vector<blob>> MakeConfigFrame(std::allocator<char> allocator)
{
    OutputBuffer stream(256, static_cast<uint32_t>(256));

    epoxy::EpoxyConfig config;

    OutputBuffer output(256, 4);
    FastBinaryWriter<OutputBuffer> writer(output);

    Serialize(config, writer);

    std::vector<blob> data;
    output.GetBuffers(data);

    uint32_t length = 0;
    for (const blob& buffer : data)
    {
        length += buffer.length();
    }

    stream.Write(static_cast<uint16_t>(1));     // A config frame has one framelet.
    stream.Write(static_cast<uint16_t>(detail::FrameletType::EPOXY_CONFIG));
    stream.Write(static_cast<uint32_t>(length));

    for (const blob& buffer : data)
    {
        stream.Write(buffer);
    }
    std::shared_ptr<std::vector<blob>> buffers = std::allocate_shared<std::vector<blob>>(allocator);

    stream.GetBuffers(*buffers);
    return buffers;
}

}}  // namespace epoxy.detail
}}  // namespace bond.comm
