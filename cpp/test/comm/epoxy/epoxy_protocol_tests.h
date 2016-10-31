#include <memory>
#include <string>

#include <bond/comm/comm_types.h>
#include <bond/comm/epoxy_transport_apply.h>
#include <bond/comm/message.h>
#include <bond/comm/transport/detail/epoxy_data_structs.h>

namespace bond { namespace comm { namespace epoxy
{
template <typename P>
inline detail::FrameState message_to_frame(
    uint64_t conversation_id, std::string service_name, std::string method_name, EpoxyMessageType type, message<P>& message, blob& layer_data)
{
    detail::FrameState frame;
    frame.count = 0;

    {
        EpoxyHeaders headers;
        headers.conversation_id = conversation_id;
        headers.message_type = type;
        headers.service_name = service_name;
        headers.method_name = method_name;

        const auto initial_header_buffer_size = 150;
        OutputBuffer output_buffer(initial_header_buffer_size);
        FastBinaryWriter<OutputBuffer>fast_writer (output_buffer);
        Serialize(headers, fast_writer);

        frame.framelets.emplace_back(detail::FrameletType::EPOXY_HEADERS, output_buffer.GetBuffer());
        frame.count++;
    }

    if (!layer_data.empty())
    {
        frame.framelets.emplace_back(detail::FrameletType::LAYER_DATA, layer_data);
        frame.count++;
    }

    {
        const auto initial_payload_buffer_size = 1024;
        OutputBuffer output_buffer(initial_payload_buffer_size);
        CompactBinaryWriter<OutputBuffer> compact_writer(output_buffer);
        compact_writer.WriteVersion();

        if (message.is_error())
        {
            Serialize((Error &)message.err(), compact_writer);
        }
        else
        {
            Serialize(message.value(), compact_writer);
        }

        detail::FrameletType frameletType = (message.is_error() ? detail::FrameletType::ERROR_DATA : detail::FrameletType::PAYLOAD_DATA);
        frame.framelets.emplace_back(frameletType, output_buffer.GetBuffer());
        frame.count++;
    }

    return frame;
}

inline detail::FrameState make_config_frame()
{
    EpoxyConfig empty_config;

    OutputBuffer output_buffer(1);
    FastBinaryWriter<OutputBuffer> fast_writer(output_buffer);
    Serialize(empty_config, fast_writer);

    detail::FrameState frame;
    frame.framelets.emplace_back(detail::FrameletType::EPOXY_CONFIG, output_buffer.GetBuffer());
    frame.count = 1;
    return frame;
}

inline detail::FrameState make_protocol_error_frame(ProtocolErrorCode error_code, Error* details)
{
    ProtocolError protocol_error;
    protocol_error.error_code = error_code;
    if (details != nullptr)
    {
        protocol_error.details.set(bonded<Error>(*details));
    }

    OutputBuffer output_buffer(16);
    FastBinaryWriter<OutputBuffer> fast_writer(output_buffer);
    Serialize(protocol_error, fast_writer);

    detail::FrameState frame;
    frame.framelets.emplace_back(detail::FrameletType::PROTOCOL_ERROR, output_buffer.GetBuffer());
    frame.count = 1;
    return frame;
}
} } }   // namespace bond.comm.epoxy
