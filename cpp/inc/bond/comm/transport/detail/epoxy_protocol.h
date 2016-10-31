#pragma once

#include <memory>

#include <boost/optional.hpp>

#include <bond/core/blob.h>
#include <bond/core/bond.h>
#include <bond/comm/detail/logging.h>
#include <bond/comm/epoxy_transport_types.h>
#include <bond/comm/packet_types.h>

#include "epoxy_data_structs.h"

namespace bond { namespace comm
{
namespace epoxy { namespace detail
{
// Indicates what action should be taken in response to a Frame.
enum class FrameDisposition
{
    // The disposition of a Frame about which we know nothing. If this is ever returned by classify(), it
    // indicates that classify() detected a bug and refused to continue.
    Indeterminate,

    // The frame was a valid Request.
    DeliverRequestToService,

    // The frame was a valid Response.
    DeliverResponseToProxy,

    // The frame was a valid Event.
    DeliverEventToService,

    // The frame is a valid Config frame and needs to be handled.
    ProcessConfig,

    // The frame is a valid ProtocolError frame and needs to be handled.
    HandleProtocolError,

    // The frame was not valid, and the caller should send an error to the remote host.
    SendProtocolError,

    // The caller should silently close the connection.
    HangUp
};

struct ClassifyResult
{
    FrameDisposition disposition = FrameDisposition::Indeterminate;
    std::unique_ptr<epoxy::EpoxyHeaders> headers = nullptr;
    std::vector<::bond::blob> layer_data;
    ::bond::comm::MessageData message_data;
    std::unique_ptr<epoxy::ProtocolError> error = nullptr;
    boost::optional<enum ProtocolErrorCode> error_code = boost::none;
};

// States for the state machine inside classify().
enum class ClassifyState
{
    // These states are the happy path from no knowledge to proving a frame is good and knowing what to do with
    // its contents. Each is implemented in its own function below.

    // Do we have a frame at all?
    ExpectFrame,

    // Does the frame have at least one framelet? Is the first one valid?
    ExpectFirstFramelet,

    // Does the frame begin with a valid EpoxyHeaders?
    ExpectEpoxyHeaders,

    // Does the frame have (optional) LayerData after the EpoxyHeaders?
    ExpectOptionalLayerData,

    // Does the frame have a Payload immediately after the EpoxyHeaders (or LayerData if it was present)?
    ExpectPayload,

    // The frame has all required framelets. Does it have any trailing ones?
    ExpectEndOfFrame,

    // Do we know what to do with frames with this PayloadType?
    FrameComplete,

    // There are no problems with this frame. What should we do with it?
    ValidFrame,

    // This state is the happy path from believing the frame represents config to proving the frame is good
    // and knowing what to do with that config.
    ExpectConfig,

    // This state is the happy path from believing the frame represents an error to proving the frame is good
    // and knowing what to do with that error.
    ExpectProtocolError,

    // Terminal states. Each of these indicates that we have classified the frame and need to return something.
    // Because these frames always mean the next step is to return, their implementations are inlined in
    // classify().

    // There are no problems with this frame, and we know what to do with it. Return from classify().
    ClassifiedValidFrame,

    // We could not interpret the frame, or it violated the protocol. Return from classify().
    MalformedFrame,

    // We got an error, but couldn't interpret it.
    ErrorInErrorFrame,

    // We detected a bug in the state machine. Return from classify() with a FrameDisposition of Indeterminate.
    // This should never happen.
    InternalStateError
};

static const char * const category = "classify";

// This needs to be larger than the longest valid path through the state machine.
static const int maximum_transitions = 10;

inline ClassifyState TransitionExpectFirstFramelet(
    ClassifyState state, const FrameState& frame, boost::optional<epoxy::ProtocolErrorCode>& error_code)
{
    BOOST_VERIFY(state == ClassifyState::ExpectFirstFramelet);

    if (frame.framelets.size() == 0)
    {
        BOND_LOG(LOG_ERROR, category, "Frame was empty.");
        error_code = epoxy::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }

    switch (frame.framelets[0].first)
    {
        case FrameletType::EPOXY_HEADERS:
            return ClassifyState::ExpectEpoxyHeaders;

        case FrameletType::EPOXY_CONFIG:
            return ClassifyState::ExpectConfig;

        case FrameletType::PROTOCOL_ERROR:
            return ClassifyState::ExpectProtocolError;

        default:
            BOND_LOG(LOG_ERROR, category,
                "Frame began with invalid FrameletType " << (int) frame.framelets[0].first << ".");
            error_code = epoxy::MALFORMED_DATA;
            return ClassifyState::MalformedFrame;
    }
}

inline ClassifyState TransitionExpectEpoxyHeaders(
    ClassifyState state, const FrameState& frame, std::unique_ptr<epoxy::EpoxyHeaders>& headers,
    boost::optional<epoxy::ProtocolErrorCode>& error_code)
{
    BOOST_VERIFY(state == ClassifyState::ExpectEpoxyHeaders);

    if (frame.framelets.size() == 0 || frame.framelets[0].first != FrameletType::EPOXY_HEADERS)
    {
        return ClassifyState::InternalStateError;
    }

    headers = std::unique_ptr<epoxy::EpoxyHeaders>(new EpoxyHeaders);
    blob framelet = frame.framelets[0].second;
    InputBuffer input_buffer(framelet);
    FastBinaryReader<InputBuffer> fast_binary_reader(input_buffer);
    try
    {
        Deserialize(fast_binary_reader, *headers);
    }
    catch (const bond::Exception&)
    {
        BOND_LOG(LOG_ERROR, category, "Didn't get a valid EpoxyHeaders.");
        error_code = ProtocolErrorCode::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }

    std::string payload_type_name;
    bool result = FromEnum(payload_type_name, headers->message_type);
    if (!result)
    {
        payload_type_name = "<unknown: " + std::to_string((int) headers->message_type) + ">";
    }
    BOND_LOG(LOG_DEBUG, category,
        "Deserialized EpoxyHeaders with conversation ID " << headers->conversation_id
        << " and payload type " << payload_type_name << ".");
    return ClassifyState::ExpectOptionalLayerData;
}

inline ClassifyState TransitionExpectOptionalLayerData(
    ClassifyState state, const FrameState& frame, const std::unique_ptr<epoxy::EpoxyHeaders>& headers,
    blob& layer_data, boost::optional<epoxy::ProtocolErrorCode>& error_code)
{
    BOOST_VERIFY(state == ClassifyState::ExpectOptionalLayerData);

    if (frame.framelets.size() < 2)
    {
        BOND_LOG(LOG_ERROR, category, "Frame had headers but no payload.");
        error_code = ProtocolErrorCode::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }

    if (frame.framelets[1].first == FrameletType::LAYER_DATA)
    {
        layer_data = frame.framelets[1].second;
        BOND_LOG(LOG_DEBUG, category,
            "Extracted " << layer_data.length() << "-byte layer data in conversation ID " << headers->conversation_id << ".");
    }

    return ClassifyState::ExpectPayload;
}

inline ClassifyState TransitionExpectPayload(
    ClassifyState state, const FrameState& frame, const std::unique_ptr<epoxy::EpoxyHeaders>& headers,
    const blob& layer_data, MessageData& message_data, boost::optional<epoxy::ProtocolErrorCode>& error_code)
{
    BOOST_VERIFY(state == ClassifyState::ExpectPayload);

    size_t payload_data_index = (layer_data.empty() ? 1 : 2);
    if (payload_data_index >= frame.framelets.size())
    {
        BOND_LOG(LOG_ERROR, category, "Frame had headers but no payload.");
        error_code = ProtocolErrorCode::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }

    FrameletType frameletType = frame.framelets[payload_data_index].first;
    if (frameletType != FrameletType::PAYLOAD_DATA && frameletType != FrameletType::ERROR_DATA)
    {
        BOND_LOG(LOG_ERROR, category, "Frame had headers but no payload.");
        error_code = ProtocolErrorCode::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }

    message_data.is_error = (frameletType == FrameletType::ERROR_DATA);
    message_data.data.emplace_back(std::move(frame.framelets[payload_data_index].second));
    BOND_LOG(LOG_DEBUG, category,
        "Extracted " << message_data.data[0].length() << "-byte payload in conversation ID " << headers->conversation_id << ".");
    return ClassifyState::ExpectEndOfFrame;
}

inline ClassifyState TransitionExpectEndOfFrame(
    ClassifyState state, const FrameState& frame, const blob& layer_data,
    boost::optional<epoxy::ProtocolErrorCode>& error_code)
{
    BOOST_VERIFY(state == ClassifyState::ExpectEndOfFrame);

    size_t valid_frame_size = (layer_data.empty() ? 2 : 3);
    if (frame.framelets.size() == valid_frame_size)
    {
        return ClassifyState::FrameComplete;
    }
    else
    {
        BOND_LOG(LOG_ERROR, category, "Frame had trailing framelets.");
        error_code = ProtocolErrorCode::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }
}

inline ClassifyState TransitionFrameComplete(
    ClassifyState state, const std::unique_ptr<epoxy::EpoxyHeaders>& headers,
    boost::optional<epoxy::ProtocolErrorCode>& error_code)
{
    BOOST_VERIFY(state == ClassifyState::FrameComplete);

    switch (headers->message_type)
    {
    case EpoxyMessageType::REQUEST:
    case EpoxyMessageType::RESPONSE:
    case EpoxyMessageType::EVENT:
        return ClassifyState::ValidFrame;

    default:
        BOND_LOG(LOG_WARNING, category,
            "Received unrecognized payload type " << headers->message_type << ".");
        error_code = ProtocolErrorCode::NOT_SUPPORTED;
        return ClassifyState::MalformedFrame;
    }
}

inline ClassifyState TransitionValidFrame(
    ClassifyState state, const std::unique_ptr<epoxy::EpoxyHeaders>& headers, FrameDisposition& disposition)
{
    BOOST_VERIFY(state == ClassifyState::ValidFrame);

    switch (headers->message_type)
    {
    case EpoxyMessageType::REQUEST:
        disposition = FrameDisposition::DeliverRequestToService;
        return ClassifyState::ClassifiedValidFrame;

    case EpoxyMessageType::RESPONSE:
        disposition = FrameDisposition::DeliverResponseToProxy;
        return ClassifyState::ClassifiedValidFrame;

    case EpoxyMessageType::EVENT:
        disposition = FrameDisposition::DeliverEventToService;
        return ClassifyState::ClassifiedValidFrame;

    default:
        return ClassifyState::InternalStateError;
    }
}

inline ClassifyState TransitionExpectConfig(
    ClassifyState state, const FrameState& frame, boost::optional<epoxy::ProtocolErrorCode>& error_code,
    FrameDisposition& disposition)
{
    BOOST_VERIFY(state == ClassifyState::ExpectConfig);

    if (frame.framelets.size() == 0 || frame.framelets[0].first != FrameletType::EPOXY_CONFIG)
    {
        return ClassifyState::InternalStateError;
    }

    if (frame.framelets.size() != 1)
    {
        BOND_LOG(LOG_ERROR, category, "Config frame had trailing framelets.");
        error_code = ProtocolErrorCode::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }

    epoxy::EpoxyConfig config;
    blob framelet = frame.framelets[0].second;
    InputBuffer input_buffer(framelet);
    FastBinaryReader<InputBuffer> fast_binary_reader(input_buffer);
    try
    {
        Deserialize(fast_binary_reader, config);
    }
    catch (const bond::Exception&)
    {
        BOND_LOG(LOG_ERROR, category, "Didn't get a valid EpoxyConfig.");
        error_code = ProtocolErrorCode::MALFORMED_DATA;
        return ClassifyState::MalformedFrame;
    }

    disposition = FrameDisposition::ProcessConfig;
    return ClassifyState::ClassifiedValidFrame;
}

inline ClassifyState TransitionExpectProtocolError(
    ClassifyState state, const FrameState& frame, std::unique_ptr<epoxy::ProtocolError>& error,
    FrameDisposition& disposition)
{
    BOOST_VERIFY(state == ClassifyState::ExpectProtocolError);

    if (frame.framelets.size() == 0 || frame.framelets[0].first != FrameletType::PROTOCOL_ERROR)
    {
        return ClassifyState::InternalStateError;
    }

    if (frame.framelets.size() > 1)
    {
        BOND_LOG(LOG_ERROR, category, "Protocol error frame had trailing framelets.");
        return ClassifyState::ErrorInErrorFrame;
    }

    error = std::unique_ptr<epoxy::ProtocolError>(new ProtocolError);
    blob framelet = frame.framelets[0].second;
    InputBuffer input_buffer(framelet);
    FastBinaryReader<InputBuffer> fast_binary_reader(input_buffer);
    try
    {
        Deserialize(fast_binary_reader, *error);
    }
    catch (const bond::Exception&)
    {
        BOND_LOG(LOG_ERROR, category, "Didn't get a valid ProtocolError.");
        return ClassifyState::ErrorInErrorFrame;
    }

    BOND_LOG(LOG_DEBUG, category, "Deserialized protocol error with code " << error->error_code << ".");
    disposition = FrameDisposition::HandleProtocolError;
    return ClassifyState::ClassifiedValidFrame;
}

inline std::unique_ptr<ClassifyResult> Classify(const FrameState& frame)
{
    BOND_LOG(LOG_DEBUG,
        category,
        "Processing " << frame.framelets.size() << " framelets.");
    auto result = std::unique_ptr<ClassifyResult>(new ClassifyResult);

    auto state = ClassifyState::ExpectFirstFramelet;
    auto disposition = FrameDisposition::Indeterminate;
    std::unique_ptr<epoxy::EpoxyHeaders> headers = nullptr;
    blob layer_data;
    MessageData message_data;
    std::unique_ptr<epoxy::ProtocolError> error = nullptr;
    boost::optional<epoxy::ProtocolErrorCode> error_code;
    unsigned int transitions = 0;
    while (true)
    {
        // If it looks like we have a bug and are looping forever, bail out of the state machine.
        if (transitions++ > maximum_transitions)
        {
            result->disposition = FrameDisposition::Indeterminate;
            return result;
        }

        switch (state)
        {
        case ClassifyState::ExpectFirstFramelet:
            state = TransitionExpectFirstFramelet(state, frame, error_code);
            continue;

        case ClassifyState::ExpectEpoxyHeaders:
            state = TransitionExpectEpoxyHeaders(state, frame, headers, error_code);
            continue;

        case ClassifyState::ExpectOptionalLayerData:
            state = TransitionExpectOptionalLayerData(state, frame, headers, layer_data, error_code);
            continue;

        case ClassifyState::ExpectPayload:
            state = TransitionExpectPayload(state, frame, headers, layer_data, message_data, error_code);
            continue;

        case ClassifyState::ExpectEndOfFrame:
            state = TransitionExpectEndOfFrame(state, frame, layer_data, error_code);
            continue;

        case ClassifyState::FrameComplete:
            state = TransitionFrameComplete(state, headers, error_code);
            continue;

        case ClassifyState::ValidFrame:
            state = TransitionValidFrame(state, headers, disposition);
            continue;

        case ClassifyState::ExpectConfig:
            state = TransitionExpectConfig(state, frame, error_code, disposition);
            continue;

        case ClassifyState::ExpectProtocolError:
            state = TransitionExpectProtocolError(state, frame, error, disposition);
            continue;

        case ClassifyState::ClassifiedValidFrame:
            if (disposition == FrameDisposition::Indeterminate)
            {
                state = ClassifyState::InternalStateError;
                continue;
            }

            result->disposition = disposition;
            result->headers = std::move(headers);
            if (!layer_data.empty())
            {
                result->layer_data.emplace_back(layer_data);
            }
            result->message_data = std::move(message_data);
            result->error = std::move(error);
            return result;

        case ClassifyState::MalformedFrame:
            if (error_code == boost::none)
            {
                state = ClassifyState::InternalStateError;
                continue;
            }

            result->disposition = FrameDisposition::SendProtocolError;
            result->error_code = error_code;
            return result;

        case ClassifyState::ErrorInErrorFrame:
            {
                auto new_error = std::unique_ptr<ProtocolError>(new ProtocolError);
                new_error->error_code = ProtocolErrorCode::ERROR_IN_ERROR;
                result->error = std::move(new_error);
            }

            result->disposition = FrameDisposition::HangUp;
            return result;

        case ClassifyState::InternalStateError:
            result->disposition = FrameDisposition::Indeterminate;
            return result;

        default:
            BOND_LOG(LOG_ERROR,
                category,
                "Unhandled state " << (int) state << ". Dropping frame.");
            result->disposition = FrameDisposition::Indeterminate;
            return result;
        }
    }
}
}}  // namespace epoxy.detail
}}  // namespace bond.comm
