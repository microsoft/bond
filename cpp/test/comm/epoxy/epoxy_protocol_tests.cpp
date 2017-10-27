#define BOND_ENABLE_LOG_HANDLER

#include <iostream>
#include <memory>
#include <string>

#include <bond/comm/comm_types.h>
#include <bond/comm/transport/epoxy.h>
#include <bond/comm/transport/detail/epoxy_data_structs.h>
#include "epoxy_protocol_tests_types.h"
#include "epoxy_protocol_tests_reflection.h"

#include "../../core/unit_test_framework.h"

#include <bond/comm/transport/detail/epoxy_protocol.h>
#include "epoxy_protocol_tests.h"

using namespace std;

namespace bond { namespace comm { namespace epoxy
{
static const auto good_request_id = 1u;
static const auto good_response_id = 2u;
static const auto good_service = "MyService";
static const auto good_method = "ShaveYaks";
static const auto good_payload = unique_ptr<test::Dummy>(new test::Dummy);
static message<test::Dummy> meaningless_payload(*good_payload);
static Error some_error = []{ Error e; e.error_code = ErrorCode::INTERNAL_SERVER_ERROR, e.message = "Meaningless message"; return e; }();
static bond::comm::error error_wrapper(some_error);
static message<test::Dummy> meaningless_error(error_wrapper);
static const auto meaningless_error_code = ProtocolErrorCode::GENERIC_ERROR;

static unique_ptr<EpoxyHeaders> good_request_headers;
static unique_ptr<EpoxyHeaders> good_response_headers;
static unique_ptr<EpoxyHeaders> good_event_headers;
static unique_ptr<EpoxyHeaders> unknown_type_headers;
static blob empty_layer_data;
static blob non_empty_layer_data;

static detail::FrameState good_request_frame;
static detail::FrameState good_request_layer_data_frame;
static detail::FrameState good_response_frame;
static detail::FrameState good_error_response_frame;
static detail::FrameState good_event_frame;
static detail::FrameState short_request_frame;           // a request frame with EpoxyHeaders but no PayloadData
static detail::FrameState double_headers_request_frame;  // a request frame with duplicate EpoxyHeaders
static detail::FrameState headers_config_request_frame;  // a request frame with EpoxyHeaders, then an EpoxyConfig
static detail::FrameState double_payload_request_frame;  // a request frame with duplicate PayloadData
static detail::FrameState backwards_request_frame;       // a request frame with PayloadData before EpoxyHeaders
static detail::FrameState config_frame;                  // a frame with a well-formed EpoxyConfig
static detail::FrameState config_frame_extra;            // a frame with a well-formed EpoxyConfig and extra stuff
static detail::FrameState config_frame_bad_config_data;  // a frame that fits that shape of a config frame, but with a payload that can't be deserialized
static detail::FrameState protocol_error_frame;          // a frame with a well-formed ProtocolError
static detail::FrameState double_protocol_error_frame;   // a frame with two ProtocolError frames
static detail::FrameState empty_frame;

static void CreateFramelets()
{
    good_request_headers = unique_ptr<EpoxyHeaders>(new EpoxyHeaders);
    good_request_headers->service_name = good_service;
    good_request_headers->method_name = good_method;
    good_request_headers->message_type = EpoxyMessageType::REQUEST;
    good_request_headers->conversation_id = good_request_id;

    good_response_headers = unique_ptr<EpoxyHeaders>(new EpoxyHeaders);
    good_response_headers->service_name = good_service;
    good_response_headers->method_name = good_method;
    good_response_headers->message_type = EpoxyMessageType::RESPONSE;
    good_response_headers->conversation_id = good_response_id;

    good_event_headers = unique_ptr<EpoxyHeaders>(new EpoxyHeaders);
    good_event_headers->service_name = good_service;
    good_event_headers->method_name = good_method;
    good_event_headers->message_type = EpoxyMessageType::EVENT;
    good_event_headers->conversation_id = good_request_id;

    unknown_type_headers = unique_ptr<EpoxyHeaders>(new EpoxyHeaders);
    unknown_type_headers->service_name = good_service;
    unknown_type_headers->method_name = good_method;
    unknown_type_headers->message_type = static_cast<EpoxyMessageType>(-100);
    unknown_type_headers->conversation_id = good_request_id;

    non_empty_layer_data = blob(boost::make_shared<char[]>(3), 3);
}

static void CreateFrames()
{
    // Good frames, from which we can pull good framelets to build bad frames
    good_request_frame = message_to_frame(
        good_request_id, good_service, good_method, EpoxyMessageType::REQUEST, meaningless_payload, empty_layer_data);
    good_request_layer_data_frame = message_to_frame(
        good_request_id, good_service, good_method, EpoxyMessageType::REQUEST, meaningless_payload, non_empty_layer_data);

    good_response_frame = message_to_frame(
        good_response_id, good_service, good_method, EpoxyMessageType::RESPONSE, meaningless_payload, empty_layer_data);
    good_error_response_frame = message_to_frame(
        good_response_id, good_service, good_method, EpoxyMessageType::RESPONSE, meaningless_error, empty_layer_data);


    good_event_frame = message_to_frame(
        good_request_id, good_service, good_method, EpoxyMessageType::EVENT, meaningless_payload , empty_layer_data);

    config_frame = make_config_frame();
    protocol_error_frame = make_protocol_error_frame(meaningless_error_code, nullptr);

    auto good_framelet_count = good_request_frame.count;

    // Bad frames made of good framelets.
    short_request_frame.count = good_framelet_count - 1;
    for (int i = 0; i < good_framelet_count - 1; i++)
    {
        short_request_frame.framelets.emplace_back(good_request_frame.framelets[i]);
    }

    double_headers_request_frame.count = good_framelet_count + 1;
    double_headers_request_frame.framelets.emplace_back(good_request_frame.framelets[0]);
    for (const auto& framelet : good_request_frame.framelets)
    {
        double_headers_request_frame.framelets.emplace_back(framelet);
    }

    headers_config_request_frame.count = 2;
    headers_config_request_frame.framelets.emplace_back(good_request_frame.framelets[0]);
    headers_config_request_frame.framelets.emplace_back(config_frame.framelets[0]);

    double_payload_request_frame.count = good_framelet_count + 1;
    for (const auto& framelet : good_request_frame.framelets)
    {
        double_payload_request_frame.framelets.emplace_back(framelet);
    }
    double_payload_request_frame.framelets.emplace_back(good_request_frame.framelets[good_framelet_count - 1]);

    backwards_request_frame.count = good_framelet_count;
    for (int i = good_framelet_count - 1; i >= 0; i--)
    {
        backwards_request_frame.framelets.emplace_back(good_request_frame.framelets[i]);
    }

    double_protocol_error_frame = make_protocol_error_frame(meaningless_error_code, nullptr);
    double_protocol_error_frame.framelets.emplace_back(protocol_error_frame.framelets[0]);
    double_protocol_error_frame.count++;

    config_frame_extra = make_config_frame();
    config_frame_extra.framelets.emplace_back(good_request_frame.framelets[0]);
    config_frame_extra.count++;

    // Bad frames made of bad framelets.
    auto invalid_config_data = boost::make_shared<char[]>(1);
    invalid_config_data[0] = 0x01;
    config_frame_bad_config_data.framelets.emplace_back(detail::FrameletType::EPOXY_CONFIG, blob(invalid_config_data, 1));
    config_frame_bad_config_data.count = 1;

    empty_frame.count = 0;
}

// For each state that is implemented as a function, test:
//  * that it works in the happy path
//  * that it transitions to an internal error if it's coming from any unexpected state
//  * that it transitions to an internal error if any state left by previous transitions is unacceptable
//  * that it transitions to an expected error state if the frame is malformed

static void TransitionExpectFirstFramelet_Valid()
{
    boost::optional<epoxy::ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectFirstFramelet(
        detail::ClassifyState::ExpectFirstFramelet, good_request_frame, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectEpoxyHeaders == after);
    UT_AssertIsFalse(error_code);

    after = detail::TransitionExpectFirstFramelet(
        detail::ClassifyState::ExpectFirstFramelet, protocol_error_frame, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectProtocolError == after);
    UT_AssertIsFalse(error_code);
}

static void TransitionExpectFirstFramelet_MalformedFrame()
{
    boost::optional<epoxy::ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectFirstFramelet(
        detail::ClassifyState::ExpectFirstFramelet, empty_frame, error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);

    after = detail::TransitionExpectFirstFramelet(
        detail::ClassifyState::ExpectFirstFramelet, backwards_request_frame, error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);
}

static void TransitionExpectEpoxyHeaders_Valid()
{
    unique_ptr<epoxy::EpoxyHeaders> request_headers = nullptr;
    boost::optional<epoxy::ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectEpoxyHeaders(
        detail::ClassifyState::ExpectEpoxyHeaders, good_request_frame, request_headers, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectOptionalLayerData == after);
    UT_AssertIsNotNull(request_headers);
    UT_AssertAreEqual(good_request_id, request_headers->conversation_id);
    UT_AssertAreEqual(good_service, request_headers->service_name);
    UT_AssertAreEqual(good_method, request_headers->method_name);
    UT_AssertAreEqual(EpoxyMessageType::REQUEST, request_headers->message_type);
    UT_AssertIsFalse(error_code);

    unique_ptr<epoxy::EpoxyHeaders> request_layer_data_headers = nullptr;
    after = detail::TransitionExpectEpoxyHeaders(
        detail::ClassifyState::ExpectEpoxyHeaders, good_request_layer_data_frame, request_layer_data_headers,
        error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectOptionalLayerData == after);
    UT_AssertIsNotNull(request_layer_data_headers);
    UT_AssertAreEqual(good_request_id, request_layer_data_headers->conversation_id);
    UT_AssertAreEqual(good_service, request_headers->service_name);
    UT_AssertAreEqual(good_method, request_layer_data_headers->method_name);
    UT_AssertAreEqual(EpoxyMessageType::REQUEST, request_layer_data_headers->message_type);
    UT_AssertIsFalse(error_code);
}

static void TransitionExpectEpoxyHeaders_InvalidPreconditions()
{
    unique_ptr<epoxy::EpoxyHeaders> headers = nullptr;
    boost::optional<epoxy::ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectEpoxyHeaders(
        detail::ClassifyState::ExpectEpoxyHeaders, empty_frame, headers, error_code);
    UT_AssertIsTrue(detail::ClassifyState::InternalStateError == after);
    UT_AssertIsNull(headers);
    UT_AssertIsFalse(error_code);

    after = detail::TransitionExpectEpoxyHeaders(
        detail::ClassifyState::ExpectEpoxyHeaders, backwards_request_frame, headers, error_code);
    UT_AssertIsTrue(detail::ClassifyState::InternalStateError == after);
    UT_AssertIsNull(headers);
    UT_AssertIsFalse(error_code);
}

static void TransitionOptionalExpectLayerData_Valid()
{
    blob layer_data;
    boost::optional<epoxy::ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectOptionalLayerData(
        detail::ClassifyState::ExpectOptionalLayerData, good_request_frame, good_request_headers, layer_data,
        error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectPayload == after);
    UT_AssertIsTrue(layer_data.empty());
    UT_AssertIsFalse(error_code);

    after = detail::TransitionExpectOptionalLayerData(
        detail::ClassifyState::ExpectOptionalLayerData, good_request_layer_data_frame, good_request_headers,
        layer_data, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectPayload == after);
    UT_AssertIsFalse(layer_data.empty());
    UT_AssertIsFalse(error_code);
}

static void TransitionExpectOptionalLayerData_MalformedFrame()
{
    blob layer_data;
    boost::optional<epoxy::ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectOptionalLayerData(
        detail::ClassifyState::ExpectOptionalLayerData, short_request_frame, good_request_headers, layer_data,
        error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(layer_data.empty());
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);
}

static void TransitionExpectPayload_Valid()
{
    MessageData message_data;
    boost::optional<ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectPayload(
        detail::ClassifyState::ExpectPayload, good_request_frame, good_request_headers, empty_layer_data, message_data,
        error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectEndOfFrame == after);
    UT_AssertIsFalse(message_data.data.empty());
    UT_AssertIsFalse(error_code);

    after = detail::TransitionExpectPayload(
        detail::ClassifyState::ExpectPayload, good_request_layer_data_frame, good_request_headers, non_empty_layer_data,
        message_data, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ExpectEndOfFrame == after);
    UT_AssertIsFalse(message_data.data.empty());
    UT_AssertIsFalse(error_code);
}

static void TransitionExpectPayload_MalformedFrame()
{
    MessageData message_data;
    boost::optional<ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectPayload(
        detail::ClassifyState::ExpectPayload, empty_frame, good_request_headers, empty_layer_data, message_data,
        error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(message_data.data.empty());
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);

    after = detail::TransitionExpectPayload(
        detail::ClassifyState::ExpectPayload, short_request_frame, good_request_headers, empty_layer_data, message_data,
        error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(message_data.data.empty());
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);

    after = detail::TransitionExpectPayload(
        detail::ClassifyState::ExpectPayload, good_request_layer_data_frame, good_request_headers, empty_layer_data,
        message_data, error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(message_data.data.empty());
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);

    after = detail::TransitionExpectPayload(
        detail::ClassifyState::ExpectPayload, good_request_frame, good_request_headers, non_empty_layer_data,
        message_data, error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(message_data.data.empty());
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);
}

static void TransitionExpectEndOfFrame_Valid()
{
    boost::optional<ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectEndOfFrame(
        detail::ClassifyState::ExpectEndOfFrame, good_request_frame, empty_layer_data, error_code);
    UT_AssertIsTrue(detail::ClassifyState::FrameComplete == after);
    UT_AssertIsFalse(error_code);

    after = detail::TransitionExpectEndOfFrame(
        detail::ClassifyState::ExpectEndOfFrame, good_request_layer_data_frame, non_empty_layer_data, error_code);
    UT_AssertIsTrue(detail::ClassifyState::FrameComplete == after);
    UT_AssertIsFalse(error_code);
}

static void TransitionExpectEndOfFrame_MalformedFrame()
{
    boost::optional<ProtocolErrorCode> error_code;

    auto after = detail::TransitionExpectEndOfFrame(
        detail::ClassifyState::ExpectEndOfFrame, double_payload_request_frame, empty_layer_data, error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);
}

static void TransitionFrameComplete_Valid()
{
    boost::optional<ProtocolErrorCode> error_code;

    auto after = detail::TransitionFrameComplete(
        detail::ClassifyState::FrameComplete, good_request_headers, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ValidFrame == after);
    UT_AssertIsFalse(error_code);

    after = detail::TransitionFrameComplete(
        detail::ClassifyState::FrameComplete, good_response_headers, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ValidFrame == after);
    UT_AssertIsFalse(error_code);

    after = detail::TransitionFrameComplete(
        detail::ClassifyState::FrameComplete, good_event_headers, error_code);
    UT_AssertIsTrue(detail::ClassifyState::ValidFrame == after);
    UT_AssertIsFalse(error_code);
}

static void TransitionFrameComplete_MalformedFrame()
{
    boost::optional<ProtocolErrorCode> error_code;

    auto after = detail::TransitionFrameComplete(
        detail::ClassifyState::FrameComplete, unknown_type_headers, error_code);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(ProtocolErrorCode::NOT_SUPPORTED == error_code);
}

static void TransitionValidFrame_Valid()
{
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionValidFrame(detail::ClassifyState::ValidFrame, good_request_headers, disposition);
    UT_AssertIsTrue(detail::ClassifyState::ClassifiedValidFrame == after);
    UT_AssertIsTrue(detail::FrameDisposition::DeliverRequestToService == disposition);

    disposition = detail::FrameDisposition::Indeterminate;

    after = detail::TransitionValidFrame(detail::ClassifyState::ValidFrame, good_response_headers, disposition);
    UT_AssertIsTrue(detail::ClassifyState::ClassifiedValidFrame == after);
    UT_AssertIsTrue(detail::FrameDisposition::DeliverResponseToProxy == disposition);

    disposition = detail::FrameDisposition::Indeterminate;

    after = detail::TransitionValidFrame(detail::ClassifyState::ValidFrame, good_event_headers, disposition);
    UT_AssertIsTrue(detail::ClassifyState::ClassifiedValidFrame == after);
    UT_AssertIsTrue(detail::FrameDisposition::DeliverEventToService == disposition);
}

static void TransitionExpectConfig_Valid()
{
    boost::optional<ProtocolErrorCode> error_code;
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionExpectConfig(
        detail::ClassifyState::ExpectConfig, config_frame, error_code, disposition);
    UT_AssertIsTrue(detail::ClassifyState::ClassifiedValidFrame == after);
    UT_AssertIsTrue(detail::FrameDisposition::ProcessConfig == disposition);
    UT_AssertIsFalse(error_code);
}

static void TransitionExpectConfig_MalformedFrame()
{
    boost::optional<ProtocolErrorCode> error_code;
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionExpectConfig(
        detail::ClassifyState::ExpectConfig, config_frame_extra, error_code, disposition);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);
    UT_AssertIsTrue(detail::FrameDisposition::Indeterminate == disposition);
}

static void TransitionExpectConfig_MalformedConfigData()
{
    boost::optional<ProtocolErrorCode> error_code;
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionExpectConfig(
        detail::ClassifyState::ExpectConfig, config_frame_bad_config_data, error_code, disposition);
    UT_AssertIsTrue(detail::ClassifyState::MalformedFrame == after);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == error_code);
    UT_AssertIsTrue(detail::FrameDisposition::Indeterminate == disposition);
}

static void TransitionExpectConfig_InvalidPreconditions()
{
    boost::optional<ProtocolErrorCode> error_code;
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionExpectConfig(
        detail::ClassifyState::ExpectConfig, empty_frame, error_code, disposition);
    UT_AssertIsTrue(detail::ClassifyState::InternalStateError == after);
    UT_AssertIsTrue(detail::FrameDisposition::Indeterminate == disposition);
    UT_AssertIsFalse(error_code);

    // Non-empty, non-config frame.
    after = detail::TransitionExpectConfig(
        detail::ClassifyState::ExpectConfig, protocol_error_frame, error_code, disposition);
    UT_AssertIsTrue(detail::ClassifyState::InternalStateError == after);
    UT_AssertIsTrue(detail::FrameDisposition::Indeterminate == disposition);
    UT_AssertIsFalse(error_code);
}

static void TransitionExpectProtocolError_Valid()
{
    unique_ptr<epoxy::ProtocolError> error = nullptr;
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionExpectProtocolError(
        detail::ClassifyState::ExpectProtocolError, protocol_error_frame, error, disposition);
    UT_AssertIsTrue(detail::ClassifyState::ClassifiedValidFrame == after);
    UT_AssertIsNotNull(error);
    UT_AssertIsTrue(meaningless_error_code == error->error_code);
    UT_AssertIsTrue(detail::FrameDisposition::HandleProtocolError == disposition);
}

static void TransitionExpectProtocolError_ErrorInError()
{
    unique_ptr<epoxy::ProtocolError> error = nullptr;
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionExpectProtocolError(
        detail::ClassifyState::ExpectProtocolError, double_protocol_error_frame, error, disposition);
    UT_AssertIsTrue(detail::ClassifyState::ErrorInErrorFrame == after);
    UT_AssertIsNull(error);
    UT_AssertIsTrue(detail::FrameDisposition::Indeterminate == disposition);
}

static void TransitionExpectProtocolError_InvalidPreconditions()
{
    unique_ptr<epoxy::ProtocolError> error = nullptr;
    auto disposition = detail::FrameDisposition::Indeterminate;

    auto after = detail::TransitionExpectProtocolError(
        detail::ClassifyState::ExpectProtocolError, good_request_frame, error, disposition);
    UT_AssertIsTrue(detail::ClassifyState::InternalStateError == after);
    UT_AssertIsNull(error);
    UT_AssertIsTrue(detail::FrameDisposition::Indeterminate == disposition);
}

// These end-to-end tests cover states that don't fit in functions

static void AssertHeadersEqual(const EpoxyHeaders& expected, const EpoxyHeaders& actual)
{
    UT_AssertIsTrue(expected.service_name == actual.service_name);
    UT_AssertIsTrue(expected.method_name == actual.method_name);
    UT_AssertIsTrue(expected.message_type == actual.message_type);
    UT_AssertIsTrue(expected.conversation_id == actual.conversation_id);
}

static void Classify_Valid()
{
    {
        auto request_result = detail::Classify(good_request_frame);
        UT_AssertIsTrue(detail::FrameDisposition::DeliverRequestToService == request_result->disposition);
        AssertHeadersEqual(*good_request_headers, *request_result->headers);
        UT_AssertIsTrue(request_result->layer_data.empty());
        UT_AssertIsFalse(request_result->message_data.is_error);
        UT_AssertIsTrue(good_request_frame.framelets[good_request_frame.framelets.size() - 1].second
            == request_result->message_data.data[0]);
        UT_AssertIsNull(request_result->error);
        UT_AssertIsFalse(request_result->error_code);
    }

    {
        auto request_layer_result = detail::Classify(good_request_layer_data_frame);
        UT_AssertIsTrue(detail::FrameDisposition::DeliverRequestToService == request_layer_result->disposition);
        AssertHeadersEqual(*good_request_headers, *request_layer_result->headers);
        UT_AssertIsFalse(request_layer_result->layer_data.empty());
        UT_AssertIsFalse(request_layer_result->message_data.is_error);
        UT_AssertIsTrue(good_request_layer_data_frame.framelets[good_request_layer_data_frame.framelets.size() - 1].second
            == request_layer_result->message_data.data[0]);
        UT_AssertIsNull(request_layer_result->error);
        UT_AssertIsFalse(request_layer_result->error_code);
    }

    {
        auto response_result = detail::Classify(good_response_frame);
        UT_AssertIsTrue(detail::FrameDisposition::DeliverResponseToProxy == response_result->disposition);
        AssertHeadersEqual(*good_response_headers, *response_result->headers);
        UT_AssertIsTrue(response_result->layer_data.empty());
        UT_AssertIsFalse(response_result->message_data.is_error);
        UT_AssertIsTrue(good_response_frame.framelets[good_response_frame.framelets.size() - 1].second
            == response_result->message_data.data[0]);
        UT_AssertIsNull(response_result->error);
        UT_AssertIsFalse(response_result->error_code);
    }

    {
        auto error_response_result = detail::Classify(good_error_response_frame);
        UT_AssertIsTrue(detail::FrameDisposition::DeliverResponseToProxy == error_response_result->disposition);
        AssertHeadersEqual(*good_response_headers, *error_response_result->headers);
        UT_AssertIsTrue(error_response_result->layer_data.empty());
        UT_AssertIsTrue(error_response_result->message_data.is_error);
        UT_AssertIsTrue(good_error_response_frame.framelets[good_response_frame.framelets.size() - 1].second
            == error_response_result->message_data.data[0]);
        UT_AssertIsNull(error_response_result->error);
        UT_AssertIsFalse(error_response_result->error_code);
    }

    {
        auto event_result = detail::Classify(good_event_frame);
        UT_AssertIsTrue(detail::FrameDisposition::DeliverEventToService == event_result->disposition);
        AssertHeadersEqual(*good_event_headers, *event_result->headers);
        UT_AssertIsTrue(event_result->layer_data.empty());
        UT_AssertIsFalse(event_result->message_data.is_error);
        UT_AssertIsTrue(good_event_frame.framelets[good_event_frame.framelets.size() - 1].second
            == event_result->message_data.data[0]);
        UT_AssertIsNull(event_result->error);
        UT_AssertIsFalse(event_result->error_code);
    }

    {
        auto config_result = detail::Classify(config_frame);
        UT_AssertIsTrue(detail::FrameDisposition::ProcessConfig == config_result->disposition);
        UT_AssertIsNull(config_result->headers);
        UT_AssertIsTrue(config_result->layer_data.empty());
        UT_AssertIsTrue(config_result->message_data.data.empty());
        UT_AssertIsNull(config_result->error);
        UT_AssertIsFalse(config_result->error_code);
    }

    {
        auto protocol_error_result = detail::Classify(protocol_error_frame);
        UT_AssertIsTrue(detail::FrameDisposition::HandleProtocolError == protocol_error_result->disposition);
        UT_AssertIsNull(protocol_error_result->headers);
        UT_AssertIsTrue(protocol_error_result->layer_data.empty());
        UT_AssertIsTrue(protocol_error_result->message_data.data.empty());
        UT_AssertIsTrue(meaningless_error_code == protocol_error_result->error->error_code);
        UT_AssertIsFalse(protocol_error_result->error_code);
    }

    {
        auto double_protocol_error_result = detail::Classify(double_protocol_error_frame);
        UT_AssertIsTrue(detail::FrameDisposition::HangUp == double_protocol_error_result->disposition);
        UT_AssertIsNull(double_protocol_error_result->headers);
        UT_AssertIsTrue(double_protocol_error_result->layer_data.empty());
        UT_AssertIsTrue(double_protocol_error_result->message_data.data.empty());
        UT_AssertIsTrue(ProtocolErrorCode::ERROR_IN_ERROR == double_protocol_error_result->error->error_code);
        UT_AssertIsFalse(double_protocol_error_result->error_code);
    }
}

static void Classify_MalformedFrame()
{
    auto empty_result = detail::Classify(empty_frame);
    UT_AssertIsTrue(detail::FrameDisposition::SendProtocolError == empty_result->disposition);
    UT_AssertIsNull(empty_result->headers);
    UT_AssertIsTrue(empty_result->layer_data.empty());
    UT_AssertIsTrue(empty_result->message_data.data.empty());
    UT_AssertIsNull(empty_result->error);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == empty_result->error_code);

    auto short_result = detail::Classify(short_request_frame);
    UT_AssertIsTrue(detail::FrameDisposition::SendProtocolError == short_result->disposition);
    UT_AssertIsNull(short_result->headers);
    UT_AssertIsTrue(short_result->layer_data.empty());
    UT_AssertIsTrue(short_result->message_data.data.empty());
    UT_AssertIsNull(short_result->error);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == short_result->error_code);

    auto double_headers_result = detail::Classify(double_headers_request_frame);
    UT_AssertIsTrue(detail::FrameDisposition::SendProtocolError == double_headers_result->disposition);
    UT_AssertIsNull(double_headers_result->headers);
    UT_AssertIsTrue(double_headers_result->layer_data.empty());
    UT_AssertIsTrue(double_headers_result->message_data.data.empty());
    UT_AssertIsNull(double_headers_result->error);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == double_headers_result->error_code);

    auto headers_config_result = detail::Classify(headers_config_request_frame);
    UT_AssertIsTrue(detail::FrameDisposition::SendProtocolError == headers_config_result->disposition);
    UT_AssertIsNull(headers_config_result->headers);
    UT_AssertIsTrue(headers_config_result->layer_data.empty());
    UT_AssertIsTrue(headers_config_result->message_data.data.empty());
    UT_AssertIsNull(headers_config_result->error);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == headers_config_result->error_code);

    auto double_payload_result = detail::Classify(double_payload_request_frame);
    UT_AssertIsTrue(detail::FrameDisposition::SendProtocolError == double_payload_result->disposition);
    UT_AssertIsNull(double_payload_result->headers);
    UT_AssertIsTrue(double_payload_result->layer_data.empty());
    UT_AssertIsTrue(double_payload_result->message_data.data.empty());
    UT_AssertIsNull(double_payload_result->error);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == double_payload_result->error_code);

    auto backwards_result = detail::Classify(backwards_request_frame);
    UT_AssertIsTrue(detail::FrameDisposition::SendProtocolError == backwards_result->disposition);
    UT_AssertIsNull(backwards_result->headers);
    UT_AssertIsTrue(backwards_result->layer_data.empty());
    UT_AssertIsTrue(backwards_result->message_data.data.empty());
    UT_AssertIsNull(backwards_result->error);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == backwards_result->error_code);

    auto config_extra_result = detail::Classify(config_frame_extra);
    UT_AssertIsTrue(detail::FrameDisposition::SendProtocolError == config_extra_result->disposition);
    UT_AssertIsNull(config_extra_result->headers);
    UT_AssertIsTrue(config_extra_result->layer_data.empty());
    UT_AssertIsTrue(config_extra_result->message_data.data.empty());
    UT_AssertIsNull(config_extra_result->error);
    UT_AssertIsTrue(ProtocolErrorCode::MALFORMED_DATA == config_extra_result->error_code);
}

} } // namespace comm.epoxy
}   // namespace bond


using namespace bond::comm::epoxy;

bool init_unit_test()
{
    CreateFramelets();
    CreateFrames();

    UnitTestSuite suite("epoxy_protocol");
    suite.AddTestCase(TransitionExpectFirstFramelet_Valid, "TransitionExpectFirstFramelet_Valid");
    suite.AddTestCase(TransitionExpectFirstFramelet_MalformedFrame, "TransitionExpectFirstFramelet_MalformedFrame");
    suite.AddTestCase(TransitionExpectEpoxyHeaders_Valid, "TransitionExpectEpoxyHeaders_Valid");
    suite.AddTestCase(TransitionExpectEpoxyHeaders_InvalidPreconditions, "TransitionExpectEpoxyHeaders_InvalidPreconditions");
    suite.AddTestCase(TransitionOptionalExpectLayerData_Valid, "TransitionOptionalExpectLayerData_Valid");
    suite.AddTestCase(TransitionExpectOptionalLayerData_MalformedFrame, "TransitionExpectOptionalLayerData_MalformedFrame");
    suite.AddTestCase(TransitionExpectPayload_Valid, "TransitionExpectPayload_Valid");
    suite.AddTestCase(TransitionExpectPayload_MalformedFrame, "TransitionExpectPayload_MalformedFrame");
    suite.AddTestCase(TransitionExpectEndOfFrame_Valid, "TransitionExpectEndOfFrame_Valid");
    suite.AddTestCase(TransitionExpectEndOfFrame_MalformedFrame, "TransitionExpectEndOfFrame_MalformedFrame");
    suite.AddTestCase(TransitionFrameComplete_Valid, "TransitionFrameComplete_Valid");
    suite.AddTestCase(TransitionFrameComplete_MalformedFrame, "TransitionFrameComplete_MalformedFrame");
    suite.AddTestCase(TransitionValidFrame_Valid, "TransitionValidFrame_Valid");
    suite.AddTestCase(TransitionExpectConfig_Valid, "TransitionExpectConfig_Valid");
    suite.AddTestCase(TransitionExpectConfig_MalformedFrame, "TransitionExpectConfig_MalformedFrame");
    suite.AddTestCase(TransitionExpectConfig_MalformedConfigData, "TransitionExpectConfig_MalformedConfigData");
    suite.AddTestCase(TransitionExpectConfig_InvalidPreconditions, "TransitionExpectConfig_InvalidPreconditions");
    suite.AddTestCase(TransitionExpectProtocolError_Valid, "TransitionExpectProtocolError_Valid");
    suite.AddTestCase(TransitionExpectProtocolError_ErrorInError, "TransitionExpectProtocolError_ErrorInError");
    suite.AddTestCase(TransitionExpectProtocolError_InvalidPreconditions, "TransitionExpectProtocolError_InvalidPreconditions");
    suite.AddTestCase(Classify_Valid, "Classify_Valid");
    suite.AddTestCase(Classify_MalformedFrame, "Classify_MalformedFrame");

    return true;
}
