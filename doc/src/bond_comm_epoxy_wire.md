% Bond Epoxy transport wire format

# Epoxy transport wire format #

This documents the [Bond Epoxy transport](bond_comm_epoxy.html) wire format.
For API-level documentation, see the respective language guides.

The intended audience for this is transport implementers so that compatible
clients/servers can be produced.

# Requirements #

Since Bond transports operate on messages, the Epoxy transport needs a way
to group sequences of bytes within the stream of bytes provided by a TCP
connection into individual messages. It does this by "framing" each message.
Thus, this wire format turns a stream of bytes into a sequence of _frames_.

These frames are then used to transmit Bond messages between participants.
The messages are decomposed into simple tuples. These tuples are used to
implement the [messaging patterns](bond_comm.html#messaging-patterns) this
transport supports.

The Epoxy transport wire format should be [simple](#example), so that it is
easy to implement--at least the basic feature set.

The Epoxy transport defers to Bond serialization/marshalling when it can.
Instead of re-implementing backward/forward compatible serialization
schemes, it uses Bond. For any structures that are used as part of the
protocol itself, the [Fast Binary](bond_cpp.html#fast-binary) protocol
version 1 is used to serialize them, as this protocol is simple to
implement. Payloads and errors are marshalled as Compact Binary v1, as this
protocol provides a good trade-off between size and
serialization/deserialization time.

"Interesting" structures need to be able to be allocated and
serialized/deserialized independently. For example, a Bond Epoxy transport
proxy that only wants to inspect headers should not be required to
deserialize entire messages.

# Conventions #

The following hold in this document, unless otherwise specified.

* All sizes are given in bytes.
* All multi-byte quantities are little-endian.

## Terminology ##

* _**client**_: an entity that establishes connections to _servers_. In
  Berkeley socket parlance, this entity calls `connect()`
* _**connection**_: a transmision channel capable of bi-directional
  communication
* _**conversation**_: one message-based interaction between two _endpoints_.
  This includes things like the request/response and event
  [messaging patterns](bond_comm.html#messaging-patterns)
* _**endpoint**_: either the _client_ or the _server_
* _**frame**_: a collection of related data, all of which are transmitted
  together
* _**framelet**_: a type, length, data tuple within a _frame_
* _**headers**_: metadata about a conversation
* _**initiator**_: the entity that starts a conversation
* _**layer data**_: opaque blob of data used by Bond Communications' layers
  facility
* _**message**_: either _payload data_ or _error data_
* _**payload data**_: opaque blob of data that is the contents of the
  message being sent when the message contains the user's payload type
* _**error data**_: opaque blob of data that is the contents of the message
  being sent when the message contains an error
* _**receiver**_: the endpoint that is reading _frames_, regardless of the
  higher-level messaging pattern being employed
* _**sender**_: the endpoint originating _frames_, regardless of the
  higher-level messaging pattern being employed
* _**server**_: an entity listening for connections from _clients_,
  typically on a well-known port. In Berkeley socket parlance, this entity
  calls `bind()`, `listen()`, and `accept()`
* _**service**_: a Bond entity that exposes invokable methods that may take
  and return Bond structures

# The frame #

A frame is the indivisible unit of transmission in the Epoxy transport.

Each frame is an ordered collection of type, length, data tuples. These
tuples are called _framelets_.

The basic structure of a frame is:

* the count of the framelets in the frame (2 bytes)
* framelet 1
* ...
* framelet _n_

The framelet count must be greater than 0 and less than 65,535.

The first framelet in the frame determines the type of the frame. It
dictates which subsequent framelets (after the first) are allowed and in
which order. Thus, the order of the framelets within a frame must be
preserved by both the sender and the receiver.

The possible frame types are:

| Frame type | First framelet |
|------------|----------------|
| [Config frame](#establishing-a-connection) | `EpoxyConfig` |
| [Message frame](#exchanging-messages) | `EpoxyHeaders` |
| [Error frame](#protocol-error) | `ProtocolError` |

Each frame type is described in more details in its own section.

See also [Limits](#limits) for a discussion about things like maximum frame
size.

    +----0---+----1---+================+
    | framelet count  | framelets  ... |
    +--------+--------+================+

## Framelets ##

A framelet is an entity within a frame. It consists of:

* the [framelet type](#framelet-types) (2 bytes)
* the framelet content size (4 bytes, but see [Limits](#limits).)
* the content itself

How the content of a framelet is interpreted depends on the type.

    +----0---+----1---+----2---+----3---+----4---+----5---+============+
    | framelet type   | framelet size                     | content    |
    +--------+--------+--------+--------+--------+--------+============+

## Framelet types ##

Each framelet has a unique type ID, regardless of which frame its is permitted to occur in.

| Framelet | Type ID | Parent frame | Required | Encoding | Contents |
|----------|---------|--------------|----------|----------|----------|
| `EpoxyConfig` | 0x47 0x43 ("GC") | Config | Yes | Fast Binary v1 serialized | [`EpoxyConfig`](#epoxy-config-structure) structure |
| `EpoxyHeaders` | 0x52 0x48 ("RH") | Message | Yes | Fast Binary v1 serialized | [`EpoxyHeaders`](#epoxy-headers-structure) structure |
| `ErrorData` | 0x44 0x45 ("DE") | Message | One of DP or DE | Compact Binary v1 marshalled | Message error data |
| `LayerData` | 0x59 0x4C ("YL") | Message | No | Compact Binary v1 marshalled | Auxiliary data used by Bond Communications layers stack |
| `PayloadData` | 0x44 0x50 ("DP") | Message | One of DP or DE | Compact Binary v1 marshalled | Message payload data |
| `ProtocolError` | 0x52 0x45 ("RE") | Error | Yes | Fast Binary v1 serialized | [`ProtocolError`](#protocol-error) structure |

_Note_: The framelet type IDs were assigned such that they are mnemonic
strings for their contents. In this table, they are "reversed" from what
might be expected, as they are little-endian encoded. This reversal makes
them show up the "right way" when reading a frame in most hex editors.

### Unknown framelets ###

If an endpoint encounters an unknown framelet, it must send a
[protocol error](#protocol-error) with the error code `PROTOCOL_VIOLATED`
and close the connection.

Encountering an unknown framelet indicates that the handshake
[connection handshake](#establishing-a-connection) has gone catastrophically
wrong, as newer features can only be used after both endpoints have agreed
to them.

This complicates the implementation and deployment of Epoxy transport
proxies (not to be confused with the generated client-side proxy objects),
but in our experience, transport-specific proxies are extremely rare and are
only used in high constrained environments.

# Connection life cycle #

At the beginning of an Epoxy connection, an underlying transport connection
is established. This is either a
[TCP connection or a TLS connection](#secure-connections). Then, a
[two frame handshake](#establishing-a-connection) occurs. After that, the
endpoints [exchange message](#exchanging-messages) with each other.

At any point, one of the endpoints may send a
[protocol error](#protocol-error). It may then either gracefully or forcibly
close its underlying connection, ending the Epoxy connection.

When one endpoint has finished sending all its messages, it gracefully
closes its side of the underlying connection. It should then wait a
reasonable amount of time for the other side to also gracefully close its
its side of the underlying connection, ending the Epoxy connection.

# Establishing a connection #

When a connection is initially established, a two frame handshake occurs.

The client sends a frame consisting of only one `EpoxyConfig` framelet with
the [various features it is prepared to use](#epoxy-config-structure). The
client then waits for a corresponding frame to be sent by the server.

Upon accepting a connection, the server waits for the first frame from the
client. The server then inspects the `EpoxyConfig` structure and determines
whether and which features to use. The server then sends back the final
`EpoxyConfig` that will be used for this connection. If the server is
presented with a config it cannot or will not use, it must respond with a
frame containing only a `ProtocolError`framelet with error code
`CLIENT_BAD_HANDSHAKE` and close the connection.

Application should be given a chance to accept/reject a connection when it
is being established. If the connection is rejected, a `ProtocolError` with
error code `CONNECTION_REJECTED` must be sent. Then, the connection must be
closed.

If the client does not agree with the choices that the server made or if the
server committed a protocol violation (e.g., selecting an option not
proposed by the client), it may send a frame containing only a
`ProtocolError`framelet. Then it must close the connection. There is no way
to re-handshake.

## Secure connections ##

The Bond Epoxy wire format has no provisions for communications security.

Implementations of the Epoxy transport are encouraged to offer both plain
TCP and TLS connections. TLS 1.2 or greater must be used. Configuration of
the other TLS parameters (e.g., allowed cipher suites) should be deferred to
the application/system administrator.

When using TLS to secure Epoxy, the TLS connection is established before the
Epoxy [handshake](#establishing-a-connection) occurs.

When applications are given a chance to accept/reject a connection,
certificate details should be made available to them.

Authorization is an application-level problem, and applications will need to
incorporate their authorization requirements into their service definitions.
Layer data is a good place to store information that is used across all
services and methods on the same connection.

## Recommended port ##

The recommended port for the Bond Epoxy transport is 25188 (ASCII "bd").

If TLS connections are being used, the recommended port is 25156 (ASCII
"bD").

# Exchanging messages #

A Message frame is used to send a message between endpoints. It starts with
an `EpoxyHeaders` framelet. The required order of the framelets is:

1. `EpoxyHeaders`
1. optional `LayerData`
1. one of `PayloadData` or `ErrorData`

This order allows for clients and servers to start processing the frame as
it arrives.

# Protocol Error #

An Error frame consisting of just a `ProtocolError` framelet is used to signal
critical transport errors from which the transport cannot recover. Upon
receipt of a `ProtocolError` framelet, the connection transitions to an
error state and must be closed. All subsequent frames may be discarded.

The contents of a `ProtocolError` framelet is a `ProtocolError` struct,
serialized in Bond Fast Binary v1.

# Framelet structures #

Within the Epoxy transport wire format, the following Bond structures are
used for schematized data, especially when backward/forward compatibility is
needed, since Bond already has a rich system for dealing with such data.

## Epoxy Config Structure ##

The `EpoxyConfig` structure is used to configure the Epoxy protocol during
the [initial handshake](#establishing-a-connection).

It is currently empty, but as new features are added, fields to
enable/disable their use will be added to this structure.

    namespace bond.comm.epoxy;

    struct EpoxyConfig
    {
    }

## Epoxy Headers Structure ##

The `EpoxyHeaders` structure is used for metadata needed to implement the
various messaging patterns.

    namespace bond.comm.epoxy;

    enum EpoxyMessageType
    {
        REQUEST = 1;
        RESPONSE = 2;
        EVENT = 3;
    }

    struct EpoxyHeaders
    {
        0: uint64 conversation_id;
        1: required EpoxyMessageType message_type;
        2: string service_name;
        3: string method_name;
    }

### Conversation ID ###

Conversation IDs are used to identify _conversations_. Messaging patterns
like request/response use conversation IDs to associate the request with the
response, as many requests and responses are simultaneously in flight over
the same connection. Other patterns, like event, just use the conversation
ID for diagnostic purposes (e.g., to correlate client- and server-side
logs.)

When a sender initiates a new conversation, it must generate a new
conversation ID unique to the connection. If the receiver needs to refer to
the same conversation, it must use the same conversation ID the sender
assigned.

Frames with unexpected conversation IDs may be dropped. (E.g., if a client
initiated a request/response but gave up before the server sent the
response, the client is permitted to ignore the response.)

A client may only use odd-numbered conversation IDs; a server may only use
even-numbered conversation IDs. There are no reserved conversation IDs
otherwise.

Conversation IDs should be assigned in increasing order to aid in debugging,
but endpoints are free to use whatever scheme they like. Conversation IDs
should not be reused for a different conversation during the lifetime of a
connection, because, as stated above, unexpected conversation IDs may be
dropped.

Endpoints must not rely on comparing IDs across conversations for causality.
That is, there is no guarantee that conversation 8 happened before
conversation 10. For some messaging patterns, there is causality within the
same conversation. For example, endpoints _can_ assume, that the response
for conversation 7 happened after the request for conversation 7 was sent,
received, and processed.

#### Conversation ID exhaustion ####

When either the client or the server has exhausted all of its conversation
IDs, it must send an [Error frame](#protocol-error) with the error code
`CONVERSATION_IDS_EXHAUSTED`. This will allow the connection to be
re-established, resetting the conversation IDs.

_Design Note_: Re-establishing the connection is a simple way to handle
conversation ID exhaustion. Rollover schemes are complicated to specify,
implement, debug, and test. Additionally, a single high-volume endpoint
sending 1,000,000,000 conversations per second will be able to use the same
connection for over 580 years before exhausting its conversation IDs. In
practice, the `CONVERSATION_IDS_EXHAUSTED` error should never be
encountered.

### Service name ###

The `serice_name` field is the name of the service that is being invoked.

The `service_name` field must be set to a UTF-8 encoded string (without a
BOM) that is the fully-qualified service name. Namespace elements are separated
by the period character ('.', ASCII 0x2E). May be empty string if the message
type does not require a service name.

Example: `root_namespace.child_namespace.some_service`

### Method name ###

The `method_name` field is the name of the method on the service that is being
invoked. It is also a UTF-8 encoded string (without a BOM). May be empty string
if the message type does not require a method name.

Example: `SomeMethod`

### Message type ###

The `EpoxyMessageType` indicates what kind of message pattern is being
employed:

* `Request`: initiates a conversation that expects a response. The message
  contains a `PayloadData` framelet that is a Bond structure of the expected
  request type for the method being invoked.
* `Response`: completes the conversation with a response to a previous
  request. The message either contains a `PayloadData` framelet with the
  serialized form of the expected Bond response type for the method invoked
  or it contains an `ErrorData` framelet with a a Bond structure derived
  from `Error`.
* `Event`: initiates and completes a one-way, best-effort conversation. No
  response is allowed, not even an error response. The message contains a
  `PayloadData` framelet that is a Bond structure of the expected request
  type for the method being invoked.

Notice that the `ErrorData` framelet only makes sense for a `Response`.
Implementations will need to be robust against senders that have a mismatch
here.

Implementations must also be robust against type mismatches for
`PayloadData` and `ErrorData` framelets.

## `ProtocolError` struct ##

    struct ProtocolError
    {
        0: ProtocolErrorCode error_code;
        1: nullable<bonded<bond.comm.Error>> details;
    }

The `error_code` field indicates the reason for the protocol error.

Here is the current list of protocol errors.

    enum ProtocolErrorCode
    {
        // An unknown error has occurred.
        GENERIC_ERROR = 0;
        // The endpoint has suffered a catastrophic, unrecoverable error.
        INTERNAL_ERROR = 1;
        // The client attempted to communicate in a way the server does not support.
        NOT_SUPPORTED = 2;
        // The endpoint has detected a violation of the protocol and does
        // not have a more specific error to use.
        PROTOCOL_VIOLATED = 3;
        // Some data that the Epoxy transport needed to process, like the
        // EpoxyHeaders structure, was malformed.
        MALFORMED_DATA = 4;
        // Some limit has been exceeded.
        LIMIT_EXCEEDED = 5;
        // The set of config options given to the server were unacceptable.
        CLIENT_BAD_HANDSHAKE = 6;
        // The client detected that the server violated the handshake
        // protocol (e.g., selected an option outside the client's set.)
        SERVER_BAD_HANDSHAKE = 7;
        // An invalid conversation ID was used (e.g., client used even)
        BAD_CONVERSATION_ID = 8;
        // The endpoint is out of conversation IDs.
        CONVERSATION_IDS_EXHAUSTED = 9;
        // The endpoint gave up waiting for data (e.g., initial handshake,
        // reading a large framelet)
        TIMEOUT = 10;

        // The connection was explicitly rejected.
        CONNECTION_REJECTED = 11;

        // Indicates that the endpoint received a ProtocolError that was malformed.
        // This should not be sent over the network, and should only be created by
        // a Proxy/Service in place of the malformed ProtocolError it refers to.
        ERROR_IN_ERROR = 0xffff;
    }

# Limits #

In order to protect resources, implementations may impose reasonable limits
on:

* the number of framelets in a frame (must support at least 4 and should
  support up to 16)
* the size of an individual framelet (must support at least 512 bytes and
  should support up to 32 MiB)
* the total size of a frame (must support at least 2 KiB and should support
  up to 32 MiB)

The `LIMIT_EXCEEDED` protocol error should be used when a limit has been
exceeded.

Endpoints must accept the full range of
[conversation IDs](#conversation-id), but may use a limited range
themselves.

# Example #

A simple service that can perform basic arithmetic is used in this example.

    namespace examples.calc

    enum Operation
    {
        Add,
        Div,
        Sub,
        Mul,
    };

    struct Params
    {
        1: int32   x;
        2: int32   y;
        3: Operation operation = Add;
    };

    struct Result
    {
        1: int32 z;
    };

    service Calc
    {
        Result Calculate(Params);
    };

Say we have a client that just connected to a server hosting this service
and it would like to multiply the numbers 67 and 87:

    let doSomeMathRequest = make examples.calc.Params x:67 operation:Mul y:87.

The `EpoxyHeaders` for this request would be

    let doSomeMathHeaders = make
        EpoxyHeaders
        conversation_id:3
        message_type:Request
        service_name:"examples.calc.Calc"
        method_name:"Calculate".

These two structures would then be marshalled and serialized, respectively,
and put into `PayloadData` and `EpoxyHeaders` frames.

The request frame is:

    TODO: add hex dump after stabilizing representation

The frame is presented in hex encoding here. Offsets within the frame are in
the left-most column, and the ASCII representation of each byte is in the
right-most column.
<!-- This is the output of hexl-mode for you Emacs users. -->

Let's break down the distinct entities in this frame.

1. ... TODO ...

The response is similar.

    let doSomeMathResponse = make examples.calc.Params z: 5829.
    let doSomeMathResponseHeaders = make
        EpoxyHeaders
        conversation_id:3
        message_type:Response
        error_code:0.

The response frame is:

    TODO: add hex dump after stabilizing representation

It has a similar break down.

1. ... TODO ...

# Wire format evolution #

_This section may need be expanded and clarified after we actually add a new
feature for the first time._

The wire format will need to be changed over time. Here's a sketch for how
that will be done:

* Add a capability to the `EpoxyConfig` structure and use it as part of
  negotiation.
* If the feature is negotiated successfully, then subsequent frames can use
  that capability. This might mean that new framelet types are going to be
  sent or that the client/server expects that a given field in a Bond struct
  will be processed instead of ignored.

# Rejected designs #

The following designs were rejected.

## Frame/framelet padding ##

We don't expect to see any performance benefit from padding frames or
framelets to the next machine word boundary. (Also, which machine and which
word boundary?)

If performance testing reveals otherwise, we can adding a padding framelet
easily.

## Other framing schemes ##

These framing schemes were rejected:

* `(count) length length payload payload` - sender cannot stream framelets
  as it produces them
* `(count) payload delimiter payload delimiter` - don't want to deal with
  escaping of payloads
* omitting the size for fixed-width framelet types - there aren't any
  fixed-width framelet types, so this optimization adds complexity with
  little expected payoff

## Explicit frame types ##

Instead of tagging each frame with an explicit type, we use the first
framelet within each frame determine the type of the frame. Duplication
among frame types is handled by using the same structures in framelet
contents.

## Omitting event conversation IDs ##

Having a conversation ID for events is useful for debugging, and
conversation IDs are cheap to create and send. This also removes some
special casing to, say, always assign events ID 0/1.

## Conversation ID framelet ##

Conversation IDs are not in their own framelet. If they were, more errors,
like `LIMIT_EXCEEDED` and `MALFORMED_DATA`, could be turned into a normal
Bond Communication errors (`status_code` non-zero) and the connection could
remain open.

However, promoting conversation IDs to a framelet adds stuff at the lower
framing layer that the framing layer doesn't need to know about most of the
time. Protocol errors are expected to be rare, and closing the connection
simplifies implementation significantly.

## Ignoring unknown framelets ##

Ignoring unknown framelets makes backward/forward compatibility simple.
There would also be no need to upgrade things like proxies when a new
framelet type is added. However, if the unknown framelet changes the
semantic value of a the frame or of a known framelet, the endpoint may not
do the right thing.

We also expect to be able to add most features to the protocol by using Bond
backward/forward compatibility on the structures embedded within the
framelets.
