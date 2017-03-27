% Bond Epoxy transport

**IMPORTANT NOTE: Bond Comm and Epoxy are deprecated. We recommend using
[Bond-over-gRPC](bond_over_grpc.html) for communication. This documentation
is retained for transitional purposes.**

# About #

The Bond Epoxy transport is a Bond
[communications transport](bond_comm.html#transport-flexibility) that uses a
[custom binary protocol over TCP](bond_comm_epoxy_wire.html), and optionally
TLS, to pass messages.

It supports the following messaging patterns:

* request/response
* event

# Implementations #

Epoxy is available for [C#](bond_cs.html#epoxy-transport) today.
