% Bond Simple In Memory (SimpleInMem) transport

# About #

The Bond SimpleInMem transport is a Bond
[communications transport](bond_comm.html#transport-flexibility) that uses a
custom binary protocol to pass messages between Bond client and server 
hosted in-memory. This is not a shared memory transport, and works only inside a single process.
SimpleInMem primarily serves two purposes:

* quickly enable customers to automate testing of Bond services
* example Bond transport

It supports the following messaging patterns:

* request/response
* event

# Implementations #

SimpleInMem is available as [C#](bond_cs.html#simpleinmem-transport) implementation today.
