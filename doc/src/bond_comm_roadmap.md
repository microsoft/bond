% Bond Comm Roadmap

# Roadmap

This is what we currently are working on for Bond Comm.

Note that none of these feature are guaranteed to ship.

If there's a feature you think is useful and should be on the roadmap or
would like to help implement a feature, please see the
[guidelines for contributing](https://github.com/Microsoft/bond/blob/master/CONTRIBUTING.md).

## Sort term (next few weeks)

* TLS support in C# Epoxy
* C# Epoxy performance improvements
* C++ support

## Medium term (next few months)

* additional languages, based on interest
* HTTP(S)-based transports (HTTP 1.1 or HTTP 2 TBD)

## Long term (next few years)

* additional messaging patterns
* additional transports

## Never

These are features that we never plan to implement in Bond Comm.

* Automatic polymorphic deserialization of Error responses and conversion to
  language-specific exceptions. To do so transparently and automatically,
  type information would need to be included in payloads. This is not
  something we want to require all users to include in their payloads. We
  wouldn't be opposed to providing library support to make this easier to
  opt in to, but we will not make it the default due to its cost.
