# Bond Changelog #

Notable changes--especially new features and **breaking changes**--are
recorded here.

Bond's version numbers follow [Semantic Versioning](http://semver.org/).
Each release is tagged with a Git tag. The
[NuGet packages](https://www.nuget.org/packages/Bond.CSharp/) follow the Git
tag versions. The Bond compiler (`gbc`) and
[compiler library](https://hackage.haskell.org/package/bond) have a slightly
different versioning scheme, following the Haskell community's
[package versioning policy](https://wiki.haskell.org/Package_versioning_policy).

## Unreleased ##
* `gbc` & compiler library: TBD (minor bump needed)
* IDL core version: TBD
* IDL comm version: TBD
* C++ version: TBD (major bump needed)
* C# NuGet version: TBD
* C# Comm NuGet version: TBD (minor bump needed)

### C++ ###
* **Breaking change:** Suppression of MSVC name shadowing warnings is no
  longed needed, and Bond's warning.h header no longed supresses C4456,
  C4458, or C4459. This may cause these warnings to now be triggered on
  other code.

### C# Comm ###

* EpoxyListener's StopAsync() now stops all the outstanding connections that
  it accepted.
* EpoxyTransport's StopAsync() now stops all the connections and listeners
  that it created.

## 5.2.0: 2017-02-07 ##

* `gbc` & compiler library: 0.8.0.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 5.2.0
* C# NuGet version: 5.2.0
* C# Comm NuGet version: 0.10.0

### `gbc` and Bond compiler library ###

* **Breaking change:** The C++ Comm .cpp template has been renamed to
  `comm_cpp` from `types_comm_cpp` to match the file it generates.
* Add export-attribute option for C++ and make apply-attribute a
  deprecated synonym for export-attribute
* Fix C++ Comm build problems when services are shared via DLL.
  [Issue #314](https://github.com/Microsoft/bond/issues/314)

### C++ ###

* Fixed compatibility with RapidJSON v1.1.0.
  [Issue #271](https://github.com/Microsoft/bond/issues/271)
* The minimum supported version of Boost is now 1.58
* The `bf` utility now supports multiple payloads.
  [Pull request #288](https://github.com/Microsoft/bond/pull/288)
* Fixed an issue with aliased enums.
  [Pull request #288](https://github.com/Microsoft/bond/pull/298)
* Fixed an issue with template parameter deduction in `bond::is_nullable`
  that occurs with Microsoft Visual C++ 2015 Update 3.
  [Issue #306](https://github.com/Microsoft/bond/issues/306)

### C++ Comm ###
* Fixed a multiply-defined symbol linker error for
  `bond::comm::epoxy::detail::MakeConfigFrame`.

### C# ###

* Added controls to cap pre-allocation during deserialization of containers
  and blobs.
* Fixed computation of default value for aliased bool and wstring fields.
  [Issue #300](https://github.com/Microsoft/bond/issue/300)

### C# Comm ###

* Resources are now properly cleaned up if failures are encountered when
  establishing client-side Epoxy connections.
* The generated interfaces for services are now public. They were
  inadvertently internal before.

## 5.1.0: 2016-11-14 ##

* `gbc` & compiler library: 0.7.0.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 5.1.0
* C# NuGet version: 5.1.0
* C# Comm NuGet version: 0.9.0

### `gbc` and Bond compiler library ###

* **Breaking change:** The Haskell utility functions `structName` and
  `structParams` were renamed to `className` and `classParams` (in the
  `Language.Bond.Codegen.Cpp.Util` module).
* Added initial support for generating C++ Comm services and proxies.

### C++ Comm ###
* The initial C++ Comm code has been merged in, but there is still work left
  to be done before the first preview release. Use at your own risk.

### C# ###
* Bond C# now supports
  [.NET Standard 1.0, 1.3, and 1.6](https://blogs.msdn.microsoft.com/dotnet/2016/09/26/introducing-net-standard/),
  so you can use Bond in .NET Core applications.
  [Pull request #243](https://github.com/Microsoft/bond/pull/243)
    * Not all assemblies work with all versions of the .NET Standard or on
      all platforms. The
      [manual](https://microsoft.github.io/bond/manual/bond_cs.html#frameworks-targeted)
      lists which assemblies target which frameworks.
    * Bond C# Comm is not yet building with .NET Core toolchain, so its
      .NET Core support is preliminary.
* Bond.JSON now depends on Newsoft.JSON 9.0.1, the earliest version that
  supports .NET Standard 1.0.
* A new assembly, Bond.Reflection.dll, has been added, due to some internal
  refactoring needed for .NET Core support. Many of the Bond assemblies now
  have a dependency on this assembly, so you'll need to deploy it. If you
  use NuGet to consume Bond, this is should be handled automatically.
* Fixed a bug in the MSBuild targets that caused codegen to always be run if
  all of the BondCodegen items have Options metadata.
* Fixed a bug in the MSBuild targets that caused compilation to fail if the
  $BondOutputDirectory did not end with a trailing slash.

### C# Comm ###
* The constructor for `LayerStackProvider` no longer requires a logger;
  instead, the transport's logger is passed to `OnSend`/`OnReceive`. Before,
  using the same logger with a transport and `LayerStackProvider` required a
  duplicate implementation.
* Fixed a bug that prevented `EpoxyListener` from accepting multiple
  connections in parallel.

## C# Comm 0.8.0: 2016-10-12 ##

* C# Comm NuGet version: 0.8.0

### C# Comm ###
* `EpoxyTransport` can be configured to enable TCP keep-alive to help detect
  dead connections. See `EpoxyTransportBuilder.SetKeepAliveTimes` for
  details.

## 5.0.0: 2016-09-12 #

* `gbc` & compiler library: 0.6.0.0
* IDL core version: 2.0
* IDL comm version: 1.1
* C++ version: 5.0.0
* C# NuGet version: 5.0.0
* C# Comm NuGet version: 0.7.0

### IDL core ###
* **Breaking change:** `bond.TypeDef.list_sub_type` field removed, as it was
  breaking some consumers of serialized SchemaDef. We plan to restore this
  field in the future.
  [Issue #161 re-opened](https://github.com/Microsoft/bond/issues/161)

### IDL comm ###
* Update IDL to conform to naming conventions.
* Adjust IDL for changes made to Epoxy internals

### C++ ###
* **Breaking change:** Runtime SchemaDef `list_sub_type` field removed, as
  it was breaking some consumers of serialized SchemaDef. We plan to restore
  this field in the future.
  [Issue #161 re-opened](https://github.com/Microsoft/bond/issues/161)
* Generated enum types now have a `FromEnum` method that can be used to
  convert from an enum value to a string. Now generated enum types have all
  four of `ToEnum`, `FromEnum`, `ToString`, and `FromString`. (The `...Enum`
  variants return false on failure, while the `...String` variants throw.)

### C# ###
* **Breaking change:** Runtime SchemaDef `list_sub_type` field removed, as
  it was breaking some consumers of serialized SchemaDef. We plan to restore
  this field in the future.
  [Issue #161 re-opened](https://github.com/Microsoft/bond/issues/161)
* The Bond.Runtime NuGet package no longer artificially limits
  Newtonsoft.Json to versions before 10.
  [Issue #212](https://github.com/Microsoft/bond/issues/212)

### C# Comm ###
* `EpoxyListeners` can now be configured to require clients to authenticate
  themselves with a certificate. This is configured via the
  `clientCertificateRequired` parameter when creating an
  `EpoxyServerTlsConfig`.
* Internals of the Epoxy protocol cleaned up. See the
  [updated wire format specification](https://microsoft.github.io/bond/manual/bond_comm_epoxy_wire.html).

## 4.3.0: 2016-08-23 ##

* `gbc` & compiler library: 0.5.0.0
* IDL core version: 1.0 (first release)
* IDL comm version: 1.0 (first release)
* C++ version: 4.3.0
* C# NuGet version: 4.3.0
* C# Comm NuGet version: 0.6.0

### `gbc` and Bond compiler library ###

* **Breaking change:** Runtime SchemaDef now includes information about
  whether BT_LIST fields are nullable or blobs.
  [Issue #161](https://github.com/Microsoft/bond/issues/161)
* User-defined `TypeMapping`s can now be created. This makes is easier to
  implement code generation for new languages. [Pull request
  #172](https://github.com/Microsoft/bond/pull/172)
* Validate default value type mistmatches.
  [Issue #72](https://github.com/Microsoft/bond/issues/72)
  [Issue #128](https://github.com/Microsoft/bond/issues/128)
* Validate default value out-of-range values.
  [Issue #73](https://github.com/Microsoft/bond/issues/73)
* Fail when struct field has default value of `nothing`.
  [Issue #164](https://github.com/Microsoft/bond/issues/164)
* Fail when enum field doesn't have default value.
  [Issue #177](https://github.com/Microsoft/bond/issues/177)
* Validate default value of type aliases
* Generated types will used `= default` move constructors if possible. This
  results in many generated types having `noexcept` move constructors.
* Fix a bug where, if a Bond namespace contained a struct and an enum value with
  the same name, generated C++ would contain ambiguous references.
  [Issue #202](https://github.com/Microsoft/bond/issues/202)

### IDL core ###

* Set up a separate IDL directory so that IDL is independent of language
  bindings. [Pull request #219](https://github.com/Microsoft/bond/pull/219)

### IDL comm ###

* Set up a separate IDL directory so that IDL is independent of language
  bindings. Convert comm IDL files to use C++-style naming convention. [Pull
  request #219](https://github.com/Microsoft/bond/pull/219)

### C++ ###

* Improvements in the `nullable` implementation. [Pull request #174](https://github.com/Microsoft/bond/pull/174)
    * Correctly use allocator model.
    * Reduce size of `nullable` in the normal case.
    * And others
* Runtime SchemaDef now includes information about whether BT_LIST fields
  are nullable or blobs.
  [Issue #161](https://github.com/Microsoft/bond/issues/161)
* The following Bond types have (possibly conditional) `noexcept` move
  constructors: `blob`, `bonded`, `maybe`, `nullable`, `RuntimeSchema`,
  `value`.

### C# ###

* Bond can now be used with Newtonsoft.Json >= 7.0.1 and < 10
* Runtime SchemaDef now includes information about whether BT_LIST fields
  are nullable or blobs.
  [Issue #161](https://github.com/Microsoft/bond/issues/161)

### C# Comm ###

* Logging interface revamped to make it non-static, eliminate boilerplate code, and to handle
  logging-related exceptions gracefully.
    * `LogHandler` renamed to `ILogSink`.
    * `TransportBuilder.SetLogSink` added to associate a logger with a
      transport.
    * `TransportBuilder.EnableDebugLogging` added to control whether debug
      logs are emitted or not.
    * The `ILogSink.Log` method is now provided a pre-formatted string
      instead of a format string and its arguments.
* Transport is now parameterized with Connection and Listener
  implementations. This eliminates the need to cast the results of
  `transport.ConnectToAsync()` and `transport.MakeListener()` to
  transport-specific subtypes.
* Epoxy has a hook for performing custom host to IP address resolution. This
  is configured with `EpoxyTransportBuilder.SetResolver`.
* Bond-generated Errors now give clients opaque GUIDs. These GUIDs can be
  matched against emitted metrics for debugging.
* Epoxy can now be configured to use TLS to secure the connection.
    * TLS configuration is set via
      `EpoxyTransportBuilder.SetClientTlsConfig`/`EpoxyTransportBuilder.SetServerTlsConfig`.
    * See the
      [TLS example](https://github.com/Microsoft/bond/tree/master/examples/cs/comm/tls/)
      for even more details.

## 4.2.1: 2016-06-02 ##

* `gbc` & compiler library: 0.4.1.0
* C# NuGet version: 4.2.1
* C# Comm NuGet version: 0.5.0

### `gbc` ###

* Support for parsing
  [service definitions](https://microsoft.github.io/bond/manual/compiler.html#service-definition)
  and generating C# service bases, interfaces, and proxies.
    * New types for
      [services](https://microsoft.github.io/bond/manual/compiler.html#services)
      and
      [methods](https://microsoft.github.io/bond/manual/compiler.html#methods)
      have been added to the compiler AST.
* MSBuild-compatible error messages.
  [Issue #136](https://github.com/Microsoft/bond/issues/136)

### C# ###

* Added
  [`Deserializer.TryDeserialize()`](https://github.com/Microsoft/bond/blob/db315adaf4b812adc5ca484b1bcffafe1df6d351/cs/src/core/Deserializer.cs#L231-239).
* Added two new
  [NuGet packages](https://microsoft.github.io/bond/manual/bond_cs.html#nuget-packages)
  to make it easier to consume Bond piecemeal.
    * Bond.Compiler: contains `gbc`, `bond.bond`, and `bond_const.bond` in a
      tools-only package
    * Bond.Compiler.CSharp: contains `gbc` and C# MSBuild targets. No longer
      do you have to consume Bond.CSharp (which pulls in all of the rest of
      Bond) just to get codegen.

### C# Comm ###

* Initial preview release of the
  [Bond Communications framework](https://microsoft.github.io/bond/manual/bond_comm.html).

## 4.2.0: 2016-04-28 ##

* `gbc` & compiler library: 0.4.0.2
* C# NuGet version: 4.2.0

### C# ###

* Add support for Compact Binary v2 writing.
  [Issue #70](https://github.com/Microsoft/bond/issues/70)

## 4.1.0: 2016-04-22

* `gbc` & compiler library: 0.4.0.2
* C# NuGet version: 4.1.0

### `gbc` ###

* Field ordinals larger than 65535 are now rejected.
  [Issue #111](https://github.com/Microsoft/bond/issues/111)
* Fields that duplicate the name of an existing field are now rejected.
  [Issue #123](https://github.com/Microsoft/bond/issues/123)
* The generated C# code now compiles with no errors or warnings at
  `/warn:4`. [Issue #82](https://github.com/Microsoft/bond/issues/82)
* Added
  [Visual Studio Code highlighting rules for `.bond` files](https://github.com/Microsoft/bond/tree/b2b9cd7256286fd484444dfaf7645d380a3ee936/tools/syntax/VsCode).

### C++ ###

* Enums are now cast to 32-bit integers to avoid some compiler warnings.
* Bond can be used in code bases where there is a function-style macro named
  `U`.

### C# ###

* The generated C# code now compiles with no errors or warnings at
  `/warn:4`. [Issue #82](https://github.com/Microsoft/bond/issues/82)
* Bond-generated enums constants are now cast to 32-bit integers to avoid
  some compiler warnings.
* [Implicit conversion between `bond.GUID` and `System.Guid`](https://github.com/Microsoft/bond/blob/bc4c56a3ca0858f4bd93916e80ceff9bbeada606/cs/test/core/GuidConversionTests.cs#L14)
  has been added.
  [Pull request #145](https://github.com/Microsoft/bond/pull/145)
* The ability to
  [customize the implementation of `IBonded<T>` used](https://microsoft.github.io/bond/manual/bond_cs.html#understanding-bondedt)
  has been added.
  [Pull request #153](https://github.com/Microsoft/bond/pull/153)

## 4.0.2: 2015-12-14

* `gbc` & compiler library: 0.4.0.1
* C# NuGet version: 4.0.2

### Bond compiler library ###

* Added a
  [custom codegen example](https://github.com/Microsoft/bond/tree/master/examples/codegen/read-only).

### C# ###

* Added support to
  [not inline nested structs in `Serializers`](https://github.com/Microsoft/bond/blob/cb95fdb3e1e10c3e4cae2f2d55e2b116041010a8/cs/src/core/Serializer.cs#L69).
  This can be used to speed up the time to create the serializer for very
  complex schemas.
* Fix for rare buffer corruption in InputStream.
  [Issue #114](https://github.com/Microsoft/bond/issues/114).
* Fix for SimpleXmlParser not handling XML declarations. [Issue #112](https://github.com/Microsoft/bond/issues/82)

## Earlier release ##

These sections need to be written. In the meantime, consult the
[repository's history](https://github.com/Microsoft/bond/commits/master).
