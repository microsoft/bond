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
* `gbc` & compiler library: (major bump already done in bond.cabal)
* IDL core version: TBD
* IDL comm version: TBD
* C++ version: (major bump needed)
* C# NuGet version: (minor bump needed)
* C# Comm NuGet version: TBD

### `gbc` and Bond compiler library ###

* C++ codegen now properly generates move assignment operator which was broken
  for some cases.
* C++ codegen no longer generates checks for C++11, except for MSVC 2013 workarounds.
* C++ codegen no longer generates data member initialization that invokes a constructor
  accepting `Comparer` for associative containers.
* C++ codegen now can generate copy and move constructors with an allocator argument
  when a custom allocator is used and `--alloc-ctors` is passed to `gbc`.
* C++ codegen now can generate [type aliases](http://en.cppreference.com/w/cpp/language/type_alias)
  that correspond to ones in IDL when `--type-aliases` flag is passed to `gbc`.
* C++ codegen now can use [`std::scoped_allocator_adaptor`](http://en.cppreference.com/w/cpp/memory/scoped_allocator_adaptor)
  for strings and containers when custom allocator is used and `--scoped-alloc` flag
  is passed to `gbc`.
* C++ codegen now generates lazily constructed enum name-to-value and value-to-name maps.
  Additionally, a user-defined map type can now be provided to `GetNameToValueMap` and
  `GetValueToNameMap`.
* `import` statements can now end with an optional semicolon.

### C++ ###

* **Breaking change** Constructors accepting a `Comparer` have been removed from
  `bond::maybe` and `bond::nullable` types.
* **Breaking change** The `bond::is_blob` and `bond::is_nullable` traits
  have been removed. The `blob` and `nullable` types are not customizable,
  so these where never needed or used. The related functionality provided by
  `bond::get_list_sub_type_id` remains.
* The CMake build now enforces a minimum Boost version of 1.58. The build
  has required Boost 1.58 or later since version 5.2.0, but this was not
  enforced.
* gRPC v1.10.0 is now required to use Bond-over-gRPC.
    * This version include a number of memory leak fixes that users of Bond-over-gRPC were encountering. [Issue #810](https://github.com/Microsoft/bond/issues/810)
* Fixed includes for gRPC services with events or parameterless methods.
  [Issue #735](https://github.com/Microsoft/bond/issues/735)
* Fixed a bug which would read an unrelated struct's field(s) when deserializing a
  base struct. [Issue #742](https://github.com/Microsoft/bond/issues/742)
* Fixed a bug in `bond::MapTo<T>::Field` where `Protocols` type parameter was
  not passed to `bond::Apply`.
* Fixed a race condition when `bond::ext::gRPC::io_manager::shutdown` and
  `bond::ext::gRPC::io_manager::wait` are called concurrently.
* Fixed a race condition during `bond::ext::gRPC::unary_call` destruction.
* Fixed the broken move constructor of `bond::bonded<T, Reader&>`.
* Fixed the move constructor of `bond::value` to actually move the underlying reader.
* Added `bond::ProtobufBinaryWriter<Buffer>` for serialization to the Protocol
  Buffers binary format.

### C# ###

* The C# attribute `Bond.Attribute` can now be applied to methods. This
  fixes broken codegen when attributes are used on service methods.
  [Issue #617](https://github.com/Microsoft/bond/issues/617)
* Bond Attributes on service methods are now present on all the client
  overloads for the methods. Previously, just the "friendly" method had the
  attributes.

## 7.0.2: 2017-10-30 ##
* `gbc` & compiler library: 0.10.1.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 7.0.2
* C# NuGet version: 7.0.1
* C# Comm NuGet version: 0.14.0

### C++ ###

* Fixed a memory leak when deserializing Bond-over-gRPC messages that were
  stored as multiple slices.

### C# ###

* There were no C# changes in this release.

## 7.0.1: 2017-10-26 ##
* `gbc` & compiler library: 0.10.1.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 7.0.0
* C# NuGet version: 7.0.1
* C# Comm NuGet version: 0.14.0

### C# ###

* Fixes a regression introduced in 7.0.0 that resulted in an exception during
  generation of Serializer instances if the type contained an aliased
  `required` blob field.

## 7.0.0: 2017-10-24 ##
* `gbc` & compiler library: 0.10.1.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 7.0.0
* C# NuGet version: 7.0.0
* C# Comm NuGet version: 0.13.0

### `gbc` and Bond compiler library ###

* Add service/method annotations in C# for Comm and gRPC.
* Add service/method metadata support in C++ for gRPC.
* C++ codegen now uses [`std::allocator_traits`](http://en.cppreference.com/w/cpp/memory/allocator_traits)
  for rebinding allocator types.

### C++ ###

* Added `bond::make_box` helper function to create `bond::Box<T>` instances.
* When Unicode conversion fails during JSON deserialization to wstring, a
  bond::CoreException is now thrown instead of a Boost exception.
* When SimpleJSON deserializes a map key with no matching value, a
  bond::CoreException is now thrown.
* When SimpleJSON deserializes a map key of non-primitive type, a
  bond::CoreException is now thrown.
* Errors from some versions of G++ like "non-template type `Deserialize`
  used as a template" have been fixed.
  [Issue #538](https://github.com/Microsoft/bond/issues/538)
* Guard against overflows in OutputMemoryStream, blob, and SimpleArray.
* Use RapidJSON's iterative parser to handle deeply nested JSON data without
  causing a stack overflow.
* Guard against min/max being function-style macros in more places.
* Allow Bond-generated C++ types to use non-default-constructable
  allocators. (This works even on MSVC 2013 by only compiling the
  generated-type's default constructor if it is used. The default
  constructor is now a templated constructor that is invokable with zero
  arguments.)
* Fixed some macro uses that did not have sufficient parenthesis around
  parameters and resulted in compiler errors.
* Added the `bond::ext::gRPC::shared_unary_call` type. This type can be used
  when shared ownership semantics are needed for `unary_call` instances.
* Provide compile-time access to metadata about gRPC services and methods.
* Using `bond::ext::gRPC::wait_callback` no longer causes a shared_ptr cycle
  and the resulting resource leak.
* Ensure that `bond_grpc.h` and `bond_const_grpc.h` are generated when the
  CMake variable `BOND_ENABLE_GRPC` is set to that importing `bond.bond` and
  `bond_const.bond` when defining a service works.
* Added `bond::capped_allocator` adapter that will allow to limit the max
  number of bytes to allocate during deserialization.

### C# ###

* **Breaking change** The code generation MSBuild targets no longer support
  Mono's xbuild: only MSBuild is supported. Mono has
  [deprecated xbuild in favor of MSBuild](http://www.mono-project.com/docs/about-mono/releases/5.0.0/#msbuild)
  now that
  [MSBuild is open source and cross-platform](https://github.com/Microsoft/msbuild).
* **Breaking change** The code generation MSBuild targets now automatically
  compile the generated `_grpc.cs` files if `--grpc` is passed to `gbc`.
  Explicit `<Compile Include="$(IntermediateOutputPath)foo_grpc.cs" />`
  lines in MSBuild projects will need to be removed to fix error MSB3105
  about duplicate items. See commit
  [a120cd99](https://github.com/Microsoft/bond/commit/a120cd9995d74e11b75766c5195ea4587c304dd7#diff-3b0b4bed9029ae89dbfb824ce7eff5e8R54)
  for an example of how to fix this.
  [Issue #448](https://github.com/Microsoft/bond/issues/448)
* **Breaking change** The low-level API `IParser.ContainerHandler` now has an
  `arraySegment` parameter for the converted blob.
* The code generation MSBuild targets will now skip compiling the
  `_types.cs` files when `--structs=false` is passed to `gbc`.
* Added `Bond.Box.Create` helper method to create `Bond.Box<T>` instances.
* Reflection.IsBonded now recognizes custom IBonded implementations.
* Use Newtonsoft's JSON.NET BigInteger support -- when available -- to
  handle the full range of uint64 values in the SimpleJson protocol (.NET
  4.5 or greater, .NET Standard 1.6 or greater).
* `Bond.IO.Unsafe.InputStream` can now be used with streams that do not
  implement [`Stream.Seek`][msdn-stream-seek], like
  [`System.IO.Compression.GzipStream`][msdn-gzipstream].
  [Issue #498](https://github.com/Microsoft/bond/issues/498)
    * Such streams are detected by inspecting
      [`Stream.CanSeek`][msdn-stream-canseek].
* Fix a bug in CompactBinaryWriter when using v2 that repeated first pass
  when a bonded field was serailized, resulting in extra work and extra
  state left in the CompactBinaryWriter.
* Apply IDL annotations to services and methods for gRPC.
  [Issue #617](https://github.com/Microsoft/bond/issues/617)
* Fixed a bug that produced C# code that couldn't be compiled when using
  Bond-over-gRPC with a generic type instantiated with a collection.
  [Issue #623](https://github.com/Microsoft/bond/issues/623)
* When targeting .NET 4.5, avoid resolving external entities when using
  `SimpleXmlReader`.
* Remove redundant conversions during serialization of aliased blobs.

[msdn-gzipstream]: https://msdn.microsoft.com/en-us/library/system.io.compression.gzipstream(v=vs.110).aspx
[msdn-stream-canseek]: https://msdn.microsoft.com/en-us/library/system.io.stream.canseek(v=vs.110).aspx
[msdn-stream-seek]: https://msdn.microsoft.com/en-us/library/system.io.stream.seek(v=vs.110).aspx

### C# Comm ###

* Apply IDL annotations to services and methods for Comm.

## 6.0.1 ##

This version was not used.

## 6.0.0: 2017-06-29 ##
* `gbc` & compiler library: 0.10.0.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 6.0.0
* C# NuGet version: 6.0.0
* C# Comm NuGet version: 0.12.0

### `gbc` and Bond compiler library ###

* IDL support for service inheritance syntax
    * **Breaking change** In the Bond Haskell library, the `Service` type
      has a new field `serviceBase`.
* C++ codegen now generates
  [extern templates](http://en.cppreference.com/w/cpp/language/function_template)
  of `bond::Apply` instead of overloads.
* C++ codegen hides FieldTemplate details, shortening symbol names.

### C++ ###

* **Breaking change** A C++11 compiler is now required. The minimum
  supported C++ compiler versions are now:
    * Clang 3.4 or newer
    * GNU C++ 4.7 or newer
    * Microsoft Visual C++ 2013 or newer
* **Breaking change** The generated apply.h/.cpp files now contain
  [extern templates](http://en.cppreference.com/w/cpp/language/function_template)
  of `bond::Apply` instead of overload implementations. Calls to bare `Apply`
  or `TypeNamespace::Apply` must be changed to `bond::Apply`.
* **Breaking change** Users who are implementing custom streams are now
  required to provide the free functions `CreateInputBuffer`,
  `CreateOutputBuffer` and `GetBufferRange`, depending on which scenarios
  are used (there will be a corresponding compilation error for each case).
    * Users who were _mistakenly_ calling `bond::Merge<T>` with explicit an
      template argument will get a compilation error. To fix, remove the
      `<T>` part.
    * In addition, users of MSVC12 are required to define a `range_type`
      typedef as a return type of corresponding `GetBufferRange` inside
      their custom input buffer implementation.
    * Please see
      [InputBuffer](https://github.com/Microsoft/bond/commit/11beaf5319639e4bdee96a25f95154e4fed93a75#diff-9260b18a00d12a6102a69b9fffd7e33f),
      [OutputBuffer](https://github.com/Microsoft/bond/commit/11beaf5319639e4bdee96a25f95154e4fed93a75#diff-1f15d4c92f87d4bd41f705b20cce80ad),
      and
      [the bf example](https://github.com/Microsoft/bond/commit/11beaf5319639e4bdee96a25f95154e4fed93a75#diff-bdda0f39d99280d4858b4453906eea17)
      for more details.
* **Breaking change** The `bond::customize<protocols>` has been removed. All the
  public APIs that require a protocol list (e.g. `bond::Marshal`) now accept
  an extra template argument `Protocols` which defaults to `bond::BuiltInProtocols`.
  Custom input streams now require `bond::type_id<>` to be specialized with a
  unique magic number. For more details please see [the bf example](https://github.com/Microsoft/bond/tree/master/examples/cpp/core/bf).
* Initial support for sending
  [Bond objects over gRPC](https://microsoft.github.io/bond/manual/bond_over_grpc.html)
  has been added.
* The `bond::Apply` function now has a uniform signature. Call sites for the
  `Marshaler<Writer>` transform overload that were _mistakenly_ passing
  `Writer` explicitly (e.g. `bond::Apply<Writer>(marshaler, value)`) will
  now get a compiler error. To fix, remove the `<Writer>` part:
  `bond::Apply(marshaler, value)`.
* Fixed a bug that caused serialization using
  `CompactBinaryWriter<OutputCounter>` (to get the expected length of
  serializing with compact binary) to produced bogus results.
* Fixed
  [custom streams](https://microsoft.github.io/bond/manual/bond_cpp.html#custom-streams)
  support which was broken for some scenarios.
* For Visual C++ 2017 compability, RapidJSON v1.0.0 or newer is now
  required. The RapidJSON submodule that Bond uses by default has been
  updated to v1.1.0 due to a warning from clang in earlier versions.
* C++ codegen hides FieldTemplate details, shortening symbol names.

### C# ###

* **Breaking change** Support for .NET 4.0 has been dropped from the
  [supported frameworks](https://microsoft.github.io/bond/manual/bond_cs.html#frameworks-targeted).
* **Breaking change** The deprecated type `Bond.BondReflection` has been
  removed. The type `Bond.Reflection` should be used instead.
* **Breaking change** Bond assemblies are now
  [strong-name signed](https://msdn.microsoft.com/en-us/library/wd40t7ad(v=vs.110).aspx)
  with the
  [bond.snk](https://github.com/Microsoft/bond/blob/82c97e12621eeb906d1bd46e3abba9da14289c61/cs/build/internal/bond.snk)
  key in the repository instead of with a Microsoft key. This allows anyone
  to produce compatible assemblies, not just Microsoft. Official
  distribution of Bond will continue to be
  [Authenticode signed](https://msdn.microsoft.com/en-us/library/ms537361(v=vs.85).aspx)
  with a Microsoft certificate.
  [Issue #414](https://github.com/Microsoft/bond/issues/414)
    * The new public key for assemblies is now
      `00240000048000009400000006020000002400005253413100040000010001000d504ac18b4b149d2f7b0059b482f9b6d44d39059e6a96ff0a2a52678b5cfd8567cc67254132cd2debb5b95f6a1206a15c6f8ddac137c6c3ef4995f28c359acaa683a90995c8f08df7ce0aaa8836d331a344a514c443f112f80bf2ebed40ccb32d7df63c09b0d7bef80aecdc23ec200a458d4f8bafbcdeb9bf5ba111fbbd4787`
* **Breaking change** Bond assemblies now have assembly and file versions
  that correspond to their NuGet package version. Strong name identities
  will now change release-over-release in line with the NuGet package
  versions. [Issue #325](https://github.com/Microsoft/bond/issues/325)
* The codegen MSBuild targets will now re-run codegen if gbc itself has been
  changed.
* Fixed a bug where JSON and XML protocols would permit the serialization of
  non-nullable string fields that were set to null instead of throwing a
  NullReferenceException.
  [Issue #417](https://github.com/Microsoft/bond/issues/417)

## 5.3.1: 2017-04-25 ##

* `gbc` & compiler library: 0.9.0.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 5.3.0
* C# NuGet version: 5.3.1
* C# Comm NuGet version: 0.11.1

### C# ###

* Cleaned up documentation comments.

## 5.3.0: 2017-04-12 ##

* `gbc` & compiler library: 0.9.0.0
* IDL core version: 2.0
* IDL comm version: 1.2
* C++ version: 5.3.0
* C# NuGet version: 5.3.0
* C# Comm NuGet version: 0.11.0

### `gbc` and Bond compiler library ###

* C++ codegen ensures that parameter names do not shadow field names.
* When generating C++ apply files, there are now explicit `bond::Apply<>`
  instantiations for `CompactBinaryWriter<OutputCounter>` and
  `SimpleBinaryWriter<Null>` writers.
  [Pull request #373](https://github.com/Microsoft/bond/pull/373)
    * **Breaking change (Haskell library only):**
      `Language.Bond.Codegen.Cpp.ApplyOverloads.Protocol` is now a union of
      `ProtocolReader` and `ProtocolWriter` to permit mixing and matching of
      reader/writer protocols without having to explicitly compute the full
      cross product.
* Add gbc flags to pick which C# files to generate (structs, gRPC, and
  comm). Only structs are generated by default.
* gbc ensures that method names are unique within a service.
  [Issue #381](https://github.com/Microsoft/bond/issues/381)

### C++ ###

* Fix Python shared_ptr converter build break with Boost 1.63.
* Improve compliance with
  [Microsoft's SDL](https://www.microsoft.com/en-us/sdl/).
    * Bond now builds on MSVC with
      [`_CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES`](https://msdn.microsoft.com/en-us/library/ms175759.aspx)
      instead of `_CTR_SECURE_NO_WARNINGS`.
    * Bond builds on MSVC with SDL recommended warnings enabled.
* Eliminate need for warning suppression on MSVC14 via warning.h in Bond
  itself. warning.h is still in place on MSVC12; furthermore, we don't alter
  warning.h for now as it may be depended upon by application code.
* Avoid unaligned memory access on non-x86/x64 platforms.
  [Issue #305](https://github.com/Microsoft/bond/issues/305)
* Improve compliance with strict-aliasing rules.
    * Bond now builds on Clang/GCC with `-fstrict-aliasing`.
* When generating C++ apply files, there are now explicit `bond::Apply<>`
  instantiations for `CompactBinaryWriter<OutputCounter>` and
  `SimpleBinaryWriter<Null>` writers.
  [Pull request #373](https://github.com/Microsoft/bond/pull/373)
* Improve C++ allocator support
  [Issue #379](https://github.com/Microsoft/bond/issues/379)
  [Pull request #380](https://github.com/Microsoft/bond/pull/380)
    * Support C++11 and above allocator model for rebind
    * Simplify detection of the default allocator
* Remove per-field instantiation of DynamicParser<>::UnknownFieldOrTypeMismatch method.

## C# ###

* Added gRPC integration. See the
  [Bond-over-gRPC manual](https://microsoft.github.io/bond/manual/bond_over_grpc.html).
* Added controls to cap incremental allocation between reads in
  `Bond.IO.Unsafe.InputStream`.
* Extended fix for bug parsing JSON when a string value is a date.
  [Pull request #358](https://github.com/Microsoft/bond/pull/358)
* Bond C# 5.1.0 accidentally broke backward compability by renaming
  `Bond.Reflection` to `Bond.BondReflection`. This has been fixed:
  `Bond.BondReflection` was unrenamed back to `Bond.Reflection`, and a shim
  `Bond.BondReflection` type now redirects all calls to their original names
  to minimize further breakage.
  [Issue #369](https://github.com/Microsoft/bond/issues/369)
    * Code that started using `Bond.BondReflection` by name will encounter
      warning CS0618 indicating use of an obselete method/type. To fix this,
      use the original name `Bond.Reflection`. This warning can be
      suppressed if needed. However...
    * ...the shim type `Bond.BondReflection` will be removed during or after
      the next major release of C# Bond.

### C# Comm ###

* **Breaking change** To generate C# Comm files, be sure to pass the
  `--comm` flag to gbc.
  [See how the pingpong example was updated](https://github.com/Microsoft/bond/blob/581d88632e0ad8b2fc87f5674273f613e78af752/examples/cs/comm/pingpong/pingpong.csproj#L39).
* C# Comm is now deprecated. We recommend that you use Bond-over-gRPC. See
  the
  [Bond-over-gRPC manual](https://microsoft.github.io/bond/manual/bond_over_grpc.html).
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

## Breaking changes between 3.x and 4.x ##

Bond C# had the following breaking changes introduced in 4.x compared to the
3.x versions:

* The
  [Bond.Core.CSharp NuGet package](https://www.nuget.org/packages/Bond.Core.CSharp/)
  was introduced so that not all uses of Bond depend on Json.NET. The
  [Bond.Runtime.CSharp package](https://www.nuget.org/packages/Bond.Runtime.CSharp/)
  still depends on Json.NET and also depends on Bond.Core.CSharp. The
  primary package remains Bond.CSharp.
* `CompactBinaryReader<InputStream>` now
  [explicitly implements](https://docs.microsoft.com/en-us/dotnet/articles/csharp/programming-guide/interfaces/explicit-interface-implementation)
  `ICloneable<CompactBinaryReader<InputStream>>` instead of implicitly.
  `FastBinaryReader` and `SimpleBinaryReader` were likewise changed.

## Earlier release ##

These sections need to be written. In the meantime, consult the
[repository's history](https://github.com/Microsoft/bond/commits/master).
