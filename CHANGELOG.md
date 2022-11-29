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

* IDL core version: TBD
* C++ version: TBD (major bump needed)
* C# NuGet version: TBD (major bump needed)
* `gbc` & compiler library: TBD (major bump needed)

### `gbc` and Bond compiler library ###
* **Breaking change**: Codegen for Bond-over-gRPC has been removed: the
  `--grpc` switch is no longer supported. Service definitions are still
  parsed, but codegen can no longer be done for C++ or C#. See [issue
  \#1131, Bond-over-gRPC will be deprecated February
  2022](https://github.com/microsoft/bond/issues/1131), for the full
  announcement.

### C++ ###

* **Breaking change**: All Bond-over-gRPC code has been removed. This is
  everything under the `bond::ext:grpc` namespace. Service definitions can
  still appear in .bond files, but no C++ code will be generated for them.
  See [issue \#1131, Bond-over-gRPC will be deprecated February
  2022](https://github.com/microsoft/bond/issues/1131), for the full
  announcement.

### C# ###

* **Breaking change**: All Bond-over-gRPC code has been removed. This is
  everything under the `Bond.Grpc` namespace and the Bond.Grpc.CSharp NuGet
  package. Service definitions can still appear in .bond files, but no C#
  code will be generated for them. See [issue \#1131, Bond-over-gRPC will be
  deprecated February 2022](https://github.com/microsoft/bond/issues/1131),
  for the full announcement.
* Added codegen and deserialization support for container type aliases to
  use
  [System.Collections.Immutable](https://learn.microsoft.com/dotnet/api/system.collections.immutable)
  collections. (Pull request
  [\#1161](https://github.com/microsoft/bond/pull/1161))

## 10.0: 2022-03-07 ##

* IDL core version: 3.0
* C++ version: 10.0
* C# NuGet version: 10.0
* `gbc` & compiler library: 0.12.1.0

### C++ ###

* **Breaking change**: Bond-over-gRPC has been marked deprecated.
  Bond-over-gRPC will be removed in the next major version of Bond. See
  [issue \#1131, Bond-over-gRPC will be deprecated February
  2022](https://github.com/microsoft/bond/issues/1131), for the full
  announcement.
  * The `[[deprecated]]` attribute has been added to the `bond::ext::grpc`
    namespace in every top-level Bond-over-gRPC++ header. This will cause
    compiler warnings/errors for uses of Bond-over-gRPC++ that you may need
    to handle.
* Fixed multiple symbol definition for Win32Exception in
  `grpc/win_thread_pool.h`. ([Issue
  \#1129](https://github.com/microsoft/bond/issues/1129))
* Add forward declaration for `GenericWriteVariableUnsigned` to fix "C3861:
  'GenericWriteVariableUnsigned': identifier not found" when using custom
  streams that do not have their own implementation of
  `WriteVariableUnsigned`. ([Issue
  \#1115](https://github.com/microsoft/bond/issues/1115))
* Fixed `bond::DynamicParser` that may not emit transform's `OmittedField`
  for compile-time schema and an omitted optional field in the payload.
  ([Issue \#1120](https://github.com/microsoft/bond/issues/1120))
* Fixed missing include directives.
* Removed `bond::blob`'s unnecessary `_content` member, reducing its size
  by 1 pointer.
* Added an ability to apply transform to a schema.
* Added `noexcept` to `bond::blob`'s non-throwing functions.

### C# ###

* **Breaking change**: Bond-over-gRPC code has been marked deprecated.
  Bond-over-gRPC will be removed in the next major version of Bond. See
  [issue \#1131, Bond-over-gRPC will be deprecated February
  2022](https://github.com/microsoft/bond/issues/1131), for the full
  announcement.
  * The `[Obsolete]` attribute has been added to every public type in the
    `Bond.Grpc` assembly. This will cause compiler warnings/errors for uses
    of Bond-over-gRPC# that you may need to handle.
* Added virtual method `OutputBuffer.ResizeBuffer` that can be overridden to
  use buffer allocators other than `new byte[]` (e.g.
  `ArrayPool<byte>.Rent()`). ([Pull request
  \#1128](https://github.com/microsoft/bond/pull/1128))
* The error message emitted when duplicate .bond items are detected by the
  MSBuild codegen now correctly refers to `$(EnableDefaultItems)` as the
  switch that controls this behavior. ([Issue
  \#1110](https://github.com/microsoft/bond/issues/1110))

## 9.0.5: 2021-04-14 ##

* IDL core version: 3.0
* C++ version: 9.0.5
* C# NuGet version: 9.0.5
* `gbc` & compiler library: 0.12.1.0

### C++/Python ###

* Added CMake variable `BOND_FIND_GRPC` to allow for external gRPC
  installations. The search for external GRPC installations is only done
  when `BOND_ENABLE_GRPC` is `TRUE`.
* Removed use of deprecated `std::ptr_fun` in the Python library. ([Issue
  \#1080](https://github.com/microsoft/bond/issues/1080))

### C# ###

* Implicit codegen now excludes any .bond files in the project's output
  directories (e.g., `bin/`, `obj/debug/netstandard1.0`). This behavior
  matches the implicit compilation behavior for .cs files.

## 9.0.4: 2020-11-23 ##

* IDL core version: 3.0
* C++ version: 9.0.4
* C# NuGet version: 9.0.3
* `gbc` & compiler library: 0.12.1.0

### C++ ###

* Bond now uses the `[[noreturn]]` attribute to annotate functions that do
  not return. Previously, it used compiler-specific annotations.

### C# ###

* There were no C# changes in this release.

## 9.0.3: 2020-08-06 ##

* IDL core version: 3.0
* C++ version: 9.0.2
* C# NuGet version: 9.0.3
* `gbc` & compiler library: 0.12.1.0

### C++ ###

* There were no C++ changes in this release.

### C# ###

* Fixed a performance regression in `OutputBuffer.Grow`: it was incorrectly
  growing the buffer by one byte at a time instead of geometrically. ([Issue
  \#1065](https://github.com/microsoft/bond/issues/1065), [Pull request
  \#1066](https://github.com/microsoft/bond/pull/1066))

## 9.0.2: 2020-08-03 ##
* IDL core version: 3.0
* C++ version: 9.0.2
* C# NuGet version: 9.0.2
* `gbc` & compiler library: 0.12.1.0

### C++ ###

* gbc is now installed with 555 (r-xr-xr-x) permissions. ([Issue
  \#1062](https://github.com/microsoft/bond/issues/1062))

### C# ###

* Fixed a regression writing to non-seekable streams using
  `CompactBinaryWriter`. The fix in commit
  [b0fd4a1](https://github.com/microsoft/bond/commit/b0fd4a15a7cae946dd2855122559ca59cc34dbea#diff-9534daaa1fb3d4776494b25c8bba3939L212)
  inadvertently added a call to `Stream.Position` in the Release
  configuration. This call is only intended to be made when Bond is built in
  the Debug configuration.

## 9.0.1: 2020-07-14 ##
* IDL core version: 3.0
* C++ version: 9.0.1
* C# NuGet version: 9.0.1
* `gbc` & compiler library: 0.12.1.0

### C++ ###
* The RapidJSON submodule now points to commit
  [8f4c021](https://github.com/Tencent/rapidjson/commit/8f4c021fa2f1e001d2376095928fc0532adf2ae6).
  This addresses some aliasing warnings in recent versions of Clang and GCC.
    * The RapidJSON submodule now uses its new home under the [Tencent
      organization](https://github.com/Tencent/rapidjson.git).

### C# ###

* Fixed MSB3105/CS2002 error about duplicate Compile items when a directory
  contains multiple .bond files and `--gprc` is in `$BondOptions`. ([Issue
  \#1050](https://github.com/microsoft/bond/issues/1050))
* Fix handling of large container lengths that could cause an infinite loop
  when deserializing some payloads. This fix addresses
  [CVE-2020-1469](https://portal.msrc.microsoft.com/en-US/security-guidance/advisory/CVE-2020-1469).

## 9.0: 2020-05-26  ##
* IDL core version: 3.0
* C++ version: 9.0.0
* C# NuGet version: 9.0.0
* `gbc` & compiler library: 0.12.1.0

### C++ ###
* **Breaking change** MSVC 2013 support has been removed. ([Issue
  \#851](https://github.com/microsoft/bond/issues/851))
* **Breaking change** `bond/core/warning.h` has been deleted. Since the
  [5.3.0 release](#530-2017-04-12), Bond hasn't needed global warning
  suppressions for any compiler except MSVC 2013. This file was only used
  for MSVC 2013, which is no longer a supported compiler.
* Fixed MSVC warning for deprecation of `std::result_of_t` in `/std:c++17`.
  ([Issue \#1007](https://github.com/microsoft/bond/issues/1007))
* Fixed MSVC warning C5208: unnamed class used in typedef name cannot
  declare members other than non-static data members, member enumerations,
  or member classes. ([Issue
  \#1027](https://github.com/microsoft/bond/issues/1027))
* Fixed Boost 1.73 warning "The practice of declaring the Bind placeholders
  (_1, _2, ...) in the global namespace is deprecated." ([Pull request
  \#1036](https://github.com/microsoft/bond/pull/1036))

### C# ###

* Fixed creation of duplicate default constructor when defining an empty struct
  and using `--preview-constructor-parameters`.
  ([Issue \#963](https://github.com/microsoft/bond/issues/963))
* The .NET assemblies are now only Authenticode signed with SHA-2. Legacy
  Windows OS versions may need [updates to work with SHA-2
  signatures](https://support.microsoft.com/en-us/help/4472027/2019-sha-2-code-signing-support-requirement-for-windows-and-wsus).

### `gbc` and Bond compiler library ###
* C++ codegen no longer supports MSVC 2013.
* gbc is now only Authenticode signed with SHA-2. Legacy Windows OS versions
  may need [updates to work with SHA-2
  signatures](https://support.microsoft.com/en-us/help/4472027/2019-sha-2-code-signing-support-requirement-for-windows-and-wsus).

## gbc & compiler library 0.12.0.1: 2019-11-21 ##
* IDL core version: 3.0
* C++ version: 8.2.0
* C# NuGet version: 8.2.0
* `gbc` & compiler library: 0.12.0.1

There are no changes to code generation or the language projections.

### Bond compiler library ###

* Fixed Cabal packaging issues that prevented the publication of
  [0.12.0.0](#820-2019-11-18).

## 8.2.0: 2019-11-18  ##
* IDL core version: 3.0
* C++ version: 8.2.0
* C# NuGet version: 8.2.0
* `gbc` & compiler library: 0.12.0.0

### C++ ###

* gRPC v1.17.1 is now required to use Bond-over-gRPC.
* Fixed an ambiguous `HexDigit` overload compilation error when
  compiling with some versions of GCC. ([Pull request
  \#954](https://github.com/Microsoft/bond/pull/954))
* Fixed ambiguous call to `maybe::operator==` that breaks GCC 9
  build. ([Pull request
  \#975](https://github.com/microsoft/bond/pull/975))
* Fixed MSVC warning C4296: "'<': expression is always false" in protocol.h.
  ([Issue
  \#981](https://github.com/microsoft/bond/issues/981))

### C# ###

* Added .NET 4.6 target framework to Bond.IO.dll so that it can use
  `MemoryStream.TryGetBuffer()` when cloning streams like is done when
  targeting .NET Standard 1.3+.

### Bond compiler library ###

* **Breaking change** The bond compiler library (the Haskell library) and
  `gbc` have been switched to build with stackage snapshot lts-14.4. This
  snapshot uses megaparsec 7 and aeson 1.4.4, both of which had breaking
  changes that are reflected in the library, e.g., the `parseBond` signature
  has changed. There is no impact to users of the gbc command line tool or
  changes to C++ and C# code generation.

## 8.1.0: 2019-03-27 ##

### C++ ###

* There were no C++ changes in this release.

### C# ###

* Updated to gRPC v1.17.1.
* Add a constructor that accepts a `CallInvoker` instance to the generated
  gRPC clients to support client-side interceptors. For more details about
  C# interceptors, see the [proposal in the gRPC
  project](https://github.com/grpc/proposal/blob/master/L12-csharp-interceptors.md).
  [Issue \#950](https://github.com/microsoft/bond/issues/950)

## 8.0.1: 2018-06-29 ##
* `gbc` & compiler library: 0.11.0.3
* IDL core version: 3.0
* C++ version: 8.0.1
* C# NuGet version: 8.0

### C++ ###
* Fixed a crash in `bond::ext::grpc::server`'s destructor when invoked on
  a moved-from object instance.
* Added move-assignment operator to `bond::blob`.
* Added cmake variable `BOND_FIND_RAPIDJSON` to allow for external RapidJSON installations.

### C# ###

* There were no C# changes in this release.

### C# ###
* Fixed alias conversion issue for generic fields [Issue
  \#928](https://github.com/microsoft/bond/issues/928).

## 8.0.0: 2018-05-30 ##
* `gbc` & compiler library: 0.11.0.0
* IDL core version: 3.0
* C++ version: 8.0
* C# NuGet version: 8.0

### `gbc` and Bond compiler library ###

* **Breaking change** The deprecated Bond Comm functionality has been removed.
  This includes all gbc options related to Comm and the Comm codegen templates
  in the Haskell library. [Issue
  \#824](https://github.com/microsoft/bond/issues/824)
* C++ codegen now properly generates move assignment operators. Previously,
  this was broken for some cases.
* C++ codegen no longer generates checks for C++11, except for MSVC 2013
  workarounds.
* C++ codegen no longer generates data member initialization that invokes a
  constructor accepting `Comparer` for associative containers.
* C++ codegen can now generate copy and move constructors with an allocator
  argument when a custom allocator is used and `--alloc-ctors` is passed to
  `gbc`.
* C++ codegen can now generate [type
  aliases](http://en.cppreference.com/w/cpp/language/type_alias) that
  correspond to ones in IDL when the `--type-aliases` flag is passed to `gbc`.
* C++ codegen can now use
  [`std::scoped_allocator_adaptor`](http://en.cppreference.com/w/cpp/memory/scoped_allocator_adaptor)
  for strings and containers when a custom allocator is used and the
  `--scoped-alloc` flag is passed to `gbc`.
* C++ codegen now generates lazily constructed enum name-to-value and
  value-to-name maps. Additionally, a user-defined map type can now be
  provided to `GetNameToValueMap` and `GetValueToNameMap`.
* C++ codegen now applies the `--export-attribute` to the `ToString`,
  `FromString`, `ToEnum` and `FromEnum` functions.
* Fixed a bug in C++ codegen that incorrectly applied the export attribute to
  generic gRPC services.
* C++ codegen now generates an `allocator_type` typedef for a struct when the
  `--allocator` option is passed to `gbc`, instead of specializing
  `std::uses_allocator`.
* `import` statements can now end with an optional semicolon.
* File and directory paths on the command line, in response files, or in
  `import` statements can now use a mix of forward and backslashes. [Issue
  \#869](https://github.com/microsoft/bond/issues/869)
* gbc is now Authenticode dual-signed with both SHA-1 and SHA-2.

### C++ ###

* **Breaking change** The deprecated Bond Comm functionality has been removed.
  This includes all gbc options related to Comm and all Comm APIs and header
  files. [Issue \#824](https://github.com/microsoft/bond/issues/824)
* **Breaking change** Only versions of Boost released in the past two years
  (1.61 and newer) are supported. Bond will *likely* continue to work with
  older versions, but it is no longer tested with anything older than 1.61.
  Test coverage for Boost versions 1.61&ndash;1.66 has been improved. [Issue
  \#771](https://github.com/microsoft/bond/issues/771)
* **Breaking change** Constructors accepting a `Comparer` have been removed
  from the `bond::maybe` and `bond::nullable` types.
* **Breaking change** The `bond::is_blob` and `bond::is_nullable` traits have
  been removed. The `blob` and `nullable` types are not customizable, so these
  where never needed or used. The related functionality provided by
  `bond::get_list_sub_type_id` remains.
* **Breaking change** Removed a dangerous implicit conversion operator from
  `bond::maybe<T>` to `const T&`. To access a `bond::maybe<T>` value, use one
  of the `bond::maybe<T>::value` functions.
* **Breaking change** The nested `pointer`, `const_pointer`, `reference` and
  `const_reference` typedefs have been removed from `bond::nullable<T>`.
* **Breaking change** The `Allocator` (second) type parameter has be removed
  from `bond::nullable<T>` and now it is always deduced from `T`.
* **Breaking change** The `bond::capped_allocator` and related types have been
  moved to the `bond::ext` namespace and the "bond/ext" include directory.
* **Breaking changes** in Bond-over-gRPC (based on real-world use and
  feedback). Check the updated
  [examples](https://github.com/microsoft/bond/tree/master/examples/cpp/grpc)
  to see how to use the changed APIs.
  - The generated `ClientCore` and `ServiceCore` class templates and the
    `Client` and `Service` convenience typedefs have all been replaced with
    normal classes named `Client` and `Service`. The `ThreadPool` type
    parameter has been removed in favor of a simplified runtime representation
    of a `Scheduler`.
  - The `Scheduler` concept and the `bond::ext::gRPC::thread_pool`
    implementation now use `operator()` instead of a `schedule()` member
    function.
  - The `bond::ext::gRPC::server_core` class template and the
    `bond::ext::gRPC::server` convenience typedef have been replaced with the
    normal class `bond::ext::gRPC::server`.
  - The generated `Client::Async*` functions now accept the
    `std::shared_ptr<grpc::ClientContext>` argument as the last parameter
    instead of as the first. This makes is easier to omit this parameter when
    no context customization is needed.
  - The client callback now directly accepts
    `bond::ext::gRPC::unary_call_result<Response>` (drops the
    `std::shared_ptr`). Also the `unary_call_result` now exposes read-only
    getters rather than fields. This simplified the type that clients need to
    deal with.
  - The `bond::ext::gRPC::wait_callback::arg_type` has been removed.
  - The `client_callback.h` header file has been renamed to
    `unary_call_result.h` to align with its contents.
  - The `bond::ext::gRPC::server_builder` has been replaced by the
    `bond::ext::gRPC::server::Start` factory function which now returns a
    plain `bond::ext::gRPC::server` object and accepts service instances
    managed by `std::unique_ptr`. This properly models the lifetime
    requirements. Service implementations must now pass a `Scheduler` to the
    generated `Service` base class which is no longer default constructible.
  - The generated method reflection information no longer uses a redundant
    `bonded<T>` wrapper for `input_type` and `result_type` typedefs.
  - The `bond::ext::gRPC::unary_call` no longer requires `bonded<T>` wrapper
    for request type.
  - The `bond::ext::gRPC::unary_call::FinishWithError` has been renamed to
    `Finish`. Overloads that take a status can be used to signal an error.
  - The `grpc::Status` second argument has been removed from
    `bond::ext::gRPC::unary_call::Finish`. gRPC does not support sending a
    response with a non-OK status, so the payload was being droped anyway.
  - Fixed `bond::ext::gRPC::unary_call`, `bond::ext::gRPC::shared_unary_call`
    and `bond::ext::gRPC::unary_call_result` types to properly use `void` and
    `bond::reflection::nothing` instead of the `bond::Void` empty struct. Also
    removed unnecessary functions from `unary_call` and `shared_unary_call`
    for those cases when they are not applicable (e.g. `Finish` is not
    available when return type is `nothing`).
* gRPC v1.12.0 is now required to use Bond-over-gRPC.
    * This version include a number of memory leak fixes that users of
      Bond-over-gRPC were encountering. [Issue
      \#810](https://github.com/microsoft/bond/issues/810)
    * This version include some Windows-specific performance
      improvements for loopback connections.
* The `bond::ext::gRPC::wait_callback` has been deprecated in favor of
  additionally generated client functions that return `std::future`.
* Fixed includes for gRPC services with events or parameterless methods.
  [Issue \#735](https://github.com/microsoft/bond/issues/735)
* Fixed a bug which would read an unrelated struct's field(s) when
  deserializing a base struct. [Issue
  \#742](https://github.com/microsoft/bond/issues/742)
* Fixed a bug in `bond::MapTo<T>::Field` that failed to pass the `Protocols`
  type parameter to `bond::Apply`.
* Fixed a race condition when `bond::ext::gRPC::io_manager::shutdown` and
  `bond::ext::gRPC::io_manager::wait` are called concurrently.
* Fixed a race condition during `bond::ext::gRPC::unary_call` destruction.
* Fixed the broken move constructor of `bond::bonded<T, Reader&>`.
* Fixed the move constructor of `bond::value` to actually move the underlying reader.
* Added the `bond::blob_prolong` helper function that will return a
  `bond::blob` with a copied data if the original one does not own the memory.
* The `bond::OutputBuffer::GetBuffers` now can accept arbitrary STL-like
  containers.
* `bond::maybe<T>` has been overhauled.
    * Fixed a bug that default initialized an instance of `T` even when a
      maybe held nothing.
    * Added `noexcept` variants of `bond::maybe<T>::value`.
    * Added `bond::maybe<T>::emplace` to construct a maybe's value in place.
    * Added various rvalue-reference and allocator-aware constructors and
      assignment operators.
    * Added `operator==(const bond::maybe<T>&, const T&)` and
      `operator==(const T&, const bond::maybe<T>&)` to compare directly to
      instances of `T`.
* Fixed an issue with the `ToString`, `FromString`, `ToEnum` and `FromEnum`
  functions that were previously not exported from a DLL when the
  `--export-attribute` option was passed to `gbc`. [Issue
  \#861](https://github.com/microsoft/bond/issues/861)
* Fixed a bug in `bond::nullable<T, Alloc>` where it was not propagating an
  allocator to `T` when `allocator_type` was not explicitly defined.
* Fixed a bug in `bond::make_box` where `const T&` was not handled correctly.
* The use of `bond::check_method` has been replaced with less restricting
  expression SFINAE checks on supported compilers. [Issue
  \#896](https://github.com/microsoft/bond/issues/896)
* Fixed a bug where `bond::ext::gRPC::io_manager` could cause a thread to join
  itself.
* The preferred namespace for Bond-over-gRPC is now `bond::ext::grpc`. The
  previous namespace, `bond::ext::gRPC`, continues to work.
* Added a Windows-specific implementation of a [thread
  pool](https://msdn.microsoft.com/en-us/library/windows/desktop/ms686766(v=vs.85).aspx).

### C# ###

* **Breaking change** The deprecated Bond Comm functionality has been removed.
  This includes all gbc options related to Comm and all Comm APIs, assemblies,
  and NuGet packages. [Issue
  \#824](https://github.com/microsoft/bond/issues/824)
* **Breaking change** The Bond.CSharp and Bond.Compiler.CSharp NuGet packages
  perform implicit codegen when the simplified .NET Core `.csproj` format is
  used. This breaking change *does not* affect projects using the classic
  `.csproj` format. Any .NET Core projects that encounter the build error
  "Duplicate BondCodegen items were included." and were explicitly listing
  `BondCodegen` items will either need to rely on implicit codegen or [disable
  all implicit inclusion](https://aka.ms/sdkimplicititems). To set per-item
  metadata, use the [item update
  syntax](https://docs.microsoft.com/en-us/visualstudio/msbuild/item-element-msbuild#examples).
  [Issue \#636](https://github.com/microsoft/bond/issues/636)
* The C# attribute `Bond.Attribute` can now be applied to methods. This fixes
  broken codegen when attributes are used on service methods. [Issue
  \#617](https://github.com/microsoft/bond/issues/617)
* Bond Attributes on service methods are now present on all the client
  overloads for the methods. Previously, just the "friendly" method had the
  attributes.
* Grpc.Core v1.12.0 is now required to use Bond-over-gRPC.
    * This version include a number of memory leak fixes that users of
      Bond-over-gRPC were encountering. [Issue
      \#810](https://github.com/microsoft/bond/issues/810)
    * This version include some Windows-specific performance improvements for
      loopback connections.
* `BondCodegen` items will now appear in the Visual Studio 2017+ UI in .NET
  Core projects.
* The .NET Standard assemblies are fully strong-name signed. Previously, they
  were inadvertently only public strong-name signed.
* The .NET assemblies are now Authenticode dual-signed with both SHA-1 and
  SHA-2.
* Fixed a bug in the codegen targets when using `gbc` from $PATH on macOS and
  Linux that prevented the C# compiler from finding the generated C# files.
* *Preview*: Added preliminary support for generating types with constructors
  with parameters for each field. This functionality will change in the future
  and may be removed. [Pull request
  \#857](https://github.com/microsoft/bond/pull/857)

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
  [Issue \#538](https://github.com/microsoft/bond/issues/538)
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
  [MSBuild is open source and cross-platform](https://github.com/microsoft/msbuild).
* **Breaking change** The code generation MSBuild targets now automatically
  compile the generated `_grpc.cs` files if `--grpc` is passed to `gbc`.
  Explicit `<Compile Include="$(IntermediateOutputPath)foo_grpc.cs" />`
  lines in MSBuild projects will need to be removed to fix error MSB3105
  about duplicate items. See commit
  [a120cd99](https://github.com/microsoft/bond/commit/a120cd9995d74e11b75766c5195ea4587c304dd7#diff-3b0b4bed9029ae89dbfb824ce7eff5e8R54)
  for an example of how to fix this.
  [Issue \#448](https://github.com/microsoft/bond/issues/448)
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
  [Issue \#498](https://github.com/microsoft/bond/issues/498)
    * Such streams are detected by inspecting
      [`Stream.CanSeek`][msdn-stream-canseek].
* Fix a bug in CompactBinaryWriter when using v2 that repeated first pass
  when a bonded field was serailized, resulting in extra work and extra
  state left in the CompactBinaryWriter.
* Apply IDL annotations to services and methods for gRPC.
  [Issue \#617](https://github.com/microsoft/bond/issues/617)
* Fixed a bug that produced C# code that couldn't be compiled when using
  Bond-over-gRPC with a generic type instantiated with a collection.
  [Issue \#623](https://github.com/microsoft/bond/issues/623)
* When targeting .NET 4.5, avoid resolving external entities when using
  `SimpleXmlReader`.
* Remove redundant conversions during serialization of aliased blobs.

[msdn-gzipstream]: https://msdn.microsoft.com/en-us/library/system.io.compression.gzipstream(v=vs.110).aspx
[msdn-stream-canseek]: https://msdn.microsoft.com/en-us/library/system.io.stream.canseek(v=vs.110).aspx
[msdn-stream-seek]: https://msdn.microsoft.com/en-us/library/system.io.stream.seek(v=vs.110).aspx

### C# Comm ###

* Apply IDL annotations to services and methods for Comm.

## 6.0.1 ##

This version was allocated but never released.

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
      [InputBuffer](https://github.com/microsoft/bond/commit/11beaf5319639e4bdee96a25f95154e4fed93a75#diff-9260b18a00d12a6102a69b9fffd7e33f),
      [OutputBuffer](https://github.com/microsoft/bond/commit/11beaf5319639e4bdee96a25f95154e4fed93a75#diff-1f15d4c92f87d4bd41f705b20cce80ad),
      and
      [the bf example](https://github.com/microsoft/bond/commit/11beaf5319639e4bdee96a25f95154e4fed93a75#diff-bdda0f39d99280d4858b4453906eea17)
      for more details.
* **Breaking change** The `bond::customize<protocols>` has been removed. All the
  public APIs that require a protocol list (e.g. `bond::Marshal`) now accept
  an extra template argument `Protocols` which defaults to `bond::BuiltInProtocols`.
  Custom input streams now require `bond::type_id<>` to be specialized with a
  unique magic number. For more details please see [the bf example](https://github.com/microsoft/bond/tree/master/examples/cpp/core/bf).
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
  [bond.snk](https://github.com/microsoft/bond/blob/82c97e12621eeb906d1bd46e3abba9da14289c61/cs/build/internal/bond.snk)
  key in the repository instead of with a Microsoft key. This allows anyone
  to produce compatible assemblies, not just Microsoft. Official
  distribution of Bond will continue to be
  [Authenticode signed](https://msdn.microsoft.com/en-us/library/ms537361(v=vs.85).aspx)
  with a Microsoft certificate.
  [Issue \#414](https://github.com/microsoft/bond/issues/414)
    * The new public key for assemblies is now
      `00240000048000009400000006020000002400005253413100040000010001000d504ac18b4b149d2f7b0059b482f9b6d44d39059e6a96ff0a2a52678b5cfd8567cc67254132cd2debb5b95f6a1206a15c6f8ddac137c6c3ef4995f28c359acaa683a90995c8f08df7ce0aaa8836d331a344a514c443f112f80bf2ebed40ccb32d7df63c09b0d7bef80aecdc23ec200a458d4f8bafbcdeb9bf5ba111fbbd4787`
* **Breaking change** Bond assemblies now have assembly and file versions
  that correspond to their NuGet package version. Strong name identities
  will now change release-over-release in line with the NuGet package
  versions. [Issue \#325](https://github.com/microsoft/bond/issues/325)
* The codegen MSBuild targets will now re-run codegen if gbc itself has been
  changed.
* Fixed a bug where JSON and XML protocols would permit the serialization of
  non-nullable string fields that were set to null instead of throwing a
  NullReferenceException.
  [Issue \#417](https://github.com/microsoft/bond/issues/417)

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
  [Pull request \#373](https://github.com/microsoft/bond/pull/373)
    * **Breaking change (Haskell library only):**
      `Language.Bond.Codegen.Cpp.ApplyOverloads.Protocol` is now a union of
      `ProtocolReader` and `ProtocolWriter` to permit mixing and matching of
      reader/writer protocols without having to explicitly compute the full
      cross product.
* Add gbc flags to pick which C# files to generate (structs, gRPC, and
  comm). Only structs are generated by default.
* gbc ensures that method names are unique within a service.
  [Issue \#381](https://github.com/microsoft/bond/issues/381)

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
  [Issue \#305](https://github.com/microsoft/bond/issues/305)
* Improve compliance with strict-aliasing rules.
    * Bond now builds on Clang/GCC with `-fstrict-aliasing`.
* When generating C++ apply files, there are now explicit `bond::Apply<>`
  instantiations for `CompactBinaryWriter<OutputCounter>` and
  `SimpleBinaryWriter<Null>` writers.
  [Pull request \#373](https://github.com/microsoft/bond/pull/373)
* Improve C++ allocator support
  [Issue \#379](https://github.com/microsoft/bond/issues/379)
  [Pull request \#380](https://github.com/microsoft/bond/pull/380)
    * Support C++11 and above allocator model for rebind
    * Simplify detection of the default allocator
* Remove per-field instantiation of DynamicParser<>::UnknownFieldOrTypeMismatch method.

## C# ###

* Added gRPC integration. See the
  [Bond-over-gRPC manual](https://microsoft.github.io/bond/manual/bond_over_grpc.html).
* Added controls to cap incremental allocation between reads in
  `Bond.IO.Unsafe.InputStream`.
* Extended fix for bug parsing JSON when a string value is a date.
  [Pull request \#358](https://github.com/microsoft/bond/pull/358)
* Bond C# 5.1.0 accidentally broke backward compability by renaming
  `Bond.Reflection` to `Bond.BondReflection`. This has been fixed:
  `Bond.BondReflection` was unrenamed back to `Bond.Reflection`, and a shim
  `Bond.BondReflection` type now redirects all calls to their original names
  to minimize further breakage.
  [Issue \#369](https://github.com/microsoft/bond/issues/369)
    * Code that started using `Bond.BondReflection` by name will encounter
      warning CS0618 indicating use of an obselete method/type. To fix this,
      use the original name `Bond.Reflection`. This warning can be
      suppressed if needed. However...
    * ...the shim type `Bond.BondReflection` will be removed during or after
      the next major release of C# Bond.

### C# Comm ###

* **Breaking change** To generate C# Comm files, be sure to pass the
  `--comm` flag to gbc.
  [See how the pingpong example was updated](https://github.com/microsoft/bond/blob/581d88632e0ad8b2fc87f5674273f613e78af752/examples/cs/comm/pingpong/pingpong.csproj#L39).
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
  [Issue \#314](https://github.com/microsoft/bond/issues/314)

### C++ ###

* Fixed compatibility with RapidJSON v1.1.0.
  [Issue \#271](https://github.com/microsoft/bond/issues/271)
* The minimum supported version of Boost is now 1.58
* The `bf` utility now supports multiple payloads.
  [Pull request \#288](https://github.com/microsoft/bond/pull/288)
* Fixed an issue with aliased enums.
  [Pull request \#288](https://github.com/microsoft/bond/pull/298)
* Fixed an issue with template parameter deduction in `bond::is_nullable`
  that occurs with Microsoft Visual C++ 2015 Update 3.
  [Issue \#306](https://github.com/microsoft/bond/issues/306)

### C++ Comm ###
* Fixed a multiply-defined symbol linker error for
  `bond::comm::epoxy::detail::MakeConfigFrame`.

### C# ###

* Added controls to cap pre-allocation during deserialization of containers
  and blobs.
* Fixed computation of default value for aliased bool and wstring fields.
  [Issue \#300](https://github.com/microsoft/bond/issue/300)

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
  [Pull request \#243](https://github.com/microsoft/bond/pull/243)
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
  [Issue \#161 re-opened](https://github.com/microsoft/bond/issues/161)

### IDL comm ###
* Update IDL to conform to naming conventions.
* Adjust IDL for changes made to Epoxy internals

### C++ ###
* **Breaking change:** Runtime SchemaDef `list_sub_type` field removed, as
  it was breaking some consumers of serialized SchemaDef. We plan to restore
  this field in the future.
  [Issue \#161 re-opened](https://github.com/microsoft/bond/issues/161)
* Generated enum types now have a `FromEnum` method that can be used to
  convert from an enum value to a string. Now generated enum types have all
  four of `ToEnum`, `FromEnum`, `ToString`, and `FromString`. (The `...Enum`
  variants return false on failure, while the `...String` variants throw.)

### C# ###
* **Breaking change:** Runtime SchemaDef `list_sub_type` field removed, as
  it was breaking some consumers of serialized SchemaDef. We plan to restore
  this field in the future.
  [Issue \#161 re-opened](https://github.com/microsoft/bond/issues/161)
* The Bond.Runtime NuGet package no longer artificially limits
  Newtonsoft.Json to versions before 10.
  [Issue \#212](https://github.com/microsoft/bond/issues/212)

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
  [Issue \#161](https://github.com/microsoft/bond/issues/161)
* User-defined `TypeMapping`s can now be created. This makes is easier to
  implement code generation for new languages. [Pull request
  \#172](https://github.com/microsoft/bond/pull/172)
* Validate default value type mistmatches.
  [Issue \#72](https://github.com/microsoft/bond/issues/72)
  [Issue \#128](https://github.com/microsoft/bond/issues/128)
* Validate default value out-of-range values.
  [Issue \#73](https://github.com/microsoft/bond/issues/73)
* Fail when struct field has default value of `nothing`.
  [Issue \#164](https://github.com/microsoft/bond/issues/164)
* Fail when enum field doesn't have default value.
  [Issue \#177](https://github.com/microsoft/bond/issues/177)
* Validate default value of type aliases
* Generated types will used `= default` move constructors if possible. This
  results in many generated types having `noexcept` move constructors.
* Fix a bug where, if a Bond namespace contained a struct and an enum value with
  the same name, generated C++ would contain ambiguous references.
  [Issue \#202](https://github.com/microsoft/bond/issues/202)

### IDL core ###

* Set up a separate IDL directory so that IDL is independent of language
  bindings. [Pull request \#219](https://github.com/microsoft/bond/pull/219)

### IDL comm ###

* Set up a separate IDL directory so that IDL is independent of language
  bindings. Convert comm IDL files to use C++-style naming convention. [Pull
  request \#219](https://github.com/microsoft/bond/pull/219)

### C++ ###

* Improvements in the `nullable` implementation. [Pull request \#174](https://github.com/microsoft/bond/pull/174)
    * Correctly use allocator model.
    * Reduce size of `nullable` in the normal case.
    * And others
* Runtime SchemaDef now includes information about whether BT_LIST fields
  are nullable or blobs.
  [Issue \#161](https://github.com/microsoft/bond/issues/161)
* The following Bond types have (possibly conditional) `noexcept` move
  constructors: `blob`, `bonded`, `maybe`, `nullable`, `RuntimeSchema`,
  `value`.

### C# ###

* Bond can now be used with Newtonsoft.Json >= 7.0.1 and < 10
* Runtime SchemaDef now includes information about whether BT_LIST fields
  are nullable or blobs.
  [Issue \#161](https://github.com/microsoft/bond/issues/161)

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
      [TLS example](https://github.com/microsoft/bond/tree/master/examples/cs/comm/tls/)
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
  [Issue \#136](https://github.com/microsoft/bond/issues/136)

### C# ###

* Added
  [`Deserializer.TryDeserialize()`](https://github.com/microsoft/bond/blob/db315adaf4b812adc5ca484b1bcffafe1df6d351/cs/src/core/Deserializer.cs#L231-239).
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
  [Issue \#70](https://github.com/microsoft/bond/issues/70)

## 4.1.0: 2016-04-22

* `gbc` & compiler library: 0.4.0.2
* C# NuGet version: 4.1.0

### `gbc` ###

* Field ordinals larger than 65535 are now rejected.
  [Issue \#111](https://github.com/microsoft/bond/issues/111)
* Fields that duplicate the name of an existing field are now rejected.
  [Issue \#123](https://github.com/microsoft/bond/issues/123)
* The generated C# code now compiles with no errors or warnings at
  `/warn:4`. [Issue \#82](https://github.com/microsoft/bond/issues/82)
* Added
  [Visual Studio Code highlighting rules for `.bond` files](https://github.com/microsoft/bond/tree/b2b9cd7256286fd484444dfaf7645d380a3ee936/tools/syntax/VsCode).

### C++ ###

* Enums are now cast to 32-bit integers to avoid some compiler warnings.
* Bond can be used in code bases where there is a function-style macro named
  `U`.

### C# ###

* The generated C# code now compiles with no errors or warnings at
  `/warn:4`. [Issue \#82](https://github.com/microsoft/bond/issues/82)
* Bond-generated enums constants are now cast to 32-bit integers to avoid
  some compiler warnings.
* [Implicit conversion between `bond.GUID` and `System.Guid`](https://github.com/microsoft/bond/blob/bc4c56a3ca0858f4bd93916e80ceff9bbeada606/cs/test/core/GuidConversionTests.cs#L14)
  has been added.
  [Pull request \#145](https://github.com/microsoft/bond/pull/145)
* The ability to
  [customize the implementation of `IBonded<T>` used](https://microsoft.github.io/bond/manual/bond_cs.html#understanding-bondedt)
  has been added.
  [Pull request \#153](https://github.com/microsoft/bond/pull/153)

## 4.0.2: 2015-12-14

* `gbc` & compiler library: 0.4.0.1
* C# NuGet version: 4.0.2

### Bond compiler library ###

* Added a
  [custom codegen example](https://github.com/microsoft/bond/tree/master/examples/codegen/read-only).

### C# ###

* Added support to
  [not inline nested structs in `Serializers`](https://github.com/microsoft/bond/blob/cb95fdb3e1e10c3e4cae2f2d55e2b116041010a8/cs/src/core/Serializer.cs#L69).
  This can be used to speed up the time to create the serializer for very
  complex schemas.
* Fix for rare buffer corruption in InputStream.
  [Issue \#114](https://github.com/microsoft/bond/issues/114).
* Fix for SimpleXmlParser not handling XML declarations. [Issue \#112](https://github.com/microsoft/bond/issues/82)

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
[repository's history](https://github.com/microsoft/bond/commits/master).
