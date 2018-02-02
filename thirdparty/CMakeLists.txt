include (Compiler)

if (BOND_ENABLE_GRPC)
    cxx_add_compile_options (Clang -Wno-unused-value)
    add_subdirectory(grpc)
endif()

if (NOT BOND_SKIP_CORE_TESTS)
    set(protobuf_BUILD_TESTS OFF)

    if (MSVC)
        set(protobuf_MSVC_STATIC_RUNTIME FALSE)
    endif()

    add_subdirectory(protobuf/cmake)
endif()
