if (NOT BOND_SKIP_CORE_TESTS)
    add_subfolder (core "examples/core")
endif()

if (NOT BOND_CORE_ONLY)
    add_subfolder (comm "examples/comm")
endif()

if (BOND_ENABLE_GRPC)
    # This is a dummy project to make sure that gRPC++ builds and can be
    # linked correctly. This will be removed later, when we have a
    # Bond-over-gRPC++ test.
    add_subfolder (grpc-dummy "examples/grpc-dummy")
endif()
