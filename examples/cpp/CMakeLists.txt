if (NOT BOND_SKIP_CORE_TESTS)
    add_subfolder (core "examples/core")
endif()

if (NOT BOND_CORE_ONLY)
    add_subfolder (comm "examples/comm")
endif()

if (BOND_ENABLE_GRPC)
    # Bond-over-gRPC++ SYNC test.
    add_subfolder (grpc-helloworld "examples/grpc-helloworld")
endif()
