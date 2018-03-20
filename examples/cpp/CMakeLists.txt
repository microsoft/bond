if (NOT BOND_SKIP_CORE_TESTS)
    add_subfolder (core "examples/core")
endif()

if (BOND_ENABLE_GRPC)
    add_subfolder (grpc "examples/grpc")
endif()
