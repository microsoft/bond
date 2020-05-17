include (Compiler)

if (BOND_ENABLE_GRPC AND NOT BOND_SKIP_GRPC_LIB_COMPILE)
    cxx_add_compile_options (Clang -Wno-unused-value)
    add_subdirectory(grpc)
endif()
