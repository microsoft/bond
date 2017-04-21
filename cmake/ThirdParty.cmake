set (BOND_ENABLE_GRPC
    "FALSE"
    CACHE BOOL "If TRUE, then Bond C++ gRPC support will be built, using the grpc submodule. The grpc pre-requesites will need to have been installed.")

if (BOND_ENABLE_GRPC)
    add_subdirectory(thirdparty/grpc)
endif()
