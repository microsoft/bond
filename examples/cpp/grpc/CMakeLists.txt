if (MSVC)
    add_subdirectory (grpc_dll)
endif()
add_subdirectory (grpc_static_library)
add_subdirectory (helloworld)
add_subdirectory (pingpong)
add_subdirectory (scalar)

