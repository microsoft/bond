add_bond_codegen (lib/grpc_static_library.bond
    OPTIONS --apply=compact
    GRPC)

add_library (grpc_static_library_lib EXCLUDE_FROM_ALL
    STATIC
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/grpc_static_library_types.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/grpc_static_library_apply.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/grpc_static_library_grpc.cpp)

add_target_to_folder (grpc_static_library_lib)

if (BOND_FIND_GRPC)
    find_package(grpc CONFIG REQUIRED)
    target_link_libraries (grpc_static_library_lib PUBLIC
        gRPC::grpc++)
else ()
    target_link_libraries (grpc_static_library_lib PUBLIC
        grpc++)
endif ()

target_link_libraries (grpc_static_library_lib PUBLIC
    bond)

add_bond_test (grpc_static_library_server
    server/grpc_static_library_server.cpp)

target_link_libraries (grpc_static_library_server PRIVATE
    grpc_static_library_lib)

add_bond_test (grpc_static_library_client
    client/grpc_static_library_client.cpp)

target_link_libraries (grpc_static_library_client PRIVATE
    grpc_static_library_lib)

cxx_target_compile_definitions (MSVC grpc_static_library_lib PRIVATE
    -D_WIN32_WINNT=0x0600)

cxx_target_compile_definitions (MSVC grpc_static_library_server PRIVATE
    -D_WIN32_WINNT=0x0600)

cxx_target_compile_definitions (MSVC grpc_static_library_client PRIVATE
    -D_WIN32_WINNT=0x0600)

# Supress warning about [[deprecated]] Bond-over-gRPC code.
cxx_target_no_warn_deprecated (grpc_static_library_lib)
cxx_target_no_warn_deprecated (grpc_static_library_server)
cxx_target_no_warn_deprecated (grpc_static_library_client)
