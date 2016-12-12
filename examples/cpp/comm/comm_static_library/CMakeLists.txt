add_bond_codegen (lib/comm_static_library.bond
    OPTIONS --apply=compact
    COMM)

add_library (comm_static_library_lib EXCLUDE_FROM_ALL
    STATIC
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_static_library_types.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_static_library_comm.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_static_library_apply.cpp)

add_target_to_folder (comm_static_library_lib)

target_link_libraries (comm_static_library_lib PUBLIC
    bond)

add_bond_test (comm_static_library_server
    server/comm_static_library_server.cpp)

target_link_libraries (comm_static_library_server PRIVATE
    comm_static_library_lib)

add_bond_test (comm_static_library_client
    client/comm_static_library_client.cpp)

target_link_libraries (comm_static_library_client PRIVATE
    comm_static_library_lib)
