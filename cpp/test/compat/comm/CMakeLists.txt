cxx_add_compile_options(Clang -Wno-unused-value)

add_bond_executable (comm_compatibility_test COMM EXCLUDE_FROM_ALL
    commcmd_arg.bond
    commcompat.cpp)

add_bond_executable (cpp_comm_compat_server COMM EXCLUDE_FROM_ALL
    ${BOND_COMPAT_TEST_DIR}/comm/pingpong.bond
    pingpong_server.cpp)

add_bond_executable (cpp_comm_compat_client COMM EXCLUDE_FROM_ALL
    ${BOND_COMPAT_TEST_DIR}/comm/pingpong.bond
    pingpong_client.cpp)

add_dependencies (comm_compatibility_test cpp_comm_compat_server)
add_dependencies (comm_compatibility_test cpp_comm_compat_client)
add_dependencies (check comm_compatibility_test)

target_compile_definitions (cpp_comm_compat_server PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL
    -DBOND_FAST_BINARY_PROTOCOL)
target_compile_definitions (cpp_comm_compat_client PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL
    -DBOND_FAST_BINARY_PROTOCOL)
target_compile_definitions (comm_compatibility_test PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL
    -DBOND_FAST_BINARY_PROTOCOL)

target_link_libraries (cpp_comm_compat_server PRIVATE
    ${Boost_CHRONO_LIBRARY})
target_link_libraries (cpp_comm_compat_client PRIVATE
    ${Boost_CHRONO_LIBRARY})

# disable generation of debug symbols to speed up build
no_pdb()

add_test (
        NAME comm_compatibility_cpp_cpp
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND comm_compatibility_test
                  -s $<TARGET_FILE:cpp_comm_compat_server>
                  -c $<TARGET_FILE:cpp_comm_compat_client>)

if (BOND_CSHARP_COMM_COMPAT_SERVER)
    if (BOND_CSHARP_COMM_COMPAT_CLIENT)
        add_test (
            NAME comm_compatibility_cs_cs
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND comm_compatibility_test
                      -s ${BOND_CSHARP_COMM_COMPAT_SERVER}
                      -c ${BOND_CSHARP_COMM_COMPAT_CLIENT})
    endif()

    add_test (
        NAME comm_compatibility_cs_cpp
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND comm_compatibility_test
                  -s ${BOND_CSHARP_COMM_COMPAT_SERVER}
                  -c $<TARGET_FILE:cpp_comm_compat_client>)
endif()

if (BOND_CSHARP_COMM_COMPAT_CLIENT)
    add_test (
        NAME comm_compatibility_cpp_cs
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND comm_compatibility_test
                  -s $<TARGET_FILE:cpp_comm_compat_server>
                  -c ${BOND_CSHARP_COMM_COMPAT_CLIENT})
endif()
