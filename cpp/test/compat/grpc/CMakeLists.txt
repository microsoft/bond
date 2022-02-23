cxx_add_compile_options(Clang -Wno-unused-value)

# driver executable
add_bond_executable (grpc_compatibility_test EXCLUDE_FROM_ALL
    grpccmd_arg.bond
    grpccompat.cpp)

# shared generated code
add_bond_codegen(TARGET cpp_grpc_compat_codegen
    GRPC
    ${BOND_COMPAT_TEST_DIR}/grpc/pingpong.bond)
add_library(cpp_grpc_compat_codegen_lib EXCLUDE_FROM_ALL
    STATIC
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/pingpong_apply.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/pingpong_types.cpp)
add_target_to_folder(cpp_grpc_compat_codegen_lib)
target_link_libraries(cpp_grpc_compat_codegen_lib PUBLIC bond)
add_dependencies(cpp_grpc_compat_codegen_lib cpp_grpc_compat_codegen)
target_compile_definitions (cpp_grpc_compat_codegen_lib PUBLIC
    -DBOND_COMPACT_BINARY_PROTOCOL
    -DBOND_FAST_BINARY_PROTOCOL)
# Supress warning about [[deprecated]] Bond-over-gRPC code.
cxx_target_no_warn_deprecated (cpp_grpc_compat_codegen_lib)

# server and client executables
add_bond_executable (cpp_grpc_compat_server GRPC EXCLUDE_FROM_ALL
    pingpong_server.cpp)

add_bond_executable (cpp_grpc_compat_client GRPC EXCLUDE_FROM_ALL
    pingpong_client.cpp)

add_dependencies (grpc_compatibility_test cpp_grpc_compat_server)
add_dependencies (grpc_compatibility_test cpp_grpc_compat_client)
add_dependencies (check grpc_compatibility_test)

cxx_target_compile_definitions (MSVC cpp_grpc_compat_server PRIVATE
    -D_WIN32_WINNT=0x0600)

cxx_target_compile_definitions (MSVC cpp_grpc_compat_client PRIVATE
    -D_WIN32_WINNT=0x0600)

# Supress warning about [[deprecated]] Bond-over-gRPC code.
cxx_target_no_warn_deprecated (cpp_grpc_compat_server)
cxx_target_no_warn_deprecated (cpp_grpc_compat_client)

target_compile_definitions (grpc_compatibility_test PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL
    -DBOND_FAST_BINARY_PROTOCOL)

target_link_libraries (cpp_grpc_compat_server PRIVATE
    cpp_grpc_compat_codegen_lib
    grpc++)
target_link_libraries (cpp_grpc_compat_client PRIVATE
    cpp_grpc_compat_codegen_lib
    grpc++
    ${Boost_CHRONO_LIBRARY})

# disable generation of debug symbols to speed up build
no_pdb()

add_test (
        NAME grpc_compatibility_cpp_cpp
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND grpc_compatibility_test
                  -s $<TARGET_FILE:cpp_grpc_compat_server>
                  -c $<TARGET_FILE:cpp_grpc_compat_client>)

if (BOND_CSHARP_GRPC_COMPAT_SERVER)
    if (BOND_CSHARP_GRPC_COMPAT_CLIENT)
        add_test (
            NAME grpc_compatibility_cs_cs
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND grpc_compatibility_test
                      -s ${BOND_CSHARP_GRPC_COMPAT_SERVER}
                      -c ${BOND_CSHARP_GRPC_COMPAT_CLIENT})
    endif()

    add_test (
        NAME grpc_compatibility_cs_cpp
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND grpc_compatibility_test
                  -s ${BOND_CSHARP_GRPC_COMPAT_SERVER}
                  -c $<TARGET_FILE:cpp_grpc_compat_client>)
endif()

if (BOND_CSHARP_GRPC_COMPAT_CLIENT)
    add_test (
        NAME grpc_compatibility_cpp_cs
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND grpc_compatibility_test
                  -s $<TARGET_FILE:cpp_grpc_compat_server>
                  -c ${BOND_CSHARP_GRPC_COMPAT_CLIENT})
endif()
