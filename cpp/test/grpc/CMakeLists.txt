# To add a new unit test suite, call add_unit_test at the bottom of this file on
# all of the files you want built into your test suite. The name of the suite
# will be derived from the name of the file that is given first.
function (add_unit_test)
    get_filename_component(name ${ARGV0} NAME_WE)
    set (files ${ARGV})
    add_bond_test (${name} EXCLUDE_FROM_ALL ${files} GRPC)
    add_target_to_folder (${name})
    add_dependencies (${name}
        bond)
    target_include_directories (${name} PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
    target_compile_definitions (${name} PUBLIC
        -DBOND_COMPACT_BINARY_PROTOCOL
        -DBOND_SIMPLE_BINARY_PROTOCOL
        -DBOND_FAST_BINARY_PROTOCOL)
    target_link_libraries (${name} PRIVATE
        bond
        grpc_test_common
        grpc_test_services
        ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
        ${Boost_CHRONO_LIBRARY})

    cxx_target_compile_options (MSVC ${name} PRIVATE -D_WIN32_WINNT=0x0600)

    cxx_target_compile_options (Clang ${name} PRIVATE -DBOOST_ASIO_HAS_STD_CHRONO)
    cxx_target_compile_options (AppleClang ${name} PRIVATE -DBOOST_ASIO_HAS_STD_CHRONO)
endfunction()


# Build common code into its own library.
add_library (grpc_test_common EXCLUDE_FROM_ALL "main.cpp")
add_target_to_folder (grpc_test_common)
target_include_directories (grpc_test_common PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
target_link_libraries (grpc_test_common PUBLIC
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
    grpc++)

# Generate C++ from .bond files
add_bond_codegen(TARGET grpc_test_services_codegen
  name_conflicts.bond
  services.bond
  GRPC)

# Build codegen into its own library.
add_library (grpc_test_services
    EXCLUDE_FROM_ALL
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/name_conflicts_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/name_conflicts_grpc.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/services_types.cpp"
    "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/services_grpc.cpp")
add_target_to_folder (grpc_test_services)
target_include_directories (grpc_test_services PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
cxx_target_compile_options (MSVC grpc_test_services PRIVATE -D_WIN32_WINNT=0x0600)
target_link_libraries(grpc_test_services PRIVATE bond grpc++)
add_dependencies(grpc_test_services grpc_test_services_codegen)

add_unit_test (io_manager.cpp)

add_unit_test (service_attributes.cpp)

add_unit_test (thread_pool.cpp)

add_unit_test (unary_call.cpp)

add_unit_test (wait_callback.cpp)
target_link_libraries(wait_callback PRIVATE bond_apply)

add_unit_test (server.cpp)
