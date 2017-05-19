# To add a new unit test suite, call add_unit_test at the bottom of this file on
# all of the files you want built into your test suite. The name of the suite
# will be derived from the name of the file that is given first.
function (add_unit_test)
    get_filename_component(name ${ARGV0} NAME_WE)
    set (files ${ARGV})
    add_bond_test (${name} EXCLUDE_FROM_ALL ${files} COMM)
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
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}
    # TODO: It feels like the grpc++ target should have this as part of its
    # interface... Perhaps a patch for upstream?
    ${CMAKE_CURRENT_SOURCE_DIR}/../../../thirdparty/grpc/include)
target_link_libraries (grpc_test_common PUBLIC
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
    grpc++)

add_unit_test (thread_pool.cpp)
add_unit_test (io_manager.cpp)
