# To add a new unit test suite, call add_unit_test at the bottom of this file on
# all of the files you want built into your test suite. The name of the suite
# will be derived from the name of the file that is given first.
function (add_unit_test)
    get_filename_component(name ${ARGV0} NAME_WE)
    set (files ${ARGV})
    add_bond_test (${name} EXCLUDE_FROM_ALL ${files} COMM)
    add_target_to_folder (${name})
    add_dependencies (${name}
        bond
        comm_test_codegen)
    target_include_directories (${name} PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
    target_compile_definitions (${name} PUBLIC
        -DBOND_COMPACT_BINARY_PROTOCOL
        -DBOND_SIMPLE_BINARY_PROTOCOL
        -DBOND_FAST_BINARY_PROTOCOL)
    target_link_libraries (${name} PRIVATE
        bond
        comm_test_common
        ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY}
        ${Boost_CHRONO_LIBRARY})

    cxx_target_compile_options (MSVC ${name} PRIVATE -D_WIN32_WINNT=0x0600)

    cxx_target_compile_options (Clang ${name} PRIVATE -DBOOST_ASIO_HAS_STD_CHRONO)
    cxx_target_compile_options (AppleClang ${name} PRIVATE -DBOOST_ASIO_HAS_STD_CHRONO)
endfunction()


# Build common code into its own library.
add_library (comm_test_common EXCLUDE_FROM_ALL "main.cpp" "idl.cpp")
add_target_to_folder (comm_test_common)
add_dependencies(comm_test_common comm_test_codegen)
target_include_directories (comm_test_common PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
target_link_libraries (comm_test_common PRIVATE
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

add_bond_codegen (TARGET comm_test_codegen comm_test_common.bond COMM)

add_unit_test (unit_test_comm.cpp
    async.cpp
    basic.cpp
    core.cpp
    error.cpp
    event.bond
    event.cpp
    forward.cpp
    generic.bond
    generic_type.cpp
    generic_service.cpp
    jumbo.cpp
    layers.cpp
    protocol.cpp
    session.bond
    session.cpp
    timeout.cpp
    unexpected.cpp
    utils.bond
    utils.cpp
    void.bond
    void.cpp)
