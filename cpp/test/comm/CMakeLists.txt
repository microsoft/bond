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
add_bond_codegen (
    TARGET comm_test_codegen
    COMM
    comm_test_common.bond
    generic.bond)

add_library (comm_test_common EXCLUDE_FROM_ALL
    STATIC
    "main.cpp"
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_test_common_apply.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_test_common_comm.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/comm_test_common_types.cpp
    # There's nothing in generic_apply.cpp to build, so we omit it.
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/generic_comm.cpp
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/generic_types.cpp)
add_target_to_folder (comm_test_common)
add_dependencies(comm_test_common comm_test_codegen)
target_include_directories (comm_test_common PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
target_compile_definitions (comm_test_common PUBLIC
        -DBOND_COMPACT_BINARY_PROTOCOL
        -DBOND_SIMPLE_BINARY_PROTOCOL
        -DBOND_FAST_BINARY_PROTOCOL)
target_link_libraries (comm_test_common
    PUBLIC
    bond
    PRIVATE
    ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})

add_unit_test (async.cpp)
add_unit_test (basic.cpp)
add_unit_test (core.cpp)
add_unit_test (error.cpp)
add_unit_test (event.cpp event.bond)
add_unit_test (forward.cpp)
add_unit_test (generic_service.cpp)
add_unit_test (generic_type.cpp)
add_unit_test (jumbo.cpp)
add_unit_test (layers.cpp)
add_unit_test (protocol.cpp)
add_unit_test (session.cpp session.bond)
add_unit_test (timeout.cpp)
add_unit_test (utils.cpp utils.bond)
add_unit_test (void.cpp void.bond)
