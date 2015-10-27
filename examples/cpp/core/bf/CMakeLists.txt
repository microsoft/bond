add_bond_executable (bf cmd_arg.bond main.cpp)

add_dependencies (check bf)

install (TARGETS bf DESTINATION bin)

target_use_cxx11 (bf)

add_test (
    NAME bf
    COMMAND
        ${CMAKE_COMMAND}
            -DBF=$<TARGET_FILE:bf>
            -DBOND_COMPAT=$<TARGET_FILE:compatibility_test>
            -DCOMPAT_DATA=${BOND_COMPAT_TEST_DIR}/data
            -P ${CMAKE_CURRENT_SOURCE_DIR}/bf_test.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_test (
    NAME bf_schema
    COMMAND
        bf ${BOND_COMPAT_TEST_DIR}/data/compat.schema.dat
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
