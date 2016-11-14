if (BOND_LIBRARIES_ONLY)
  set (bf_EXCLUDE "EXCLUDE_FROM_ALL")
else ()
  set (bf_EXCLUDE "")
endif ()

add_bond_executable (bf ${bf_EXCLUDE} cmd_arg.bond main.cpp)

if (NOT BOND_LIBRARIES_ONLY)
  # bf was excluded from the all target, so it cannot be installed
  install (TARGETS bf DESTINATION bin)
endif()

add_dependencies (check bf)

add_test (
    NAME bf
    COMMAND
        ${CMAKE_COMMAND}
            -DBF=$<TARGET_FILE:bf>
            -DBOND_COMPAT=$<TARGET_FILE:compatibility_test>
            -DCOMPAT_DATA=${BOND_COMPAT_TEST_DIR}/core/data
            -P ${CMAKE_CURRENT_SOURCE_DIR}/bf_test.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

add_test (
    NAME bf_schema
    COMMAND
        bf ${BOND_COMPAT_TEST_DIR}/core/data/compat.schema.dat
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
