add_bond_python_module (python_compatibility_test
    ${BOND_COMPAT_TEST_DIR}/core/schemas/compat.bond
    ${BOND_COMPAT_TEST_DIR}/core/schemas/compat2.bond
    ${BOND_COMPAT_TEST_DIR}/core/schemas/compat_common.bond
    compat.cpp
    compat.py)

target_compile_definitions (python_compatibility_test PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL)

# disable generation of debug symbols to speed up build
no_pdb()

function (add_python_compat_test test)
    add_test (
        NAME python_compatibility_${test}
        COMMAND
            ${CMAKE_COMMAND}
                -DPYTHON_EXECUTABLE=${PYTHON_EXECUTABLE}
                -DPYTHON_COMPAT=$<TARGET_FILE_DIR:python_compatibility_test>
                -DBOND_COMPAT=$<TARGET_FILE:compatibility_test>
                -DCOMPAT_DATA=${BOND_COMPAT_TEST_DIR}/core/data
                -DTEST=${test}
                -DSOURCE_DIR=${CMAKE_CURRENT_SOURCE_DIR}
                -P ${CMAKE_CURRENT_SOURCE_DIR}/compat.cmake
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
endfunction()

add_python_compat_test (compact)
add_python_compat_test (schema)

