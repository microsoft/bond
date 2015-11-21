cxx_add_compile_options(Clang -Wno-unused-value)

add_bond_executable (compatibility_test EXCLUDE_FROM_ALL
    ${BOND_COMPAT_TEST_DIR}/schemas/compat.bond
    ${BOND_COMPAT_TEST_DIR}/schemas/compat2.bond
    ${BOND_COMPAT_TEST_DIR}/schemas/compat_no_generics.bond
    ${BOND_COMPAT_TEST_DIR}/schemas/compat_common.bond
    cmd_arg.bond
    compat.cpp
    serialization.cpp)

add_dependencies (check compatibility_test)

target_compile_definitions (compatibility_test PRIVATE
    -DBOND_COMPACT_BINARY_PROTOCOL
    -DBOND_SIMPLE_BINARY_PROTOCOL
    -DBOND_FAST_BINARY_PROTOCOL
    -DBOND_SIMPLE_JSON_PROTOCOL)

target_use_cxx11 (compatibility_test)

# disable generation of debug symbols to speed up build
no_pdb()

function (add_interop_test interop test)
    add_test (
        NAME ${interop}_compatibility_${test}
        COMMAND
            ${CMAKE_COMMAND}
                -DCSHARP_COMPAT=${BOND_CSHARP_COMPAT_TEST}
                -DBOND_COMPAT=$<TARGET_FILE:compatibility_test>
                -DGBC=${GBC_EXECUTABLE}
                -DCOMPAT_DATA=${BOND_COMPAT_TEST_DIR}/data
                -DTEST=${test}
                -P ${CMAKE_CURRENT_SOURCE_DIR}/compat.cmake
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})
endfunction()

function (add_compat_test test)
    add_test (
        NAME compatibility_${test}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND compatibility_test ${test} -d ${BOND_COMPAT_TEST_DIR}/data/compat.${test}.dat expected.cpp.${test} deserialized.cpp.${test})

    if (BOND_CSHARP_COMPAT_TEST)
        add_interop_test (cs ${test})
    endif()
endfunction()

add_compat_test (compact)
add_compat_test (compact2)
add_compat_test (fast)
add_compat_test (json)
add_compat_test (simple)
add_compat_test (simple2)
add_compat_test (schema)
add_interop_test (gbc schema)
