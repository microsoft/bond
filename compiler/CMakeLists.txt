cmake_minimum_required (VERSION 2.8.12)

if (POLICY CMP0074)
    cmake_policy(SET CMP0074 NEW)
endif()

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

include (CMakeParseArguments)
include (Folders)

find_package (Stack)

function (add_stack_build target)
    set (flagArgs ENABLE_TESTS)
    set (multiValueArgs SOURCES DEPENDS)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    if (${arg_ENABLE_TESTS})
        set (options "--test")
    endif()

    # If unspecified, turn off optimizations
    list (APPEND options $<$<CONFIG:>:--fast>)

    list (APPEND options $<$<CONFIG:Debug>:--fast>)
    list (APPEND options $<$<CONFIG:Debug>:--no-executable-stripping>)
    list (APPEND options $<$<CONFIG:Debug>:--no-library-stripping>)
    list (APPEND options $<$<CONFIG:Debug>:--no-strip>)

    list (APPEND options $<$<CONFIG:MinSizeRel>:--executable-stripping>)
    list (APPEND options $<$<CONFIG:MinSizeRel>:--ghc-options=-O2>)
    list (APPEND options $<$<CONFIG:MinSizeRel>:--library-stripping>)

    list (APPEND options $<$<CONFIG:Release>:--executable-stripping>)
    list (APPEND options $<$<CONFIG:Release>:--ghc-options=-O2>)
    list (APPEND options $<$<CONFIG:Release>:--library-stripping>)

    list (APPEND options $<$<CONFIG:RelWithDebInfo>:--ghc-options=-O2>)
    list (APPEND options $<$<CONFIG:RelWithDebInfo>:--no-executable-stripping>)
    list (APPEND options $<$<CONFIG:RelWithDebInfo>:--no-library-stripping>)
    list (APPEND options $<$<CONFIG:RelWithDebInfo>:--no-strip>)

    add_custom_command (
        DEPENDS ${arg_SOURCES}
        COMMAND ${CMAKE_COMMAND}
            ${CMAKE_BINARY_DIR}
            -N
            -Dbuild_dir=${CMAKE_CURRENT_BINARY_DIR}
            -Dtarget=${target}
            "\"-Dstack_options=${options}\""
            -P stack_build.cmake
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/build/${target}/${target}${CMAKE_EXECUTABLE_SUFFIX})
    add_custom_target (${target}
        SOURCES ${arg_SOURCES}
        DEPENDS ${arg_DEPENDS}
                ${CMAKE_CURRENT_BINARY_DIR}/build/${target}/${target}${CMAKE_EXECUTABLE_SUFFIX})
    add_target_to_folder(${target})
endfunction()

function (add_stack_test target)
    list (INSERT ${target} 0 EXCLUDE_FROM_ALL)
    add_stack_build (${ARGV} ENABLE_TESTS)
    add_test (
        NAME ${target}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND ${CMAKE_CURRENT_BINARY_DIR}/build/${target}/${target})
endfunction()

file (GLOB language_bond "src/Language/Bond/*.hs")
source_group ("Language\\Bond" FILES ${language_bond})

file (GLOB language_bond_syntax "src/Language/Bond/Syntax/*.hs")
source_group ("Language\\Bond\\Syntax" FILES ${language_bond_syntax})

file (GLOB language_bond_codegen "src/Language/Bond/Codegen/*.hs")
source_group ("Language\\Bond\\Codegen" FILES ${language_bond_codegen})

file (GLOB language_bond_codegen_cpp "src/Language/Bond/Codegen/Cpp/*.hs")
source_group ("Language\\Bond\\Codegen\\Cpp" FILES ${language_bond_codegen_cpp})

file (GLOB language_bond_codegen_cs "src/Language/Bond/Codegen/Cs/*.hs")
source_group ("Language\\Bond\\Codegen\\Cs" FILES ${language_bond_codegen_cs})

file (GLOB language_bond_codegen_java "src/Language/Bond/Codegen/Java/*.hs")
source_group ("Language\\Bond\\Codegen\\Java" FILES ${language_bond_codegen_java})

set (sources
    bond.cabal
    stack.yaml
    Main.hs
    IO.hs
    Options.hs
    ${language_bond}
    ${language_bond_syntax}
    ${language_bond_codegen}
    ${language_bond_codegen_cpp}
    ${language_bond_codegen_cs}
    ${language_bond_codegen_java})

file (GLOB tests "tests/Tests/*.hs")
source_group ("Tests" FILES ${tests})

set (test_sources
    tests/TestMain.hs
    ${tests})

set (completion_dir etc/bash_completion.d)
set (completion ${CMAKE_CURRENT_BINARY_DIR}/gbc.comp)
set (output ${CMAKE_CURRENT_BINARY_DIR}/build/gbc/gbc${CMAKE_EXECUTABLE_SUFFIX})
set (GBC_EXECUTABLE ${output} PARENT_SCOPE)

add_custom_command (
    DEPENDS ${output}
    COMMAND ${output} --help=bash > ${completion}
    OUTPUT ${completion})

add_stack_build (gbc
    DEPENDS ${completion}
    SOURCES ${sources})

if(NOT BOND_SKIP_GBC_TESTS)
    add_stack_test (gbc-tests
        DEPENDS gbc
        SOURCES ${test_sources})
endif()

install (FILES ${output}
    PERMISSIONS OWNER_READ OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
    DESTINATION bin)

install (FILES ${completion}
         RENAME gbc
         DESTINATION ${completion_dir})
