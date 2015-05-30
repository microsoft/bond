cmake_minimum_required (VERSION 2.8.12)

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

include (CMakeParseArguments)

# required Haskell components
find_package (Cabal 1.18.0.0 REQUIRED)
find_package (GHC 7.4.1.0 REQUIRED)

function (add_cabal_build target)
    set (flagArgs ENABLE_TESTS)
    set (multiValueArgs SOURCES DEPENDS)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    if (${arg_ENABLE_TESTS})
        set (options --enable-tests)
    endif()
    add_custom_command (
        DEPENDS ${arg_SOURCES}
        COMMAND ${CMAKE_COMMAND} 
            ${CMAKE_BINARY_DIR}
            -N
            -Dbuild_dir=${CMAKE_CURRENT_BINARY_DIR} 
            -Dtarget=${target}
            -Dcabal_options=${options}
            -P cabal_build.cmake
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/build/${target}/${target}${CMAKE_EXECUTABLE_SUFFIX})
    add_custom_target (${target}
        SOURCES ${arg_SOURCES}
        DEPENDS ${arg_DEPENDS} 
                ${CMAKE_CURRENT_BINARY_DIR}/build/${target}/${target}${CMAKE_EXECUTABLE_SUFFIX})
    add_dependencies (${target} sandbox)        
endfunction()    

function (add_cabal_test target)
    list (INSERT ${target} 0 EXCLUDE_FROM_ALL) 
    add_cabal_build (${ARGV} ENABLE_TESTS)
    add_test (
        NAME ${target} 
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMAND ${CMAKE_CURRENT_BINARY_DIR}/build/${target}/${target})
endfunction()

set (sources
    gbc.cabal
    Main.hs
    Files.hs
    Options.hs
    Bond/Lexer.hs
    Bond/Parser.hs
    Bond/Schema/JSON.hs
    Bond/Schema/Types.hs
    Bond/Schema/Util.hs
    Bond/Util.hs
    Bond/Version.hs
    Bond/Template/CustomMapping.hs
    Bond/Template/TypeMapping.hs
    Bond/Template/Util.hs
    Bond/Template/Cpp/Apply_cpp.hs
    Bond/Template/Cpp/Apply_h.hs
    Bond/Template/Cpp/Enum_h.hs
    Bond/Template/Cpp/Reflection_h.hs
    Bond/Template/Cpp/Types_cpp.hs
    Bond/Template/Cpp/Types_h.hs
    Bond/Template/Cpp/Util.hs
    Bond/Template/Cs/Types_cs.hs
    Bond/Template/Cs/Util.hs)

set (test_sources
    tests/Main.hs
    tests/Tests/Schema.hs)

set (completion_dir /etc/bash_completion.d)
set (completion ${CMAKE_CURRENT_BINARY_DIR}/gbc.comp)
set (output ${CMAKE_CURRENT_BINARY_DIR}/build/gbc/gbc${CMAKE_EXECUTABLE_SUFFIX})
set (GBC_EXECUTABLE ${output} PARENT_SCOPE)

add_custom_command (
    COMMAND ${CMAKE_COMMAND} 
        ${CMAKE_BINARY_DIR} 
        -N
        -P sandbox_init.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/cabal.sandbox.config)

add_custom_target (sandbox
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/cabal.sandbox.config)

add_custom_command (
    DEPENDS ${output}
    COMMAND ${output} --help=bash > ${completion}
    OUTPUT ${completion})

add_cabal_build (gbc
    DEPENDS sandbox ${completion}
    SOURCES ${sources})

add_cabal_test (gbc-tests
    DEPENDS gbc
    SOURCES ${test_sources})

install (FILES ${output}
    PERMISSIONS OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE
    DESTINATION bin)

if(EXISTS "${completion_dir}" AND IS_DIRECTORY "${completion_dir}")
    install (FILES ${completion}
        RENAME gbc 
        DESTINATION ${completion_dir})
endif()
