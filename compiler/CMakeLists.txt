cmake_minimum_required (VERSION 2.8.12)

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

# required Haskell components
find_package (Cabal 1.18.0.0 REQUIRED)
find_package (GHC 7.4.1.0 REQUIRED)

set (sources
    gbc.cabal
    Main.hs
    Options.hs
    Bond/Lexer.hs
    Bond/Parser.hs
    Bond/Schema.hs
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

set (output_dir ${CMAKE_CURRENT_BINARY_DIR})
set (output ${output_dir}/build/gbc/gbc${CMAKE_EXECUTABLE_SUFFIX})
set (completion_dir /etc/bash_completion.d)
set (completion ${output_dir}/gbc.comp)
set (GBC_EXECUTABLE ${output} PARENT_SCOPE)

add_custom_command (
    COMMAND ${CMAKE_COMMAND} 
        ${CMAKE_BINARY_DIR} 
        -N
        -P sandbox_init.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/cabal.sandbox.config)

add_custom_command (
    DEPENDS ${sources} ${CMAKE_CURRENT_SOURCE_DIR}/cabal.sandbox.config
    COMMAND ${CMAKE_COMMAND} 
        ${CMAKE_BINARY_DIR}
        -N
        -Doutput_dir=${output_dir} 
        -P cabal_build.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT ${output})

add_custom_command (
    DEPENDS ${output}
    COMMAND ${output} --help=bash > ${completion}
    OUTPUT ${completion})

add_custom_target (compiler
    SOURCES ${sources}
    DEPENDS ${output} ${completion})

install (FILES ${output}
    PERMISSIONS OWNER_EXECUTE GROUP_EXECUTE WORLD_EXECUTE
    DESTINATION bin)

if(EXISTS "${completion_dir}" AND IS_DIRECTORY "${completion_dir}")
    install (FILES ${completion}
        RENAME gbc 
        DESTINATION ${completion_dir})
endif()
