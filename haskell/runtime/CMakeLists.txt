cmake_minimum_required (VERSION 2.8.12)

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/../../cmake)

# required Haskell components
find_package (Cabal 1.18.0.0 REQUIRED)
find_package (GHC 7.4.1.0 REQUIRED)

set (sources
    bond.cabal
    Bond/API.hs
    Bond/BinaryProto.hs
    Bond/Bonded.hs
    Bond/Cast.hs
    Bond/CompactBinary.hs
    Bond/CompactBinary.hs-boot
    Bond/Default.hs
    Bond/FastBinary.hs
    Bond/FastBinary.hs-boot
    Bond/Imports.hs
    Bond/Schema.hs
    Bond/SimpleBinary.hs
    Bond/Stream.hs
    Bond/Types.hs
    Bond/Wire.hs
    tests/CompatTest.hs)

set (output_dir ${CMAKE_CURRENT_BINARY_DIR})
set (output ${output_dir}/build/build-done)
set (testoutput ${output_dir}/build/test-done)

add_custom_command (
    COMMAND ${CMAKE_COMMAND} 
        ${CMAKE_BINARY_DIR} 
        -N
        -P sandbox_init.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/cabal.sandbox.config)

add_custom_command (
    DEPENDS compiler ${GCB_EXECUTABLE} ${sources} ${CMAKE_CURRENT_SOURCE_DIR}/cabal.sandbox.config
    COMMAND ${CMAKE_COMMAND} 
        ${CMAKE_BINARY_DIR}
        -N
        -Doutput_dir=${output_dir} 
        -Dgbc=${GBC_EXECUTABLE} 
        -P cabal_build.cmake
    COMMAND ${CMAKE_COMMAND} -E touch ${output}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT ${output})

add_custom_command (
    DEPENDS ${sources} ${output} ${CMAKE_CURRENT_SOURCE_DIR}/cabal.sandbox.config
    COMMAND ${CMAKE_COMMAND} 
        ${CMAKE_BINARY_DIR}
        -N
        -Doutput_dir=${output_dir} 
        -P cabal_test.cmake
    COMMAND ${CMAKE_COMMAND} -E touch ${testoutput}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT ${testoutput})

add_custom_target (haskell ALL
    SOURCES ${sources}
    DEPENDS ${output} ${testoutput})
