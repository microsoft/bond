cmake_minimum_required (VERSION 2.8.12)

project (BondDoc)

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/../cmake)

include (CMakeParseArguments)
include (HaskellUtil)
include (Folders)

# build the compiler when doc directory is build by itself
if (CMAKE_SOURCE_DIR STREQUAL CMAKE_CURRENT_SOURCE_DIR)
    add_subdirectory (../compiler ${CMAKE_CURRENT_BINARY_DIR}/compiler)
endif()

#
# add_pandoc_markdown (file.md [file2.md ...]
#   [CODE code]
#   [OUTPUT_DIR dir]
#   [OUTPUT_NAME name]
#   [TEMPLATE template]
#   [OPTIONS opt [opt2 ...]])
#
function (add_pandoc_markdown)
    set (flagArgs)
    set (oneValueArgs OUTPUT_NAME OUTPUT_DIR CODE TEMPLATE)
    set (multiValueArgs OPTIONS)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    set (options
        --standalone
        --smart
        --data-dir=.pandoc
        --highlight-style tango
        ${arg_OPTIONS})
    if (arg_TEMPLATE)
        list(APPEND options --template="${arg_TEMPLATE}")
    else()
    endif()
    if (arg_CODE)
        list(APPEND options --indented-code-classes="${arg_CODE}")
    else()
    endif()
    set (inputs "${arg_UNPARSED_ARGUMENTS}")
    foreach (file ${inputs})
        get_filename_component (name ${file} NAME_WE)
        if (arg_OUTPUT_NAME)
            set (outputName ${arg_OUTPUT_NAME})
        else()
            set (outputName ${name})
        endif()
        set (output "${CMAKE_CURRENT_BINARY_DIR}/html/${arg_OUTPUT_DIR}/${outputName}.html")
        list (APPEND sources ${file})
        list (APPEND outputs ${output})
        add_custom_command(
            OUTPUT ${output}
            COMMAND ${Haskell_PANDOC_EXECUTABLE} ${options} --output="${output}" "${file}"
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            DEPENDS ${file} ${PANDOC_EXECUTABLE})
    endforeach()
    add_custom_target (${name}
        DEPENDS ${outputs}
        SOURCES ${sources})
    add_target_to_folder (${name})
    add_dependencies (documentation ${name})
endfunction()

find_haskell_program (pandoc)
find_program (Doxygen_EXECUTABLE doxygen)

if (Haskell_PANDOC_EXECUTABLE AND Doxygen_EXECUTABLE)
    add_custom_command(
        OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html"
        COMMAND ${CMAKE_COMMAND} -E copy .pandoc/templates/cmdargs.html "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html"
        COMMAND ${GBC_EXECUTABLE} --help=all,html >> "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS .pandoc/templates/cmdargs.html gbc ${GBC_EXECUTABLE})

    add_custom_target (documentation
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html")

    add_target_to_folder (documentation)

    add_pandoc_markdown (src/bond_py.md src/bond_cpp.md
        CODE "numberLines"
        OPTIONS --self-contained --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown (src/compiler.md
        OPTIONS --self-contained --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown(src/bond_over_grpc.md
        OPTIONS --self-contained --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown (src/bond_cs.md
        CODE "numberLines"
        OPTIONS --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown (src/bond_java.md
        CODE "numberLines"
        OPTIONS --self-contained --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown (src/reference_index.md
        OPTIONS --self-contained "--title-prefix=Bond Reference Index"
        OUTPUT_NAME index
        OUTPUT_DIR reference)

    add_pandoc_markdown (../README.md
        OUTPUT_NAME index
        OPTIONS --table-of-contents "--title-prefix=Bond, Microsoft's cross-platform framework for working with schematized data")

    add_pandoc_markdown (src/why_bond.md
        OPTIONS --self-contained --table-of-contents)

    file(GLOB_RECURSE cpp_headers ../cpp/inc/bond/*.h)

    set (doxygen_sources
        doxygen/bond.doxygen
        doxygen/bond_layout.xml
        doxygen/bond_reference.css
        doxygen/mainpage.md
        doxygen/examples/wait_callback_example.cpp
        ${cpp_headers})

    set (doxygen_output_dir
        "${CMAKE_CURRENT_BINARY_DIR}/html/reference")

    add_custom_command(
        OUTPUT "${doxygen_output_dir}/cpp/index.html"
        COMMAND ${CMAKE_COMMAND}
            -DDoxygen_EXECUTABLE=${Doxygen_EXECUTABLE}
            -DDoxygen_OUTPUT_DIR=${doxygen_output_dir}
            -P doxygen/doxygen.cmake
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${doxygen_sources})

    add_custom_target (cpp_reference
        DEPENDS "${doxygen_output_dir}/cpp/index.html"
        SOURCES ${doxygen_sources})

    add_target_to_folder (cpp_reference)

    add_dependencies (documentation cpp_reference)
endif()
