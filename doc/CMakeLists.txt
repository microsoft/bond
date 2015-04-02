include (CMakeParseArguments)
include (HaskellUtil)

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
    add_custom_target ("documentation_${name}"
        DEPENDS ${outputs}
        SOURCES ${sources})
    add_dependencies (documentation "documentation_${name}")
endfunction()    

find_haskell_program (pandoc)

if (Haskell_PANDOC_EXECUTABLE)
    add_custom_command(
        OUTPUT  "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html"
        COMMAND ${CMAKE_COMMAND} -E copy .pandoc/templates/cmdargs.html "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html"
        COMMAND ${GBC_EXECUTABLE} --help=html >> "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html"
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS .pandoc/templates/cmdargs.html ${GBC_EXECUTABLE})

    add_custom_target (documentation
        DEPENDS "${CMAKE_CURRENT_BINARY_DIR}/html/manual/gbc.html")

    add_pandoc_markdown (src/bond_py.md src/bond_cpp.md
        CODE "cpp,numberLines"
        OPTIONS --self-contained --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown (src/compiler.md
        CODE cpp
        OPTIONS --self-contained --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown (src/bond_cs.md
        CODE "cs,numberLines"
        OPTIONS --self-contained --table-of-contents
        OUTPUT_DIR manual)

    add_pandoc_markdown (src/reference_index.md
        CODE cpp
        OPTIONS --self-contained
        OUTPUT_NAME index
        OUTPUT_DIR reference)

    add_pandoc_markdown (../README.md
        TEMPLATE index.html
        OUTPUT_NAME index
        CODE python)

    add_pandoc_markdown (src/why_bond.md
        TEMPLATE index.html
        CODE python)
endif()

