include (CMakeParseArguments)

#
# add_bond_codegen (file.bond [file2.bond ...] 
#   [ENUM_HEADER]
#   [OUTPUT_DIR dir] 
#   [IMPORT_DIR dir [dir2, ...]]
#   [OPTIONS opt [opt2 ...]])
#   [TARGET name]
#
function (add_bond_codegen)
    set (flagArgs ENUM_HEADER)
    set (oneValueArgs OUTPUT_DIR TARGET)
    set (multiValueArgs IMPORT_DIR OPTIONS)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN}) 
    set (options)
    set (outputDir ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
    if (arg_OUTPUT_DIR)
        set (outputDir ${arg_OUTPUT_DIR})
    else()
    endif()
    list (APPEND options --output-dir="${outputDir}")
    list (APPEND options --import-dir="${BOND_INCLUDE}")
    foreach (dir ${arg_IMPORT_DIR})
        list(APPEND options --import-dir="${dir}")
    endforeach() 
    foreach (opt ${arg_OPTIONS})
        list (APPEND options "${opt}")
    endforeach()
    if (arg_ENUM_HEADER)
        list(APPEND options --enum-header)
    endif()
    set (inputs "${arg_UNPARSED_ARGUMENTS}")
    set (outputs)
    foreach (file ${inputs})
        get_filename_component (name ${file} NAME_WE)
        list (APPEND outputs 
            "${outputDir}/${name}_reflection.h"
            "${outputDir}/${name}_types.h"
            "${outputDir}/${name}_types.cpp"
            "${outputDir}/${name}_apply.h"
            "${outputDir}/${name}_apply.cpp"
        )
        if (arg_ENUM_HEADER)
            list(APPEND outputs "${outputDir}/${name}_enum.h")
        endif()
    endforeach()
    add_custom_command(
        OUTPUT ${outputs}
        COMMAND ${GBC_EXECUTABLE} c++ ${options} ${inputs}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${inputs} gbc ${GBC_EXECUTABLE})
    if (arg_TARGET)
        add_custom_target (${arg_TARGET}
            DEPENDS ${outputs}
            SOURCES ${inputs})
    endif()
endfunction()

#
# add_bond_executable (name
#   [schem.bond [schema2.bond]]
#   source.cpp [source2.cpp])
#
function (add_bond_executable target)
    set (schemas)
    set (sources)
    foreach (file ${ARGV})
        get_filename_component (ext ${file} EXT)
        if (ext STREQUAL ".bond")
            get_filename_component (name ${file} NAME_WE)
            list (APPEND schemas "${file}")
            list (APPEND sources "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/${name}_types.cpp")
        endif()
    endforeach()
    if (schemas)
        add_bond_codegen (${schemas})
    endif()
    add_executable (${ARGV} ${sources})
    target_link_libraries (${target} PRIVATE
        bond 
        bond_apply)
    target_include_directories (${target} PRIVATE 
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR} 
        ${CMAKE_CURRENT_SOURCE_DIR})
endfunction()

#
# add_bond_test (name
#   [schem.bond [schema2.bond]]
#   source.cpp [source2.cpp])
#
function (add_bond_test test)
    list (INSERT ARGV 1 EXCLUDE_FROM_ALL)
    add_bond_executable (${ARGV})
    add_dependencies (check ${test})
    add_test (
        NAME ${test} 
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMAND ${test})
endfunction()

#
# add_bond_python_module (name
#   [schem.bond [schema2.bond]]
#   source.cpp [source2.cpp])
#
function (add_bond_python_module target)
    set (schemas)
    set (sources)
    foreach (file ${ARGV})
        get_filename_component (ext ${file} EXT)
        if (ext STREQUAL ".bond")
            get_filename_component (name ${file} NAME_WE)
            list (APPEND schemas "${file}")
            list (APPEND sources "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/${name}_types.cpp")
        endif()
    endforeach()
    if (schemas)
        add_bond_codegen (${schemas})
    endif()
    list (INSERT ARGV 1 EXCLUDE_FROM_ALL)
    python_add_module (${ARGV} ${sources})
    add_dependencies (check ${target})
    target_link_libraries (${target} PRIVATE 
        bond
        bond_apply 
        ${PYTHON_LIBRARIES}
        ${Boost_PYTHON_LIBRARY})
    target_include_directories (${target} PRIVATE
        ${BOND_PYTHON_INCLUDE}
        ${WINDOWSSDK_PREFERRED_DIR}/Include
        ${WINDOWSSDK_PREFERRED_DIR}/Include/shared
        ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR} 
        ${CMAKE_CURRENT_SOURCE_DIR}
        ${PYTHON_INCLUDE_DIR})
    target_compile_definitions (${target} PRIVATE
        -DBOOST_PYTHON_STATIC_LIB)
endfunction()

