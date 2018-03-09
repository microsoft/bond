include (CMakeParseArguments)
include (Folders)

#
# add_bond_codegen (file.bond [file2.bond ...]
#   [ENUM_HEADER]
#   [GRPC]
#   [OUTPUT_DIR dir]
#   [IMPORT_DIR dir [dir2, ...]]
#   [OPTIONS opt [opt2 ...]])
#   [TARGET name]
#
function (add_bond_codegen)
    set (flagArgs ENUM_HEADER GRPC)
    set (oneValueArgs OUTPUT_DIR TARGET)
    set (multiValueArgs IMPORT_DIR OPTIONS)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    set (options)
    set (outputDir ${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR})
    if (arg_OUTPUT_DIR)
        set (outputDir ${arg_OUTPUT_DIR})
    endif()
    list (APPEND options --output-dir="${outputDir}")
    list (APPEND options --import-dir="${BOND_IDL}")
    foreach (dir ${arg_IMPORT_DIR})
        list(APPEND options --import-dir="${dir}")
    endforeach()
    foreach (opt ${arg_OPTIONS})
        list (APPEND options "${opt}")
    endforeach()
    if (arg_ENUM_HEADER)
        list(APPEND options --enum-header)
    endif()
    if (arg_GRPC)
        list(APPEND options --grpc)
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
        if (arg_GRPC)
            list(APPEND outputs "${outputDir}/${name}_grpc.cpp")
            list(APPEND outputs "${outputDir}/${name}_grpc.h")
        endif()
    endforeach()
    # if BOND_GBC_PATH is not set we must add a dependency on the "gbc" target to build it
    if (NOT BOND_GBC_PATH)
        set(gbc "gbc")
    elseif()
        set(gbc "")
    endif()
    add_custom_command(
        OUTPUT ${outputs}
        COMMAND ${GBC_EXECUTABLE} c++ ${options} ${inputs}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        DEPENDS ${inputs} ${gbc} ${GBC_EXECUTABLE})
    if (arg_TARGET)
        add_custom_target (${arg_TARGET}
            DEPENDS ${outputs}
            SOURCES ${inputs})
        add_target_to_folder(${arg_TARGET})
    endif()
endfunction()

#
# add_bond_executable (name
#   [schem.bond [schema2.bond]]
#   source.cpp [source2.cpp]
#   [GRPC])
#
function (add_bond_executable target)
    set (schemas)
    set (sources)
    set (flagArgs GRPC)
    cmake_parse_arguments (arg "${flagArgs}" "" "" ${ARGN})
    foreach (file ${ARGV})
        get_filename_component (ext ${file} EXT)
        if (ext STREQUAL ".bond")
            get_filename_component (name ${file} NAME_WE)
            list (APPEND schemas "${file}")
            list (APPEND sources "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/${name}_types.cpp")
            if (arg_GRPC)
                list (APPEND sources "${CMAKE_CURRENT_BINARY_DIR}/${CMAKE_CFG_INTDIR}/${name}_grpc.cpp")
            endif()
        endif()
    endforeach()
    if (schemas)
        set (options)
        if (arg_GRPC)
            list (APPEND options GRPC)
        endif()
        add_bond_codegen (${schemas} ${options})
    endif()
    list (REMOVE_ITEM ARGV GRPC)
    add_executable (${ARGV} ${sources})
    add_target_to_folder(${target})
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
#   source.cpp [source2.cpp]
#   [BUILD_ONLY]
#   [GRPC])
#
function (add_bond_test test)
    set (flagArgs BUILD_ONLY)
    cmake_parse_arguments (arg "${flagArgs}" "" "" ${ARGN})
    list (REMOVE_ITEM ARGV BUILD_ONLY)
    list (INSERT ARGV 1 EXCLUDE_FROM_ALL)

    add_bond_executable (${ARGV})
    add_dependencies (check ${test})
    if (NOT arg_BUILD_ONLY)
        add_test (
            NAME ${test}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMAND ${test})
    endif()

    get_property (lastTestPort GLOBAL PROPERTY LAST_TEST_PORT)
    if (NOT DEFINED lastTestPort)
        set (lastTestPort 25189)
    endif()
    math(EXPR testPort1 "${lastTestPort} + 1")
    math(EXPR testPort2 "${lastTestPort} + 2")
    target_compile_definitions (${test} PRIVATE
      -DTEST_PORT_1=${testPort1}
      -DTEST_PORT_2=${testPort2})
    set_property (GLOBAL PROPERTY LAST_TEST_PORT "${testPort2}")
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
    add_target_to_folder(${target})
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
