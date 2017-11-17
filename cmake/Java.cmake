cmake_minimum_required (VERSION 2.8.12)

include (CMakeParseArguments)
include (Folders)

find_package (JavaBuildTools)

# This function intentionally doesn't accept or configure SOURCES. cmake should
# call gradle unconditionally, and gradle will take care of up-to-dateness.
function (add_gradle_build target)
    set (oneValueArgs GRADLE_TARGET)
    set (multiValueArgs DEPENDS)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    if (NOT arg_GRADLE_TARGET)
        set (arg_GRADLE_TARGET build)
    endif()

    add_custom_command (
        DEPENDS gbc
        COMMAND
        ${GRADLE_EXECUTABLE} --console plain ${arg_GRADLE_TARGET}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/build/java/${target})

    add_custom_target ("${target}"
        DEPENDS gbc ${arg_DEPENDS}
        ${CMAKE_CURRENT_BINARY_DIR}/build/java/${target})

    add_target_to_folder("${target}")
    add_dependencies(java "${target}")
endfunction()
