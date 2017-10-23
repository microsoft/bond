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

function (add_maven_install target)
    set (oneValueArgs AFTER_BUILD JAR)
    cmake_parse_arguments (arg "${flagArgs}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})

    # The path given to -DpomFile is hard-coded to match the output path of
    # gradle's maven plugin.
    add_custom_command (
        COMMAND
        ${MAVEN_EXECUTABLE}
        install:install-file
        -DpomFile=build/poms/pom-default.xml
        -Dfile=build/libs/${arg_JAR}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/build/java/${target})

    add_custom_target ("${target}"
        DEPENDS ${arg_AFTER_BUILD}
        ${CMAKE_CURRENT_BINARY_DIR}/build/java/${target})

    add_target_to_folder("${target}")
    add_dependencies(java "${target}")
endfunction()
