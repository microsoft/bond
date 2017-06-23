# This is a workaround for Windows quirks.
# CMake Visual Studio generator translates add_custom_command into a batch file
# embedded in Visual Studio project. Batch files have problems with paths that
# contain non-ascii characters because they are limited to DOS encoding. It so
# happens that stack is quite likely to be installed in such a path because by
# default stack install uses directory under %APPDATA% which contains user name.
# As a workaround we execute this .cmake script as a custom command and use CMake
# cache to get access to variables set during configuration.

# Get proper behavior for if condition with error variable
cmake_policy (SET CMP0012 NEW)

execute_process (
    COMMAND ${STACK_EXECUTABLE} setup
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    RESULT_VARIABLE error
    OUTPUT_QUIET
    ERROR_QUIET)

if (error)
    message (FATAL_ERROR)
endif()

set (buildGhcOptions "-O2")

execute_process (
    COMMAND ${STACK_EXECUTABLE} build :${target} --no-run-tests ${stack_options} --ghc-options=${buildGhcOptions}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    RESULT_VARIABLE error)

if (error)
    message (FATAL_ERROR)
endif()

# Copy results to builddir
execute_process (
    COMMAND ${STACK_EXECUTABLE} path --dist-dir
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    OUTPUT_VARIABLE dist_dir
    RESULT_VARIABLE error)

if (error)
    message (FATAL_ERROR)
endif()

string (STRIP ${dist_dir} dist_dir)

file (COPY "${dist_dir}/build" DESTINATION "${build_dir}")
