find_program (GRADLE_EXECUTABLE gradle)

if (GRADLE_EXECUTABLE)
    execute_process (
        COMMAND ${GRADLE_EXECUTABLE} --version
        OUTPUT_VARIABLE gradle_version)
    message (STATUS "gradle found at ${GRADLE_EXECUTABLE}")
    message (STATUS "${gradle_version}")
else()
    message (FATAL_ERROR "gradle was not found. Java code cannot be built.")
endif()
