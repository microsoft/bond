FIND_PROGRAM(STACK_EXECUTABLE stack)

SET(STACK_FOUND FALSE)

IF (STACK_EXECUTABLE)
    SET(STACK_FOUND TRUE)

    execute_process (
        COMMAND ${STACK_EXECUTABLE} --version
        OUTPUT_VARIABLE stack_version)

    string (STRIP ${stack_version} stack_version)

    MESSAGE(STATUS "Stack found at ${STACK_EXECUTABLE}. ${stack_version}")
ELSE()
    MESSAGE(FATAL_ERROR "Stack was not found.")
ENDIF()
