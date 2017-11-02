find_program (GRADLE_EXECUTABLE gradle)
find_program (MAVEN_EXECUTABLE mvn)

if (GRADLE_EXECUTABLE)
    execute_process (
        COMMAND ${GRADLE_EXECUTABLE} --version
        OUTPUT_VARIABLE gradle_version)
    message (STATUS "gradle found at ${GRADLE_EXECUTABLE}")
    message (STATUS "${gradle_version}")
else()
    message (FATAL_ERROR "gradle was not found. Java will be disabled.")
endif()

if (MAVEN_EXECUTABLE)
    execute_process (
        COMMAND ${MAVEN_EXECUTABLE} --version
        OUTPUT_VARIABLE maven_version)
    message (STATUS "maven found at ${MAVEN_EXECUTABLE}")
    message (STATUS "${maven_version}")
else()
    message (FATAL_ERROR "maven was not found. Java will be disabled.")
endif()
