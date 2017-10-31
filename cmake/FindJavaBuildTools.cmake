find_program (GRADLE_EXECUTABLE gradle)
find_program (MAVEN_EXECUTABLE mvn)

set (GRADLE_FOUND FALSE)
if (GRADLE_EXECUTABLE)
    set (GRADLE_FOUND TRUE)
    execute_process (
        COMMAND ${GRADLE_EXECUTABLE} --version
        OUTPUT_VARIABLE gradle_version)
    message (STATUS "gradle found at ${GRADLE_EXECUTABLE}")
    message (STATUS "${gradle_version}")
else()
    message (STATUS "gradle was not found. Java will be disabled.")
endif()

set (MAVEN_FOUND FALSE)
if (MAVEN_EXECUTABLE)
    set (MAVEN_FOUND TRUE)
    execute_process (
        COMMAND ${MAVEN_EXECUTABLE} --version
        OUTPUT_VARIABLE maven_version)
    message (STATUS "maven found at ${MAVEN_EXECUTABLE}")
    message (STATUS "${maven_version}")
else()
    message (STATUS "maven was not found. Java will be disabled.")
endif()
