cmake_minimum_required (VERSION 2.8.12)

set (CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/../../cmake)

include (Java)

add_gradle_build (gradle-plugin GRADLE_TARGET install)
add_maven_install (gradle-plugin-install
    AFTER_BUILD gradle-plugin
    JAR bond-gradle-1.0.jar)
