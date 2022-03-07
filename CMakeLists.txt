cmake_minimum_required (VERSION 3.1)

project (bond)
cmake_policy (SET CMP0022 NEW)

if (POLICY CMP0077)
    # Allow CMake 3.13+ to override options when using FetchContent/add_subdirectory.
    cmake_policy(SET CMP0077 NEW)
endif ()

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/cmake-modules)

set (BOND_FIND_RAPIDJSON
    "FALSE"
    CACHE BOOL "If FALSE, then use and install rapidjson from the /thirdparty subdirectory")

enable_testing()

set (BOND_IDL ${CMAKE_CURRENT_SOURCE_DIR}/idl)
set (BOND_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/cpp/inc)
set (BOND_PYTHON_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/python/inc)
set (BOND_GENERATED ${CMAKE_CURRENT_SOURCE_DIR}/cpp/generated)
set (BOND_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/cpp/src)
set (BOND_COMPAT_TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR}/test/compat)

include (Config)
include (Bond)
include (NoDebug)
include (Compiler)
include (PythonTest)

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} -C ${CMAKE_CFG_INTDIR} --output-on-failure)

if (NOT BOND_GBC_PATH)
    add_subfolder (compiler "compiler")
    if (NOT BOND_SKIP_GBC_TESTS)
        add_dependencies (check gbc-tests)
    endif()
endif()

add_subdirectory (cpp)
add_subfolder (doc "doc")
add_python_subdirectory (python)
add_subdirectory (examples)

if (BOND_ENABLE_JAVA)
    add_subdirectory (java)
endif()


install (DIRECTORY
    cpp/inc/bond
    cpp/generated/bond
    python/inc/bond
    DESTINATION include
    PATTERN *.cpp EXCLUDE)

install (EXPORT bond
    DESTINATION lib/bond
    EXPORT_LINK_INTERFACE_LIBRARIES)

if (NOT BOND_FIND_RAPIDJSON)
    install (DIRECTORY
        thirdparty/rapidjson/include/rapidjson
        DESTINATION include
        PATTERN *.cpp EXCLUDE)
endif()

# if BOND_GBC_PATH is set we must copy over that gbc to the install location
if (BOND_GBC_PATH)
  if (WIN32)
    set(INSTALLED_GBC_NAME gbc.exe)
  else()
    set(INSTALLED_GBC_NAME gbc)
  endif()

  install (
    FILES ${BOND_GBC_PATH}
    DESTINATION bin
    RENAME ${INSTALLED_GBC_NAME})
endif()
