cmake_minimum_required (VERSION 2.8.12)
project (bond)

cmake_policy (SET CMP0022 NEW)

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/cmake-modules)

enable_testing()

set (BOND_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/cpp/inc)
set (BOND_PYTHON_INCLUDE ${CMAKE_CURRENT_SOURCE_DIR}/python/inc)
set (BOND_GENERATED ${CMAKE_CURRENT_SOURCE_DIR}/cpp/generated)
set (BOND_COMPAT_TEST_DIR ${CMAKE_CURRENT_SOURCE_DIR}/test/compat)

include (Config)
include (Bond)
include (NoDebug)
include (Compiler)
include (PythonTest)

add_custom_target(check COMMAND ${CMAKE_CTEST_COMMAND} -C ${CMAKE_CFG_INTDIR} --output-on-failure)

add_subfolder (compiler "compiler")
add_subdirectory (cpp)
add_subfolder (doc "doc")
add_python_subdirectory (python)
add_subdirectory (examples)

add_dependencies (check gbc-tests)

install (DIRECTORY
    cpp/inc/bond
    cpp/generated/bond
    python/inc/bond
    DESTINATION include
    PATTERN *.cpp EXCLUDE)

install (EXPORT bond
    DESTINATION lib/bond
    EXPORT_LINK_INTERFACE_LIBRARIES)

