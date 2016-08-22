cmake_minimum_required (VERSION 2.8.12)
project (bond)

cmake_policy (SET CMP0022 NEW)

set(BOND_GBC_PATH
    CACHE
    FILEPATH
    "Optional path to the gbc executable to use. If set, this gbc will be used when generating code from .bond files. If not set, then gbc will be built (and the Haskell toolchain will need to be present on the machine) and the gbc tests will be run.")

if (BOND_GBC_PATH)
    if(EXISTS ${BOND_GBC_PATH})
        set (GBC_EXECUTABLE ${BOND_GBC_PATH})
        MESSAGE (STATUS "GBC Executable found: " ${GBC_EXECUTABLE})
    else()
        MESSAGE (FATAL_ERROR "GBC Executable defined by BOND_GBC_PATH does not exists at " ${BOND_GBC_PATH})
    endif()
endif()

set (CMAKE_MODULE_PATH
    ${CMAKE_CURRENT_SOURCE_DIR}/cmake
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/cmake-modules)

enable_testing()

set (BOND_IDL ${CMAKE_CURRENT_SOURCE_DIR}/idl)
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

if (NOT BOND_GBC_PATH)
    add_subfolder (compiler "compiler")
endif()

add_subdirectory (cpp)
add_subfolder (doc "doc")
add_python_subdirectory (python)
add_subdirectory (examples)

if (NOT BOND_GBC_PATH)
    add_dependencies (check gbc-tests)
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

# if BOND_GBC_PATH is set we must copy over the gbc.exe to the install location
if (BOND_GBC_PATH)
    install (FILES
        ${BOND_GBC_PATH}
        DESTINATION bin)
endif()
