include (Compiler)

set (BOND_GBC_PATH_DESCRIPTION
     "Optional path to the gbc executable to use. If set, this gbc will be used when generating code from .bond files. If not set, then gbc will be built (and the Haskell toolchain will need to be present on the machine) and the gbc tests will be run.")

find_program (BOND_GBC_PATH "gbc"
    HINTS ENV BOND_GBC_PATH
    DOC ${BOND_GBC_PATH_DESCRIPTION}
    # We don't really want to pull gbc from the system path. If someone
    # wants to do that, they'll need to set BOND_GBC_PATH instead.
    NO_SYSTEM_ENVIRONMENT_PATH)

if (BOND_GBC_PATH)
    set (GBC_EXECUTABLE ${BOND_GBC_PATH})
    message (STATUS "Existing GBC executable found: '${GBC_EXECUTABLE}'")
endif()

if (MSVC)
    # disable MSVC warnings
    add_compile_options (/bigobj /FIbond/core/warning.h /W4 /WX)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS -D_SCL_SECURE_NO_WARNINGS)
    set (Boost_USE_STATIC_LIBS ON)
endif (MSVC)

if (WIN32)
    find_package (WindowsSDK)

    # If C# has been built we will also run C# compatibility tests
    find_program (BOND_CSHARP_COMPAT_TEST Bond.CompatibilityTest.exe
        PATH_SUFFIXES net40 net45
        NO_DEFAULT_PATH
        PATHS
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/core/bin/debug"
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/core/bin/retail")

    find_program (BOND_CSHARP_COMM_COMPAT_SERVER CommCompatServer.exe
        PATH_SUFFIXES net40 net45
        NO_DEFAULT_PATH
        PATHS
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/comm/server/bin/debug"
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/comm/server/bin/retail")

    find_program (BOND_CSHARP_COMM_COMPAT_CLIENT CommCompatClient.exe
        PATH_SUFFIXES net40 net45
        NO_DEFAULT_PATH
        PATHS
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/comm/client/bin/debug"
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/comm/client/bin/retail")
endif()

# find python interpreter, library and boost python library.
# to specify a different version, invoke cmake with:
# -DPYTHON_EXECUTABLE=/path/to/python
# -DPYTHON_LIBRARY=/path/to/libpython.so
# -DBoost_PYTHON_LIBRARY_RELEASE=/path/to/libboost-python.so
# (or Boost_PYTHON_LIBRARY_DEBUG if CMAKE_BUILD_TYPE=Debug)
# and optionally with:
# -DPython_ADDITIONAL_VERSIONS=Major.Minor
# if your python version is not implicitly supported by cmake
find_package (PythonInterp 2.7)
if (APPLE)
    if (NOT PYTHON_LIBRARY)
        # find_package (PythonLibs) doesn't find libpython in /usr/local/Cellar,
        # so if PYTHON_LIBRARY wasn't passed as a cmake -D, we have to set it
        # here.
        execute_process (
            COMMAND "python-config" "--prefix"
            OUTPUT_VARIABLE PYTHON_PREFIX
            OUTPUT_STRIP_TRAILING_WHITESPACE)
        string (CONCAT PYTHON_LIBRARY ${PYTHON_PREFIX} "/lib/libpython2.7.dylib")
    endif()
endif()
find_package (PythonLibs 2.7)

find_package (Boost 1.53.0
    OPTIONAL_COMPONENTS
        chrono
        date_time
        thread
        system
        unit_test_framework
        python)

message(STATUS "Boost Python Library: ${Boost_PYTHON_LIBRARY}")

# Make sure AppVeyor CI runs fail when unit test dependencies are not found
if (DEFINED ENV{APPVEYOR} AND ("$ENV{BOND_BUILD}" STREQUAL "C++"))
    if (NOT Boost_UNIT_TEST_FRAMEWORK_FOUND)
        message(FATAL_ERROR "Boost unit_test_framework not found")
    endif()
endif()

# disable Boost auto-linking
add_definitions (-DBOOST_ALL_NO_LIB)

# VS2015U2 fixed a bug with atomics and emits a warning without this definition.
add_definitions (-D_ENABLE_ATOMIC_ALIGNMENT_FIX)

cxx_add_compile_options(Clang
    -fPIC
    --std=c++11
    -Wall
    -Werror
    -Wno-unknown-warning-option
    -Wno-unused-local-typedefs)

cxx_add_compile_options(AppleClang
    -fPIC
    --std=c++11
    -Wall
    -Werror
    -Wno-unknown-warning-option
    -Wno-unused-local-typedefs)

cxx_add_compile_options(GNU
    -fPIC
    --std=c++11
    -Wall
    -Werror
    -Wno-unknown-warning-option
    -Wno-unused-local-typedefs)

include_directories (
    ${BOND_INCLUDE}
    ${BOND_GENERATED}
    ${Boost_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/rapidjson/include)

set (BOND_LIBRARIES_ONLY
    "FALSE"
    CACHE BOOL "If TRUE, then only build the Bond library files, skipping any tools. gbc will still be built if it cannot be found, however, as gbc is needed to build the libraries.")

set (BOND_CORE_ONLY
    "FALSE"
    CACHE BOOL "If TRUE, then only build the Bond Core")

set (BOND_SKIP_CORE_TESTS
    "FALSE"
    CACHE BOOL "If TRUE, then skip Bond Core tests and examples")

if ((NOT BOND_CORE_ONLY) AND ((CXX_STANDARD LESS 11) OR (MSVC_VERSION LESS 1800)))
    message(FATAL_ERROR "BOND_CORE_ONLY is FALSE but compiler specified does not support C++11 standard")
endif()
