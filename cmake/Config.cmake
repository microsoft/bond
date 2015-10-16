include (Compiler)

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
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/bin/debug"
            "${CMAKE_CURRENT_SOURCE_DIR}/cs/test/compat/bin/retail")
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
find_package (PythonLibs 2.7)

find_package (Boost 1.53.0
    OPTIONAL_COMPONENTS
        date_time
        thread
        system
        unit_test_framework
        python)

message(STATUS "Boost Python Library: ${Boost_PYTHON_LIBRARY}")

# disable Boost auto-linking
add_definitions (-DBOOST_ALL_NO_LIB)

cxx_add_compile_options(Clang
    -fPIC
    -Wall
    -Werror
    -Wno-unknown-warning-option
    -Wno-unused-local-typedefs)

cxx_add_compile_options(GNU
    -fPIC
    -Wall
    -Werror
    -Wno-unknown-warning-option
    -Wno-unused-local-typedefs)

include_directories (
    ${BOND_INCLUDE}
    ${BOND_GENERATED}
    ${Boost_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/rapidjson/include)

