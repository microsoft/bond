include (Compiler)

if (MSVC)
    # disable MSVC warnings
    add_compile_options (/bigobj /FIbond/core/warning.h)
    add_definitions (-D_CRT_SECURE_NO_WARNINGS)
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

find_package (PythonLibs 2.7)
find_package (PythonInterp 2.7)

find_package (Boost 1.53.0
    OPTIONAL_COMPONENTS
        date_time
        thread
        system
        unit_test_framework
        python)

# disable Boost auto-linking
add_definitions (-DBOOST_ALL_NO_LIB)

cxx_add_compile_options(Clang -fPIC)
cxx_add_compile_options(GNU -fPIC)

include_directories (
    ${BOND_INCLUDE}
    ${BOND_GENERATED}
    ${Boost_INCLUDE_DIRS}
    ${CMAKE_CURRENT_SOURCE_DIR}/thirdparty/rapidjson/include)

