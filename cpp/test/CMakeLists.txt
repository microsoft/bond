# warning C4310: cast truncates constant value
cxx_add_compile_options(MSVC /wd4310)

# Core unit tests don't build under VS2013 without this.
cxx_add_compile_options(MSVC /EHa)

add_subfolder (compat/core "tests/compat/core")

if (Boost_UNIT_TEST_FRAMEWORK_FOUND)
    if (NOT BOND_SKIP_CORE_TESTS)
        add_subfolder (core "tests/unit_test/core")
    endif()
    if (BOND_ENABLE_COMM)
        add_subfolder (comm "tests/unit_test/comm")
        add_subfolder (comm/epoxy "tests/unit_test/comm/epoxy")
        add_subfolder (compat/comm "tests/compat/comm")
    endif()
    if (BOND_ENABLE_GRPC)
        add_subfolder (compat/grpc "tests/compat/grpc")
        add_subfolder (grpc "tests/unit_test/grpc")
    endif()
endif()
