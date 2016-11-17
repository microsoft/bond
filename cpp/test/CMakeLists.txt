# warning C4310: cast truncates constant value
cxx_add_compile_options(MSVC /wd4310)

# Core unit tests don't build under VS2013 without this.
cxx_add_compile_options(MSVC /EHa)

if (NOT BOND_SKIP_CORE_TESTS)
    add_subfolder (compat/core "tests/compat/core")
endif()

if (Boost_UNIT_TEST_FRAMEWORK_FOUND)
    if (NOT BOND_SKIP_CORE_TESTS)
        add_subfolder (core "tests/unit_test/core")
    endif()
    if (NOT BOND_CORE_ONLY)
        add_subfolder (comm "tests/unit_test/comm")
        add_subfolder (comm/epoxy "tests/unit_test/comm/epoxy")
    endif()
endif()

if (NOT BOND_CORE_ONLY)
    # Only run C++/C# compat tests for MSVC until we qualify C# Comm on Linux/MacOS
    if (MSVC)
        add_subfolder (compat/comm "tests/compat/comm")
    endif()
endif()
