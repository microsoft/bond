# Core unit tests don't build under VS2013 without this.
cxx_add_compile_options(MSVC /EHa)

if (NOT BOND_SKIP_COMPAT_TESTS)
    add_subfolder (compat/core "tests/compat/core")
endif()

if (Boost_UNIT_TEST_FRAMEWORK_FOUND)
    if (NOT BOND_SKIP_CORE_TESTS)
        add_subfolder (core "tests/unit_test/core")
    endif()
endif()
