if (NOT BOND_SKIP_CORE_TESTS)
    add_subfolder (core "tests/python/unit_test/core")
    add_subfolder (compat "tests/python/compat")
endif()
