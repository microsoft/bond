if (NOT BOND_SKIP_CORE_TESTS)
    add_subfolder (core "examples/core")
endif()

if (NOT BOND_CORE_ONLY)
    add_subfolder (comm "examples/comm")
endif()
