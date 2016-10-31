add_subfolder (core "examples/core")

if (NOT BOND_CORE_ONLY)
    add_subfolder (comm "examples/comm")
endif()
